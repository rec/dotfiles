#!/usr/bin/env -S uv run --with tyro --with pydantic python
from __future__ import annotations

import os
import platform
import re
import shutil
import subprocess
import sys
from collections.abc import Callable
from importlib import import_module
from pathlib import Path
from typing import Annotated, Literal, cast

import tyro
from pydantic import BaseModel

PACKAGING_READMES = (
    "README.md",
    "packaging/README-WINDOWS.txt",
    "packaging/README-MACOS.txt",
    "packaging/README-LINUX.txt",
)


class ReleaseCommand(BaseModel, frozen=True):
    part: Annotated[Literal["patch", "minor", "major"], tyro.conf.Positional] = "patch"
    publish: bool = False


def main() -> int:
    command = tyro.cli(ReleaseCommand)
    part = command.part
    os.environ.pop("VIRTUAL_ENV", None)

    if git_output("status", "--porcelain"):
        sys.exit("release requires a clean working tree")

    branch = git_output("branch", "--show-current")
    if branch != "main":
        sys.exit(f"release must run from main, not {branch}")

    pyproject = read_pyproject()
    project = project_table(pyproject)
    project_name = project_name_from(project)
    python_version = python_version_from(project)
    app_name = project_name[:1].upper() + project_name[1:]
    dependencies = {dependency_name(d) for d in project_dependencies(project)}

    dynamic_version = "version" in cast(list[str], project.get("dynamic", []))
    if dynamic_version:
        build_system = pyproject.get("build-system")
        if (
            not isinstance(build_system, dict)
            or cast(dict[str, object], build_system).get("build-backend") != "maturin"
        ):
            sys.exit("dynamic version releases require the maturin build backend")
        target_version = bump_cargo_version(part, dry_run=True)
    else:
        target_version = command_output(
            "uv", "version", "--bump", part, "--dry-run", "--short"
        )
    tag = f"v{target_version}"

    if command_fails("git", "rev-parse", "--verify", "--quiet", tag):
        sys.exit(f"tag already exists: {tag}")
    if command_fails(
        "git", "ls-remote", "--exit-code", "--tags", "origin", f"refs/tags/{tag}"
    ):
        sys.exit(f"remote tag already exists: {tag}")

    ruff_paths, find_paths = checked_paths(project_name)

    run("uv", "run", "pytest")
    run_linter(
        "uv", "run", "ruff", "check", "--fix", "--select", "B,E,F,I", *ruff_paths
    )
    run_linter("uv", "run", "ruff", "format", "--check", *ruff_paths)
    run_linter("uv", "run", "ty", "check", ruff_paths[0])
    run_pyupgrade(find_paths, python_version)
    run("git", "diff", "--check")

    if git_output("status", "--porcelain"):
        sys.exit("release verification changed the working tree")

    build_project(project_name, app_name, dependencies)

    if dynamic_version:
        version = bump_cargo_version(part)
        run("cargo", "update", "--workspace", "--offline")
        version_paths = ["Cargo.toml", "Cargo.lock"]
    else:
        run("uv", "version", "--bump", part, "--no-sync")
        version = command_output("uv", "version", "--short")
        version_paths = ["pyproject.toml", "uv.lock"]
    tag = f"v{version}"
    if version != target_version:
        sys.exit(
            f"version changed after dry run: expected {target_version}, got {version}"
        )

    run("git", "add", *version_paths)
    run("git", "commit", "-m", f"Release {tag}")
    run("git", "push")
    run("git", "tag", tag)
    run("git", "push", "origin", tag)
    run("gh", "release", "create", tag, "--verify-tag", "--generate-notes")
    clean_dist()
    if command.publish:
        run("uv", "build")
        run("uv", "publish")
    return 0


def read_pyproject() -> dict[str, object]:
    try:
        toml_module = import_module("tomllib")
    except ModuleNotFoundError:
        toml_module = import_module("tomli")
    loads = cast(Callable[[str], dict[str, object]], toml_module.loads)
    return loads(Path("pyproject.toml").read_text())


def bump_cargo_version(part: str, dry_run: bool = False) -> str:
    path = Path("Cargo.toml")
    text = path.read_text()
    package = re.search(r"(?ms)^\[package\][ \t]*\n(.*?)(?=^\[|\Z)", text)
    if package is None:
        raise SystemExit("Cargo.toml does not contain a [package] table")
    version = re.search(r'(?m)^version\s*=\s*"(\d+)\.(\d+)\.(\d+)"', package.group(1))
    if version is None:
        raise SystemExit(
            "Cargo.toml package.version must be a literal major.minor.patch version"
        )
    numbers = [int(n) for n in version.groups()]
    index = ["major", "minor", "patch"].index(part)
    numbers[index] += 1
    numbers[index + 1 :] = [0] * (2 - index)
    result = ".".join(str(n) for n in numbers)
    if not dry_run:
        start = package.start(1) + version.start(1)
        end = package.start(1) + version.end(3)
        path.write_text(text[:start] + result + text[end:])
    return result


def project_table(pyproject: dict[str, object]) -> dict[str, object]:
    project = pyproject.get("project")
    if not isinstance(project, dict):
        raise SystemExit("pyproject.toml does not contain a [project] table")
    return cast(dict[str, object], project)


def project_name_from(project: dict[str, object]) -> str:
    name = project.get("name")
    if not isinstance(name, str):
        raise SystemExit("pyproject.toml does not contain project.name")
    return name


def project_dependencies(project: dict[str, object]) -> list[str]:
    dependencies = project.get("dependencies", [])
    if not isinstance(dependencies, list):
        raise SystemExit("pyproject.toml project.dependencies must be a list")
    return [d for d in cast(list[object], dependencies) if isinstance(d, str)]


def python_version_from(project: dict[str, object]) -> str:
    requires_python = project.get("requires-python")
    if not isinstance(requires_python, str):
        raise SystemExit("pyproject.toml does not contain project.requires-python")
    if (match := re.search(r"3\.(\d+)", requires_python)) is None:
        raise SystemExit("project.requires-python must contain a Python 3 version")
    return f"3{match.group(1)}"


def dependency_name(dependency: str) -> str:
    for s in ";[=<>~!":
        dependency = dependency.split(s, 1)[0]
    return dependency.strip().lower()


def checked_paths(project_name: str) -> tuple[list[str], list[str]]:
    source = Path(project_name)
    if not source.is_dir():
        sys.exit(f"release requires a top-level Python package directory: {source}")
    ruff_paths = [str(source)]
    find_paths = [str(source)]

    if Path("install").is_dir():
        ruff_paths.append("install")
        find_paths.append("install")

    for p in sorted(Path().glob("test*")):
        ruff_paths.append(str(p))

    if Path("test").is_dir():
        find_paths.append("test")

    return ruff_paths, find_paths


def run_pyupgrade(find_paths: list[str], python_version: str) -> None:
    files = [str(p) for root in find_paths for p in Path(root).rglob("*.py")]
    if files:
        run_linter("uv", "run", "pyupgrade", f"--py{python_version}-plus", *files)


def clean_dist() -> None:
    dist = Path("dist")
    if not dist.is_dir():
        return
    for path in dist.iterdir():
        if path.is_dir() and not path.is_symlink():
            shutil.rmtree(path)
        else:
            path.unlink()


def build_project(project_name: str, app_name: str, dependencies: set[str]) -> None:
    repo_root = Path.cwd()
    entrypoint = repo_root / "install" / "pyinstaller_entrypoint.py"
    if not entrypoint.is_file():
        return
    build_root = (
        Path(os.environ.get("TMPDIR", "/tmp")) / f"{project_name}-release-build"
    )
    args = [
        "--noconfirm",
        "--distpath",
        str(build_root / "dist"),
        "--workpath",
        str(build_root / "build"),
        "--specpath",
        str(build_root),
        "--windowed",
        "--disable-windowed-traceback",
        "--name",
        app_name,
    ]

    if (repo_root / "icon.png").is_file():
        args.extend(["--icon", str(repo_root / "icon.png")])

    args.extend(hidden_imports(project_name, dependencies))

    if "sounddevice" in dependencies:
        args.extend(
            [
                "--hidden-import",
                "_sounddevice",
                "--collect-binaries",
                "_sounddevice_data",
            ]
        )
    if "soundfile" in dependencies:
        args.extend(
            ["--hidden-import", "_soundfile", "--collect-binaries", "_soundfile_data"]
        )

    args.extend(["--add-data", f"{repo_root / project_name}:{project_name}"])
    for p in PACKAGING_READMES:
        if (path := Path(p)).is_file():
            args.extend(["--add-data", f"{repo_root / path}:{path.name}"])

    args.append(str(entrypoint))

    run("uv", "run", "--with", "pyinstaller", "--with", "pillow", "pyinstaller", *args)

    if platform.system() == "Darwin":
        run(
            "codesign",
            "--force",
            "--deep",
            "--sign",
            "-",
            str(build_root / "dist" / f"{app_name}.app"),
        )


def hidden_imports(project_name: str, dependencies: set[str]) -> list[str]:
    args = []
    if project_name == "recs":
        args.extend(["--hidden-import", "recs.ui.gui_child"])
    if "mido" in dependencies:
        args.extend(["--hidden-import", "mido.backends.rtmidi"])
    if "pynput" in dependencies:
        match platform.system():
            case "Darwin":
                args.extend(
                    [
                        "--hidden-import",
                        "pynput.keyboard._darwin",
                        "--hidden-import",
                        "pynput._util.darwin",
                    ]
                )
            case "Linux":
                args.extend(
                    [
                        "--hidden-import",
                        "pynput.keyboard._xorg",
                        "--hidden-import",
                        "pynput._util.xorg",
                        "--hidden-import",
                        "pynput._util.uinput",
                    ]
                )
    return args


def git_output(*args: str) -> str:
    return command_output("git", *args)


def command_output(*args: str) -> str:
    result = subprocess.run(args, text=True, capture_output=True)
    if result.returncode:
        sys.exit(
            f"{' '.join(args)} failed with exit status {result.returncode}:\n"
            f"{result.stderr.strip()}"
        )
    return result.stdout.strip()


def command_fails(*args: str) -> bool:
    res = subprocess.run(args, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return res.returncode == 0


def run(*args: str) -> None:
    subprocess.run(args, check=True)


def run_linter(*args: str) -> None:
    result = subprocess.run(args)
    if changes := git_output("status", "--porcelain"):
        sys.exit(
            f"{args[2]} changed the working tree; review and commit the changes, "
            f"then rerun the release:\n{changes}"
        )
    result.check_returncode()


if __name__ == "__main__":
    raise SystemExit(main())
