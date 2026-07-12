#!/usr/bin/env -S uv run python
from __future__ import annotations

import os
import platform
import subprocess
import sys
from collections.abc import Callable
from importlib import import_module
from pathlib import Path
from typing import cast

VALID_PARTS = {"patch", "minor", "major"}
PACKAGING_READMES = (
    "README.md",
    "packaging/README-WINDOWS.txt",
    "packaging/README-MACOS.txt",
    "packaging/README-LINUX.txt",
)


def main() -> int:
    part = sys.argv[1] if len(sys.argv) > 1 else "patch"
    if part not in VALID_PARTS:
        sys.exit("usage: release.sh [patch|minor|major]")

    if git_output("status", "--porcelain"):
        sys.exit("release requires a clean working tree")

    branch = git_output("branch", "--show-current")
    if branch != "main":
        sys.exit(f"release must run from main, not {branch}")

    pyproject = read_pyproject()
    project = project_table(pyproject)
    project_name = project_name_from(project)
    app_name = project_name[:1].upper() + project_name[1:]
    dependencies = {dependency_name(d) for d in project_dependencies(project)}

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
    run("uv", "run", "ruff", "check", "--fix", "--select", "B,E,F,I", *ruff_paths)
    run("uv", "run", "ruff", "format", "--check", *ruff_paths)
    run("uv", "run", "ty", "check", project_name)
    run_pyupgrade(find_paths)
    run("git", "diff", "--check")

    if git_output("status", "--porcelain"):
        sys.exit("release verification changed the working tree")

    build_project(project_name, app_name, dependencies)

    run("uv", "version", "--bump", part, "--no-sync")
    version = command_output("uv", "version", "--short")
    tag = f"v{version}"
    if version != target_version:
        sys.exit(
            f"version changed after dry run: expected {target_version}, got {version}"
        )

    run("git", "add", "pyproject.toml", "uv.lock")
    run("git", "commit", "-m", f"Release {tag}")
    run("git", "push")
    run("git", "tag", tag)
    run("git", "push", "origin", tag)
    return 0


def read_pyproject() -> dict[str, object]:
    try:
        toml_module = import_module("tomllib")
    except ModuleNotFoundError:
        toml_module = import_module("tomli")
    loads = cast(Callable[[str], dict[str, object]], toml_module.loads)
    return loads(Path("pyproject.toml").read_text())


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


def dependency_name(dependency: str) -> str:
    for s in ";[=<>~!":
        dependency = dependency.split(s, 1)[0]
    return dependency.strip().lower()


def checked_paths(project_name: str) -> tuple[list[str], list[str]]:
    ruff_paths = [project_name]
    find_paths = [project_name]

    if Path("install").is_dir():
        ruff_paths.append("install")
        find_paths.append("install")

    for p in sorted(Path().glob("test*")):
        ruff_paths.append(str(p))

    if Path("test").is_dir():
        find_paths.append("test")

    return ruff_paths, find_paths


def run_pyupgrade(find_paths: list[str]) -> None:
    python_version = Path(".python-version").read_text().strip().replace(".", "")
    files = [str(p) for root in find_paths for p in Path(root).rglob("*.py")]
    if files:
        run("uv", "run", "pyupgrade", f"--py{python_version}-plus", *files)


def build_project(project_name: str, app_name: str, dependencies: set[str]) -> None:
    repo_root = Path.cwd()
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

    args.append("install/pyinstaller_entrypoint.py")

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
    return subprocess.run(args, text=True, capture_output=True).stdout.strip()


def command_fails(*args: str) -> bool:
    res = subprocess.run(args, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return res.returncode == 0


def run(*args: str) -> None:
    subprocess.run(args, check=True)


if __name__ == "__main__":
    raise SystemExit(main())
