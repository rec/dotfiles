#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["tyro", "pydantic", "tomlkit", "packaging>=26.2"]
# ///
"""Fix Python version metadata and git:// dependency URLs in project files."""

from __future__ import annotations

import re
import subprocess
import sys
from difflib import unified_diff
from pathlib import Path
from typing import Annotated, cast

import tomlkit
import tyro
from packaging.requirements import Requirement
from packaging.specifiers import SpecifierSet
from packaging.version import Version
from pydantic import BaseModel, Field
from tomlkit.items import Array, Table


class ClassifiersCommand(BaseModel, frozen=True):
    paths: Annotated[list[Path], tyro.conf.Positional] = Field(default_factory=list)
    dry_run: Annotated[bool, tyro.conf.arg(aliases=["-d"])] = False


def main() -> int:
    command = tyro.cli(ClassifiersCommand)
    for path in command.paths or [Path("pyproject.toml")]:
        fix_classifiers(path, dry_run=command.dry_run)
    return 0


def expected_classifiers(requires_python: str, classifiers: list[str]) -> list[str]:
    requirement = SpecifierSet(requires_python, prereleases=False)
    versions = [
        f"Programming Language :: Python :: 3.{i}"
        for i in range(16)
        if not (
            requirement & SpecifierSet(f"==3.{i}.*", prereleases=False)
        ).is_unsatisfiable()
    ]
    others = [
        c
        for c in classifiers
        if not re.fullmatch(r"Programming Language :: Python :: \d+(?:\.\d+)*", c)
    ]
    return others + (
        ["Programming Language :: Python :: 3"] + versions if versions else []
    )


def fix_classifiers(path: Path, *, dry_run: bool = False) -> None:
    if path.is_dir():
        path /= "pyproject.toml"
    original = path.read_text()
    document = tomlkit.parse(original)
    project = cast(Table, document["project"])
    classifiers = cast(list[str], project.get("classifiers", []))
    requires_python = str(project["requires-python"])
    minimum = minimum_python_version(requires_python)
    expected = expected_classifiers(requires_python, classifiers)
    if classifiers != expected:
        project["classifiers"] = expected
        update_file(
            path,
            original,
            tomlkit.dumps(document),
            "Update classifiers in pyproject.toml",
            dry_run=dry_run,
        )
    version_path = path.with_name(".python-version")
    original_version = version_path.read_text() if version_path.exists() else ""
    if original_version.strip() != minimum:
        update_file(
            version_path,
            original_version,
            minimum + "\n",
            "Change inconsistent .python-version",
            dry_run=dry_run,
        )
    original = tomlkit.dumps(document)
    groups = cast(Table, document.get("dependency-groups", {}))
    for dependencies in (project.get("dependencies", []), groups.get("dev", [])):
        for i, dependency in enumerate(cast(Array, dependencies)):
            if not isinstance(dependency, str):
                continue
            url = Requirement(dependency).url
            if url is not None and url.startswith(("git://", "git+git://")):
                dependencies[i] = dependency.replace(
                    url, url.replace("git://", "https://", 1), 1
                )
    updated = tomlkit.dumps(document)
    if original != updated:
        update_file(path, original, updated, "Remove git:// URLs", dry_run=dry_run)


def minimum_python_version(requires_python: str) -> str:
    requirement = SpecifierSet(requires_python, prereleases=False)
    candidates = {Version(f"3.{i}") for i in range(16)}
    for specifier in requirement:
        version = Version(specifier.version.removesuffix(".*"))
        candidates.add(version)
        release = version.release + (0,) * (3 - len(version.release))
        candidates.add(Version(f"{release[0]}.{release[1]}.{release[2] + 1}"))
        candidates.add(Version(f"{release[0]}.{release[1] + 1}"))
    supported = sorted(v for v in candidates if v in requirement)
    if not supported:
        sys.exit(f"No minimum Python version satisfies {requires_python}")
    return str(supported[0])


def update_file(
    path: Path, original: str, updated: str, message: str, *, dry_run: bool
) -> None:
    if dry_run:
        print(
            "\n".join(
                unified_diff(
                    original.splitlines(),
                    updated.splitlines(),
                    fromfile=str(path),
                    tofile=str(path),
                    lineterm="",
                )
            )
        )
        return
    new_file = not path.exists()
    path.write_text(updated)
    if new_file:
        result = subprocess.run(
            ["git", "-C", str(path.resolve().parent), "add", "--", path.name],
            capture_output=True,
            text=True,
        )
        if result.returncode:
            sys.exit(result.stderr or result.stdout)
    result = subprocess.run(
        [
            "git",
            "-C",
            str(path.resolve().parent),
            "commit",
            "--only",
            "-m",
            message,
            "--",
            path.name,
        ],
        capture_output=True,
        text=True,
    )
    if result.returncode:
        sys.exit(result.stderr or result.stdout)
    result = subprocess.run(
        ["git", "-C", str(path.resolve().parent), "log", "-1", "--format=%h %s"],
        capture_output=True,
        text=True,
    )
    if result.returncode:
        sys.exit(result.stderr or result.stdout)
    print(result.stdout.strip())
    result = subprocess.run(
        ["git", "-C", str(path.resolve().parent), "push"],
        capture_output=True,
        text=True,
    )
    if result.returncode:
        print(result.stderr or result.stdout, file=sys.stderr, end="")


if __name__ == "__main__":
    raise SystemExit(main())
