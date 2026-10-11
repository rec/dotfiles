#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["tyro", "pydantic", "tomlkit", "packaging>=26.2"]
# ///
"""Align Python version classifiers with requires-python, through Python 3.15."""

from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path
from typing import Annotated, cast

import tomlkit
import tyro
from packaging.specifiers import SpecifierSet
from pydantic import BaseModel, Field
from tomlkit.items import Table


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
    document = tomlkit.parse(path.read_text())
    project = cast(Table, document["project"])
    classifiers = cast(list[str], project.get("classifiers", []))
    expected = expected_classifiers(str(project["requires-python"]), classifiers)
    if classifiers == expected:
        return
    if not dry_run:
        project["classifiers"] = expected
        path.write_text(tomlkit.dumps(document))
        result = subprocess.run(
            [
                "git",
                "-C",
                str(path.resolve().parent),
                "commit",
                "--only",
                "-m",
                "Update classifiers in pyproject.toml",
                "--",
                path.name,
            ],
            capture_output=True,
            text=True,
        )
        if result.returncode:
            sys.exit(result.stderr or result.stdout)
        result = subprocess.run(
            ["git", "-C", str(path.resolve().parent), "push"],
            capture_output=True,
            text=True,
        )
        if result.returncode:
            print(result.stderr or result.stdout, file=sys.stderr, end="")
    print(path)


if __name__ == "__main__":
    raise SystemExit(main())
