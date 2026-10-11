import importlib.util
import subprocess
import sys
from contextlib import redirect_stderr, redirect_stdout
from io import StringIO
from pathlib import Path
from tempfile import TemporaryDirectory
from types import ModuleType
from unittest import mock

import pytest
import tomlkit


def classifiers_module() -> ModuleType:
    spec = importlib.util.spec_from_file_location(
        "fix_classifiers", Path(__file__).with_name("fix-classifiers.py")
    )
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize(
    ("requirement", "minors"),
    [
        (">=3.10", range(10, 16)),
        (">=3.10,<3.13,!=3.11.*", [10, 12]),
        (">=3.10.5,<3.11", [10]),
    ],
)
def test_classifiers_follow_version_constraints(
    requirement: str, minors: list[int]
) -> None:
    module = classifiers_module()
    assert module.expected_classifiers(
        requirement, ["Typing :: Typed", "Programming Language :: Python :: 2.7"]
    ) == ["Typing :: Typed", "Programming Language :: Python :: 3"] + [
        f"Programming Language :: Python :: 3.{i}" for i in minors
    ]


@pytest.mark.parametrize("flag", ["--dry-run", "-d"])
def test_dry_run_prints_only_incorrect_files_without_changes(flag: str) -> None:
    module = classifiers_module()
    with TemporaryDirectory() as directory:
        path = Path(directory) / "pyproject.toml"
        original = '[project]\nrequires-python = ">=3.15"\n'
        path.write_text(original)
        output = StringIO()
        with (
            mock.patch.object(
                sys, "argv", ["fix-classifiers.py", flag, directory, str(path)]
            ),
            mock.patch.object(module.subprocess, "run") as run,
            redirect_stdout(output),
        ):
            assert module.main() == 0
        assert output.getvalue() == f"{path}\n{path}\n"
        assert path.read_text() == original
        run.assert_not_called()


def test_edit_commits_only_target_and_reports_push_failure() -> None:
    module = classifiers_module()
    with TemporaryDirectory() as directory:
        path = Path(directory) / "pyproject.toml"
        path.write_text(
            '# comment\n[project]\nname = "demo" # keep\nrequires-python = ">=3.15"\n'
            'classifiers = ["Typing :: Typed", '
            '"Programming Language :: Python :: 3.10"]\n'
            "\n[tool.example]\nvalue = 42\n"
        )
        output, errors = StringIO(), StringIO()
        with (
            mock.patch.object(
                module.subprocess,
                "run",
                side_effect=[
                    subprocess.CompletedProcess([], 0, stdout="committed", stderr=""),
                    subprocess.CompletedProcess(
                        [], 1, stdout="", stderr="push failed\n"
                    ),
                ],
            ) as run,
            redirect_stdout(output),
            redirect_stderr(errors),
        ):
            module.fix_classifiers(path)
        text = path.read_text()
        assert text.startswith('# comment\n[project]\nname = "demo" # keep\n')
        assert text.endswith("\n[tool.example]\nvalue = 42\n")
        assert tomlkit.parse(text)["project"]["classifiers"] == [
            "Typing :: Typed",
            "Programming Language :: Python :: 3",
            "Programming Language :: Python :: 3.15",
        ]
        assert run.call_args_list[0].args[0] == [
            "git",
            "-C",
            str(path.resolve().parent),
            "commit",
            "--only",
            "-m",
            "Update classifiers in pyproject.toml",
            "--",
            "pyproject.toml",
        ]
        assert run.call_args_list[1].args[0] == [
            "git",
            "-C",
            str(path.resolve().parent),
            "push",
        ]
        assert output.getvalue() == f"{path}\n"
        assert errors.getvalue() == "push failed\n"
        with (
            mock.patch.object(module.subprocess, "run") as run,
            redirect_stdout(StringIO()) as output,
        ):
            module.fix_classifiers(path)
        run.assert_not_called()
        assert output.getvalue() == ""


def test_default_path_is_current_pyproject() -> None:
    module = classifiers_module()
    with (
        mock.patch.object(sys, "argv", ["fix-classifiers.py"]),
        mock.patch.object(module, "fix_classifiers") as fix,
    ):
        assert module.main() == 0
    fix.assert_called_once_with(Path("pyproject.toml"), dry_run=False)
