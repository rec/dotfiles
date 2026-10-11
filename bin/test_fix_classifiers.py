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
def test_dry_run_prints_diffs_without_changes(flag: str) -> None:
    module = classifiers_module()
    with TemporaryDirectory() as directory:
        path = Path(directory) / "pyproject.toml"
        original = '[project]\nrequires-python = ">=3.15"\n'
        path.write_text(original)
        path.with_name(".python-version").write_text("3.15\n")
        output = StringIO()
        with (
            mock.patch.object(
                sys, "argv", ["fix-classifiers.py", flag, directory, str(path)]
            ),
            mock.patch.object(module.subprocess, "run") as run,
            redirect_stdout(output),
        ):
            assert module.main() == 0
        expected = (
            f"--- {path}\n+++ {path}\n@@ -1,2 +1,3 @@\n"
            ' [project]\n requires-python = ">=3.15"\n'
            '+classifiers = ["Programming Language :: Python :: 3", '
            '"Programming Language :: Python :: 3.15"]\n'
        )
        assert output.getvalue() == expected * 2
        assert path.read_text() == original
        assert path.with_name(".python-version").read_text() == "3.15\n"
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
        path.with_name(".python-version").write_text("3.15\n")
        with (
            mock.patch.object(
                module.subprocess,
                "run",
                side_effect=[
                    subprocess.CompletedProcess([], 0, stdout="committed", stderr=""),
                    subprocess.CompletedProcess(
                        [],
                        0,
                        stdout="abc123 Update classifiers in pyproject.toml\n",
                        stderr="",
                    ),
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
        assert run.call_args_list[2].args[0] == [
            "git",
            "-C",
            str(path.resolve().parent),
            "push",
        ]
        assert output.getvalue() == "abc123 Update classifiers in pyproject.toml\n"
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


@pytest.mark.parametrize(
    ("requirement", "minimum"),
    [
        (">=3.10", "3.10"),
        (">=3.10.5", "3.10.5"),
        (">3.10", "3.10.1"),
        (">=3.10,!=3.10.*", "3.11"),
    ],
)
def test_python_version_uses_supported_minimum(requirement: str, minimum: str) -> None:
    assert classifiers_module().minimum_python_version(requirement) == minimum


@pytest.mark.parametrize("existing", ["3.14\n", None])
def test_dry_run_shows_python_version_diff_without_writing(
    existing: str | None,
) -> None:
    module = classifiers_module()
    with TemporaryDirectory() as directory:
        path = Path(directory) / "pyproject.toml"
        original = '[project]\nrequires-python = ">=3.15"\n'
        path.write_text(original)
        version_path = path.with_name(".python-version")
        if existing is not None:
            version_path.write_text(existing)
        with (
            mock.patch.object(module.subprocess, "run") as run,
            redirect_stdout(StringIO()) as output,
        ):
            module.fix_classifiers(path, dry_run=True)
        assert f"--- {path}\n+++ {path}\n" in output.getvalue()
        assert f"--- {version_path}\n+++ {version_path}\n" in output.getvalue()
        assert "+3.15\n" in output.getvalue()
        if existing is not None:
            assert "-3.14\n" in output.getvalue()
            assert version_path.read_text() == existing
        else:
            assert not version_path.exists()
        assert path.read_text() == original
        run.assert_not_called()


@pytest.mark.parametrize("existing", ["3.14\n", None])
def test_python_version_has_its_own_commit_and_commit_output(
    existing: str | None,
) -> None:
    module = classifiers_module()
    with TemporaryDirectory() as directory:
        path = Path(directory) / "pyproject.toml"
        path.write_text('[project]\nrequires-python = ">=3.15"\n')
        version_path = path.with_name(".python-version")
        if existing is not None:
            version_path.write_text(existing)
        results = [
            subprocess.CompletedProcess([], 0, stdout="", stderr=""),
            subprocess.CompletedProcess(
                [], 0, stdout="aaa111 Update classifiers in pyproject.toml\n", stderr=""
            ),
            subprocess.CompletedProcess([], 0, stdout="", stderr=""),
        ]
        if existing is None:
            results.append(subprocess.CompletedProcess([], 0, stdout="", stderr=""))
        results.extend(
            [
                subprocess.CompletedProcess([], 0, stdout="", stderr=""),
                subprocess.CompletedProcess(
                    [],
                    0,
                    stdout="bbb222 Change inconsistent .python-version\n",
                    stderr="",
                ),
                subprocess.CompletedProcess([], 0, stdout="", stderr=""),
            ]
        )
        with (
            mock.patch.object(module.subprocess, "run", side_effect=results) as run,
            redirect_stdout(StringIO()) as output,
        ):
            module.fix_classifiers(path)
        commits = [c.args[0] for c in run.call_args_list if "commit" in c.args[0]]
        assert [c[6:] for c in commits] == [
            ["Update classifiers in pyproject.toml", "--", "pyproject.toml"],
            ["Change inconsistent .python-version", "--", ".python-version"],
        ]
        assert all("--only" in c for c in commits)
        assert output.getvalue() == (
            "aaa111 Update classifiers in pyproject.toml\n"
            "bbb222 Change inconsistent .python-version\n"
        )
        assert version_path.read_text() == "3.15\n"
        assert sum("push" in c.args[0] for c in run.call_args_list) == 2
        with (
            mock.patch.object(module.subprocess, "run") as run,
            redirect_stdout(StringIO()) as output,
        ):
            module.fix_classifiers(path)
        assert output.getvalue() == ""
        run.assert_not_called()
