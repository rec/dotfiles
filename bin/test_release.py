import importlib.util
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest import mock


def release_module():
    path = Path(__file__).with_name("release.py")
    spec = importlib.util.spec_from_file_location("release", path)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ReleaseTest(unittest.TestCase):
    def test_library_project_skips_pyinstaller(self):
        release = release_module()
        with TemporaryDirectory() as directory:
            with mock.patch.object(release.Path, "cwd", return_value=Path(directory)):
                with mock.patch.object(release, "run") as run:
                    release.build_project("library", "Library", set())

        run.assert_not_called()

    def test_release_publishes_pushed_tag(self):
        release = release_module()
        pyproject = {"name": "library", "requires-python": ">=3.10"}
        with (
            mock.patch.object(sys, "argv", ["release.py"]),
            mock.patch.object(release, "git_output", side_effect=["", "main", ""]),
            mock.patch.object(release, "read_pyproject", return_value={}),
            mock.patch.object(release, "project_table", return_value=pyproject),
            mock.patch.object(release, "project_name_from", return_value="library"),
            mock.patch.object(release, "python_version_from", return_value="310"),
            mock.patch.object(release, "project_dependencies", return_value=[]),
            mock.patch.object(
                release, "command_output", side_effect=["1.2.3", "1.2.3"]
            ),
            mock.patch.object(release, "command_fails", return_value=False),
            mock.patch.object(release, "checked_paths", return_value=([], [])),
            mock.patch.object(release, "build_project"),
            mock.patch.object(release, "run_pyupgrade"),
            mock.patch.object(release, "clean_dist"),
            mock.patch.object(release, "run") as run,
        ):
            release.main()

        run.assert_has_calls(
            [
                mock.call("git", "push", "origin", "v1.2.3"),
                mock.call(
                    "gh",
                    "release",
                    "create",
                    "v1.2.3",
                    "--verify-tag",
                    "--generate-notes",
                ),
            ]
        )
