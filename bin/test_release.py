import importlib.util
import subprocess
import sys
import unittest
from contextlib import chdir
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest import mock


def release_module():
    path = Path(__file__).with_name("release.py")
    spec = importlib.util.spec_from_file_location("release", path)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class ReleaseTest(unittest.TestCase):
    def test_help_does_not_sync_the_current_project(self) -> None:
        script = Path(__file__).with_name("release.py").resolve()
        with TemporaryDirectory() as directory:
            project = Path(directory) / "pyproject.toml"
            original = '[project]\nname = "help-test"\nversion = "0.1.0"\n'
            project.write_text(original)
            result = subprocess.run(
                [str(script), "--help"],
                cwd=directory,
                capture_output=True,
                text=True,
                check=True,
            )
            self.assertIn("--publish", result.stdout)
            self.assertEqual(result.stderr, "")
            self.assertEqual(project.read_text(), original)
            self.assertEqual(list(Path(directory).iterdir()), [project])

    def test_failed_version_preview_stops_before_verification(self) -> None:
        release = release_module()
        error = "We cannot get or set dynamic project versions in: pyproject.toml"
        with (
            mock.patch.object(sys, "argv", ["release.py", "minor", "--publish"]),
            mock.patch.dict(release.os.environ),
            mock.patch.object(release, "git_output", side_effect=["", "main"]),
            mock.patch.object(
                release,
                "read_pyproject",
                return_value={
                    "project": {
                        "name": "library",
                        "requires-python": ">=3.11",
                    }
                },
            ),
            mock.patch.object(
                release.subprocess,
                "run",
                return_value=subprocess.CompletedProcess(
                    [], 2, stdout="", stderr=error
                ),
            ),
            mock.patch.object(release, "run") as run,
            self.assertRaises(SystemExit) as stopped,
        ):
            release.main()
        run.assert_not_called()
        self.assertEqual(
            str(stopped.exception),
            "uv version --bump minor --dry-run --short failed with exit status 2:\n"
            + error,
        )

    def test_linter_changes_stop_release_cleanly(self) -> None:
        release = release_module()
        for tool in ("ruff", "ty", "pyupgrade"):
            for returncode in (0, 1):
                with (
                    self.subTest(tool=tool, returncode=returncode),
                    mock.patch.object(
                        release.subprocess,
                        "run",
                        return_value=subprocess.CompletedProcess([], returncode),
                    ),
                    mock.patch.object(
                        release, "git_output", return_value=" M library/live.py"
                    ),
                    self.assertRaises(SystemExit) as stopped,
                ):
                    release.run_linter("uv", "run", tool)
                self.assertEqual(
                    str(stopped.exception),
                    f"{tool} changed the working tree; review and commit the changes, "
                    "then rerun the release:\n M library/live.py",
                )

    def test_linter_failure_without_changes_is_not_ignored(self) -> None:
        release = release_module()
        with (
            mock.patch.object(
                release.subprocess,
                "run",
                return_value=subprocess.CompletedProcess([], 2),
            ),
            mock.patch.object(release, "git_output", return_value=""),
            self.assertRaises(subprocess.CalledProcessError),
        ):
            release.run_linter("uv", "run", "pyupgrade")

    def test_verification_paths_use_the_top_level_package(self) -> None:
        release = release_module()
        with TemporaryDirectory() as directory, chdir(directory):
            Path("library").mkdir()
            Path("test").mkdir()
            Path("install").mkdir()
            ruff_paths, find_paths = release.checked_paths("library")
        self.assertEqual(ruff_paths, ["library", "install", "test"])
        self.assertEqual(find_paths, ["library", "install", "test"])

    def test_missing_top_level_package_stops_with_a_clear_message(self) -> None:
        release = release_module()
        with TemporaryDirectory() as directory, chdir(directory):
            Path("src/library").mkdir(parents=True)
            Path("python/library").mkdir(parents=True)
            with self.assertRaises(SystemExit) as stopped:
                release.checked_paths("library")
        self.assertEqual(
            str(stopped.exception),
            "release requires a top-level Python package directory: library",
        )

    def test_library_project_skips_pyinstaller(self):
        release = release_module()
        with TemporaryDirectory() as directory:
            with mock.patch.object(release.Path, "cwd", return_value=Path(directory)):
                with mock.patch.object(release, "run") as run:
                    release.build_project("library", "Library", set())

        run.assert_not_called()

    def test_release_publishes_pushed_tag(self) -> None:
        self.check_release(publish=False)

    def test_publish_builds_after_clearing_dist_at_the_end(self) -> None:
        self.check_release(publish=True)

    def test_maturin_release_commits_cargo_versions_and_publishes(self) -> None:
        self.check_release(publish=True, dynamic=True)

    def test_cargo_version_bump_preserves_the_manifest(self) -> None:
        release = release_module()
        original = (
            '[package]\nname = "midirp"\nversion = "1.2.3" # release\n\n'
            '[dependencies]\npyo3 = "=0.29.3"\n'
        )
        for part, expected in (
            ("major", "2.0.0"),
            ("minor", "1.3.0"),
            ("patch", "1.2.4"),
        ):
            with (
                self.subTest(part=part),
                TemporaryDirectory() as directory,
                chdir(directory),
            ):
                path = Path("Cargo.toml")
                path.write_text(original)
                self.assertEqual(
                    release.bump_cargo_version(part, dry_run=True), expected
                )
                self.assertEqual(path.read_text(), original)
                self.assertEqual(release.bump_cargo_version(part), expected)
                self.assertEqual(
                    path.read_text(), original.replace('"1.2.3"', f'"{expected}"')
                )

    def check_release(self, publish: bool, dynamic: bool = False) -> None:
        release = release_module()
        pyproject = {"name": "library", "requires-python": ">=3.10"}
        if dynamic:
            pyproject["dynamic"] = ["version"]
        with (
            mock.patch.dict(release.os.environ, {"VIRTUAL_ENV": "/tmp/uv-launcher"}),
            mock.patch.object(
                sys,
                "argv",
                ["release.py", "minor", *(["--publish"] if publish else [])],
            ),
            mock.patch.object(
                release,
                "git_output",
                side_effect=["", "main", "git@github.com:rec/library.git", ""],
            ),
            mock.patch.object(
                release,
                "read_pyproject",
                return_value={"build-system": {"build-backend": "maturin"}},
            ),
            mock.patch.object(release, "project_table", return_value=pyproject),
            mock.patch.object(release, "project_name_from", return_value="library"),
            mock.patch.object(release, "python_version_from", return_value="310"),
            mock.patch.object(release, "project_dependencies", return_value=[]),
            mock.patch.object(
                release, "command_output", side_effect=["1.2.3", "1.2.3"]
            ),
            mock.patch.object(release, "command_fails", return_value=False),
            mock.patch.object(
                release,
                "checked_paths",
                return_value=(["library"], ["library"]),
            ),
            mock.patch.object(release, "build_project"),
            mock.patch.object(release, "run_pyupgrade"),
            mock.patch.object(release, "clean_dist") as clean_dist,
            mock.patch.object(release, "run") as run,
            mock.patch.object(release, "run_linter") as run_linter,
            mock.patch.object(
                release, "bump_cargo_version", return_value="1.2.3"
            ) as bump_cargo_version,
        ):
            calls = mock.Mock()
            calls.attach_mock(run, "run")
            calls.attach_mock(clean_dist, "clean_dist")
            release.main()
            self.assertNotIn("VIRTUAL_ENV", release.os.environ)

        run_linter.assert_any_call("uv", "run", "ty", "check", "library")
        if dynamic:
            bump_cargo_version.assert_has_calls(
                [mock.call("minor", dry_run=True), mock.call("minor")]
            )
            run.assert_any_call("git", "add", "Cargo.toml", "Cargo.lock")
            run.assert_any_call("cargo", "update", "--workspace", "--offline")
            self.assertNotIn(
                mock.call("uv", "version", "--bump", "minor", "--no-sync"),
                run.mock_calls,
            )
        run.assert_has_calls(
            [
                mock.call("git", "push", "origin", "v1.2.3"),
                mock.call(
                    "gh",
                    "release",
                    "create",
                    "v1.2.3",
                    "--repo",
                    "git@github.com:rec/library.git",
                    "--verify-tag",
                    "--generate-notes",
                ),
            ]
        )
        if publish:
            self.assertEqual(
                calls.mock_calls[-3:],
                [
                    mock.call.clean_dist(),
                    mock.call.run("uv", "build"),
                    mock.call.run("uv", "publish"),
                ],
            )
        else:
            self.assertEqual(calls.mock_calls[-1], mock.call.clean_dist())
