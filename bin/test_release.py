import importlib.util
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
