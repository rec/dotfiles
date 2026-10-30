import importlib.util
import json
import subprocess
import sys
from contextlib import nullcontext
from io import StringIO
from pathlib import Path

import pytest
import tomllib
from pytest_regressions.file_regression import FileRegressionFixture

spec = importlib.util.spec_from_file_location(
    "codexo", Path(__file__).with_name("codexo.py")
)
assert spec is not None and spec.loader is not None
codexo = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = codexo
spec.loader.exec_module(codexo)


@pytest.mark.parametrize("flags", [[], ["-v"], ["--verbose"]])
@pytest.mark.parametrize("retire_prompt", [False, True])
def test_retire_picker_only_includes_chat_details_in_verbose_mode(
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    flags: list[str],
    retire_prompt: bool,
) -> None:
    server = codexo.AppServer(StringIO(), StringIO())
    monkeypatch.setattr(
        server,
        "request",
        lambda *a: {
            "data": [
                {"id": "chat-id", "name": "Chat name", "cwd": "/repo", "status": {}}
            ]
        },
    )
    monkeypatch.setattr(codexo, "desktop_server", lambda: nullcontext(server))
    monkeypatch.setattr("builtins.input", lambda p: "1")
    selected: list[tuple[str, bool]] = []
    monkeypatch.setattr(codexo, "replace_chat", lambda s, i, p: selected.append((i, p)))
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "codexo.py",
            *flags,
            "retire",
            *([] if retire_prompt else ["--no-retire-prompt"]),
        ],
    )
    codexo.main()
    detail = " [chat-id] (/repo)" if flags else ""
    assert capsys.readouterr().out == f"1. Chat name{detail}\n"
    assert selected == [("chat-id", retire_prompt)]


@pytest.mark.parametrize("flag", ["-d", "--dry-run"])
@pytest.mark.parametrize("thread_id", [None, "old-chat-id"])
@pytest.mark.parametrize("retire_prompt", [False, True])
def test_dry_run_prints_operations_without_performing_them(
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    flag: str,
    thread_id: str | None,
    retire_prompt: bool,
) -> None:
    args = ["codexo.py", flag, "retire"]
    if thread_id is not None:
        args.append(thread_id)
    if not retire_prompt:
        args.append("--no-retire-prompt")
    monkeypatch.setattr(sys, "argv", args)

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Dry-run must not start app-server or create files")

    monkeypatch.setattr(codexo.subprocess, "Popen", unexpected_operation)
    monkeypatch.setattr(codexo, "desktop_server", unexpected_operation)
    codexo.main()
    output = capsys.readouterr().out
    assert "no app-server, file, or chat operations" in output
    assert ("thread/list:" in output) == (thread_id is None)
    assert ("thread/resume:" in output) is retire_prompt
    assert ("send no prompt to the retiring chat" in output) is not retire_prompt
    assert (thread_id or "<selected-chat-id>") in output
    for operation in [
        "initialize",
        "initialized",
        "thread/read",
        "handover.md",
        "turn/start",
        "turn/completed",
        "thread/start",
        "thread/name/set",
        "summarize next tasks",
        "thread/archive",
        "disconnect from app-server",
    ]:
        assert operation in output


@pytest.mark.parametrize("flag", ["-d", "--dry-run"])
@pytest.mark.parametrize("codex_flags", [[], ["--codex"]])
def test_fix_dry_run_lists_repositories_without_reading_them(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    flag: str,
    codex_flags: list[str],
) -> None:
    for r in ["safer", "tuney"]:
        directory = tmp_path / "code" / r
        directory.mkdir(parents=True)
        (directory / "pyproject.toml").touch()
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    monkeypatch.setattr(
        sys, "argv", ["codexo.py", flag, "fix", *codex_flags, "safer", "tuney"]
    )

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Dry-run must not start app-server or read repositories")

    monkeypatch.setattr(codexo.subprocess, "Popen", unexpected_operation)
    monkeypatch.setattr(codexo, "report_repository", unexpected_operation)
    codexo.main()
    output = capsys.readouterr()
    assert "safer:" in output.out
    assert "tuney:" in output.out
    assert "project/list" in output.out
    assert "thread/list" in output.out
    assert output.err == ""


@pytest.mark.parametrize("dry_run", [False, True])
def test_fix_without_codex_reports_partial_repos_and_ignores_non_directories(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    dry_run: bool,
) -> None:
    for r in ["safer", "tuney"]:
        directory = tmp_path / "code" / r
        directory.mkdir(parents=True)
        (directory / "pyproject.toml").write_text(
            '[project]\ndescription = "Local description"\n'
        )
        (directory / "README.md").write_text("# Repository title\n")
    (tmp_path / "code" / "empty").mkdir()
    (tmp_path / "code" / "file").touch()
    legacy = tmp_path / "code" / "legacy"
    legacy.mkdir()
    (legacy / "pyproject.toml").write_text("[tool.old]\n")
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    flags = ["-d"] if dry_run else []
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "codexo.py",
            *flags,
            "fix",
            "--no-codex",
            "missing",
            "safer",
            "empty",
            "file",
            "legacy",
            "tuney",
        ],
    )

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Codex must not be interrogated")

    def github_description(
        *args: object, **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        assert not dry_run
        if str(kwargs["cwd"]).endswith("/legacy"):
            raise subprocess.CalledProcessError(1, ["gh"], stderr="Unknown host")
        return subprocess.CompletedProcess([], 0, "GitHub description\n")

    monkeypatch.setattr(codexo.subprocess, "Popen", unexpected_operation)
    monkeypatch.setattr(codexo, "codex_info", unexpected_operation)
    monkeypatch.setattr(codexo.subprocess, "run", github_description)
    codexo.main()
    output = capsys.readouterr()
    if dry_run:
        assert "safer:" in output.out
        assert "tuney:" in output.out
    else:
        report = tomllib.loads(output.out)
        for r in ["safer", "tuney"]:
            assert report[r]["pyproject"] == "Local description"
            assert report[r]["README"] == "Repository title"
            assert report[r]["github"] == "GitHub description"
            assert "codex" not in report[r]
        assert "legacy" not in report
        assert report["empty"]["github"] == "GitHub description"
    assert "codex" not in output.out.lower()
    assert "missing" not in output.out
    assert "file:" not in output.out
    assert output.err == ""


@pytest.mark.parametrize("failed", ["pyproject", "README", "github", "codex", "all"])
def test_failed_sources_do_not_hide_other_information(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    failed: str,
) -> None:
    monkeypatch.setattr(codexo, "pyproject_description", lambda p: "Local description")
    monkeypatch.setattr(codexo, "readme_heading", lambda p: "README title")
    monkeypatch.setattr(codexo, "github_description", lambda p: "GitHub description")
    monkeypatch.setattr(
        codexo, "codex_info", lambda *a: codexo.CodexInfo(project="Project")
    )
    sources = {
        "pyproject": "pyproject_description",
        "README": "readme_heading",
        "github": "github_description",
        "codex": "codex_info",
    }

    def unavailable(*args: object) -> None:
        raise RuntimeError("Unavailable source")

    for name, function in sources.items():
        if failed in {name, "all"}:
            monkeypatch.setattr(codexo, function, unavailable)
    codexo.report_repository(codexo.AppServer(StringIO(), StringIO()), tmp_path)
    output = capsys.readouterr()
    assert output.err == ""
    if failed == "all":
        assert output.out == ""
    else:
        report = tomllib.loads(output.out)[tmp_path.name]
        assert len(report) == 3
        for name in sources:
            assert (name in report) == (name != failed)


@pytest.mark.parametrize("error", [FileNotFoundError, RuntimeError])
def test_codex_startup_failure_still_reports_other_sources(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    error: type[Exception],
) -> None:
    for r in ["first", "second"]:
        (tmp_path / "code" / r).mkdir(parents=True)
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    monkeypatch.setattr(sys, "argv", ["codexo.py", "fix", "first", "second"])
    monkeypatch.setattr(codexo, "pyproject_description", lambda p: "Local description")
    monkeypatch.setattr(codexo, "readme_heading", lambda p: "README title")
    monkeypatch.setattr(codexo, "github_description", lambda p: "GitHub description")

    def unavailable(*args: object, **kwargs: object) -> None:
        raise error("Codex unavailable")

    monkeypatch.setattr(codexo, "app_server", unavailable)
    codexo.main()
    output = capsys.readouterr()
    report = tomllib.loads(output.out)
    for r in ["first", "second"]:
        assert report[r]["pyproject"] == "Local description"
        assert report[r]["README"] == "README title"
        assert report[r]["github"] == "GitHub description"
        assert "codex" not in report[r]
    assert output.err == ""


@pytest.mark.parametrize(
    "content",
    [
        None,
        b"[tool.old]\n",
        b"[project]\nname = 'old'\n",
        b"project = 1\n",
        b"[project]\ndescription = 1\n",
        b"[project\n",
        b"\xff",
    ],
)
def test_unusable_pyproject_omits_only_pyproject(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    content: bytes | None,
) -> None:
    if content is not None:
        (tmp_path / "pyproject.toml").write_bytes(content)
    (tmp_path / "README.md").write_text("# README title\n")
    monkeypatch.setattr(codexo, "github_description", lambda p: "GitHub description")
    monkeypatch.setattr(
        codexo, "codex_info", lambda *a: codexo.CodexInfo(project="Project")
    )
    server = codexo.AppServer(StringIO(), StringIO())
    codexo.report_repository(server, tmp_path)
    output = capsys.readouterr()
    report = tomllib.loads(output.out)[tmp_path.name]
    assert "pyproject" not in report
    assert report["README"] == "README title"
    assert report["github"] == "GitHub description"
    assert report["codex"]["project"] == "Project"
    assert output.err == ""


def test_github_failure_omits_only_github(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    (tmp_path / "pyproject.toml").write_text(
        '[project]\ndescription = "Local description"\n'
    )

    def unknown_host(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        assert command[:3] == ["gh", "repo", "view"]
        assert kwargs["capture_output"] is True
        raise subprocess.CalledProcessError(1, command, stderr="Unknown GitHub host")

    monkeypatch.setattr(codexo.subprocess, "run", unknown_host)
    monkeypatch.setattr(
        codexo, "codex_info", lambda *a: codexo.CodexInfo(project="Project")
    )

    codexo.report_repository(codexo.AppServer(StringIO(), StringIO()), tmp_path)

    output = capsys.readouterr()
    report = tomllib.loads(output.out)[tmp_path.name]
    assert report["pyproject"] == "Local description"
    assert report["codex"]["project"] == "Project"
    assert "github" not in report
    assert output.err == ""


def test_unreadable_local_files_do_not_hide_github(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    def unreadable(*args: object, **kwargs: object) -> None:
        raise PermissionError("unreadable pyproject.toml")

    monkeypatch.setattr(codexo.Path, "open", unreadable)
    monkeypatch.setattr(codexo, "github_description", lambda p: "GitHub description")
    codexo.report_repository(None, tmp_path)
    output = capsys.readouterr()
    report = tomllib.loads(output.out)[tmp_path.name]
    assert report["github"] == "GitHub description"
    assert "pyproject" not in report
    assert "README" not in report
    assert output.err == ""


@pytest.mark.parametrize("dry_run", [False, True])
def test_fix_silently_ignores_all_invalid_entries(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    dry_run: bool,
) -> None:
    (tmp_path / "code").mkdir()
    (tmp_path / "code" / "file").touch()
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    flags = ["-d"] if dry_run else []
    monkeypatch.setattr(sys, "argv", ["codexo.py", *flags, "fix", "missing", "file"])

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Ignored entries must not trigger external operations")

    monkeypatch.setattr(codexo.subprocess, "Popen", unexpected_operation)
    monkeypatch.setattr(codexo.subprocess, "run", unexpected_operation)
    codexo.main()
    output = capsys.readouterr()
    assert output.out == ""
    assert output.err == ""


def test_repository_report_preserves_inconsistent_metadata(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    file_regression: FileRegressionFixture,
) -> None:
    directory = tmp_path / "code" / "safer"
    directory.mkdir(parents=True)
    pyproject = (
        '[project]\nname = "safer"\n'
        'description = "🧿 A safer writer for files and streams 🧿"\n'
    )
    readme = "# 🧿 `safer`: A safer writer 🧿\n\nBody.\n"
    (directory / "pyproject.toml").write_text(pyproject)
    (directory / "README.md").write_text(readme)
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    monkeypatch.setattr(
        codexo.subprocess,
        "run",
        lambda *a, **k: codexo.subprocess.CompletedProcess(
            a, 0, "🧷 A safer writer 🧷\n"
        ),
    )
    monkeypatch.setattr(
        codexo, "codex_info", lambda *a: codexo.CodexInfo(project="safer")
    )
    server = codexo.AppServer(StringIO(), StringIO())
    codexo.report_repository(server, directory)
    file_regression.check(capsys.readouterr().out.rstrip() + "\n")
    assert (directory / "pyproject.toml").read_text() == pyproject
    assert (directory / "README.md").read_text() == readme


@pytest.mark.parametrize("readme", [None, "", "Body text.\n## Section\n"])
@pytest.mark.parametrize("directory_name", ["repo", "repo.with spaces"])
def test_missing_readme_heading_omits_readme_and_continues_report(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    readme: str | None,
    directory_name: str,
) -> None:
    directory = tmp_path / directory_name
    directory.mkdir()
    (directory / "pyproject.toml").write_text(
        '[project]\ndescription = "Local description"\n'
    )
    if readme is not None:
        (directory / "README.md").write_text(readme)
    monkeypatch.setattr(
        codexo.subprocess,
        "run",
        lambda *a, **k: subprocess.CompletedProcess([], 0, "GitHub description\n"),
    )
    monkeypatch.setattr(
        codexo,
        "codex_info",
        lambda *a: codexo.CodexInfo(project="Codex project", chats=["Codex chat"]),
    )
    server = codexo.AppServer(StringIO(), StringIO())
    codexo.report_repository(server, directory)
    output = capsys.readouterr()
    report = tomllib.loads(output.out)[directory_name]
    assert "README" not in report
    assert report["github"] == "GitHub description"
    assert report["codex"]["project"] == "Codex project"
    assert report["codex"]["chats"] == ["Codex chat"]
    assert 'codex = {project = "Codex project", chats = ["Codex chat"]}' in output.out
    assert output.err == ""


@pytest.mark.parametrize("matches", [0, 1, 2])
def test_codex_info_matches_exact_roots_and_keeps_project_separate_from_chats(
    tmp_path: Path,
    matches: int,
) -> None:
    projects = [{"name": "Other project", "roots": [{"path": str(tmp_path / "child")}]}]
    selected = [
        {"name": "Project title", "roots": [{"path": str(tmp_path)}]}
        for _ in range(matches)
    ]
    messages = [
        {"id": 1, "result": {"data": projects, "nextCursor": "more-projects"}},
        {"id": 2, "result": {"data": selected}},
        {
            "id": 3,
            "result": {
                "data": [
                    {
                        "id": "a",
                        "name": "First chat",
                        "cwd": str(tmp_path),
                        "status": {"type": "idle"},
                    }
                ],
                "nextCursor": "more-chats",
            },
        },
        {
            "id": 4,
            "result": {
                "data": [
                    {
                        "id": "b",
                        "name": "Second chat",
                        "cwd": str(tmp_path),
                        "status": {"type": "idle"},
                    }
                ]
            },
        },
    ]
    writer = StringIO()
    server = codexo.AppServer(
        StringIO("".join(json.dumps(m) + "\n" for m in messages)), writer
    )
    if matches > 1:
        with pytest.raises(RuntimeError, match="Multiple Codex projects"):
            codexo.codex_info(server, tmp_path)
    else:
        info = codexo.codex_info(server, tmp_path)
        assert info.project == ("Project title" if matches else None)
        assert len(info.chats) == 2
        assert info.chats[0] == "First chat"
        assert info.chats[1] == "Second chat"
        for r in map(json.loads, writer.getvalue().splitlines()[2:]):
            assert r["params"]["cwd"] == str(tmp_path)
            assert r["params"]["archived"] is False


def test_turn_retains_events_received_before_start_response() -> None:
    messages = [
        {
            "method": "turn/completed",
            "params": {
                "threadId": "other",
                "turn": {"id": "other", "status": "completed"},
            },
        },
        {
            "method": "item/completed",
            "params": {
                "threadId": "old",
                "turnId": "earlier",
                "item": {"type": "agentMessage", "text": "stale"},
            },
        },
        {
            "method": "item/completed",
            "params": {
                "threadId": "old",
                "turnId": "turn",
                "item": {"type": "agentMessage", "text": "Reading the handover."},
            },
        },
        {
            "method": "item/completed",
            "params": {
                "threadId": "old",
                "turnId": "turn",
                "item": {"type": "agentMessage", "text": "summary"},
            },
        },
        {
            "method": "turn/completed",
            "params": {
                "threadId": "old",
                "turn": {"id": "turn", "status": "completed"},
            },
        },
        {"id": 1, "result": {"turn": {"id": "turn", "status": "inProgress"}}},
    ]
    server = codexo.AppServer(
        StringIO("".join(json.dumps(m) + "\n" for m in messages)), StringIO()
    )
    assert server.run_turn("old", "read") == "summary"


@pytest.mark.parametrize("status", ["failed", "interrupted"])
def test_unsuccessful_turn_is_not_accepted(status: str) -> None:
    messages = [
        {"id": 1, "result": {"turn": {"id": "turn", "status": "inProgress"}}},
        {
            "method": "turn/completed",
            "params": {
                "threadId": "old",
                "turn": {"id": "turn", "status": status},
            },
        },
    ]
    server = codexo.AppServer(
        StringIO("".join(json.dumps(m) + "\n" for m in messages)), StringIO()
    )
    with pytest.raises(RuntimeError, match=status):
        server.run_turn("old", "write")


def test_client_request_fails_instead_of_waiting_forever() -> None:
    writer = StringIO()
    server = codexo.AppServer(
        StringIO(
            json.dumps(
                {"id": "approval", "method": "item/commandExecution/requestApproval"}
            )
            + "\n"
        ),
        writer,
    )
    with pytest.raises(RuntimeError, match="cannot provide approvals"):
        server.request("turn/start", {})
    assert json.loads(writer.getvalue().splitlines()[-1])["error"]["code"] == -32601


@pytest.mark.parametrize(
    "failure",
    [
        None,
        "missing",
        "empty",
        "failed_read",
        "wrong_cwd",
        "busy",
        "pin_failed",
        "unchanged_handover",
    ],
)
@pytest.mark.parametrize("pinned", [False, True])
@pytest.mark.parametrize("retire_prompt", [False, True])
def test_old_chat_is_archived_only_after_handover_and_summary_succeed(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    failure: str | None,
    pinned: bool,
    retire_prompt: bool,
) -> None:
    archived: list[str] = []
    started: list[dict[str, object]] = []
    named: list[dict[str, object]] = []
    turns: list[str] = []
    expected_turns = ["old", "new"] if retire_prompt else ["new"]
    pins = ["first", "old", "last"] if pinned else ["first", "last"]
    handover = tmp_path / "handover.md"
    if failure == "unchanged_handover" or (not retire_prompt and failure != "missing"):
        handover.write_text("" if failure == "empty" else "Existing handover.")

    def request(method: str, params: dict[str, object]) -> dict[str, object]:
        if method in {"thread/read", "thread/resume"}:
            if method == "thread/resume":
                assert retire_prompt
            return {
                "thread": {
                    "id": "old",
                    "name": "Original name",
                    "cwd": str(tmp_path),
                    "status": {"type": "active" if failure == "busy" else "idle"},
                    "section": {"id": codexo.PINNED_SECTION_ID} if pinned else None,
                }
            }
        if method == "thread/start":
            started.append(params)
            assert turns == expected_turns[:-1] and handover.is_file()
            return {
                "thread": {
                    "id": "new",
                    "cwd": str(
                        tmp_path / "other" if failure == "wrong_cwd" else tmp_path
                    ),
                    "status": {"type": "idle"},
                }
            }
        if method == "thread/name/set":
            named.append(params)
            return {}
        if method == "thread/section/move":
            assert turns == expected_turns
            assert pinned
            if failure == "pin_failed":
                raise RuntimeError("Pin move failed")
            pins.insert(
                pins.index(str(params["beforeThreadId"])), str(params["threadId"])
            )
            return {}
        assert method == "thread/archive"
        assert turns == expected_turns
        archived.append(str(params["threadId"]))
        if pinned:
            pins.remove("old")
        return {}

    def run_turn(thread_id: str, prompt: str) -> str:
        turns.append(thread_id)
        assert handover.name in prompt
        if thread_id == "old":
            assert retire_prompt
            if failure not in {"missing", "unchanged_handover"}:
                handover.write_text(
                    "" if failure == "empty" else "Hand over this objective."
                )
            return "saved"
        if failure == "failed_read":
            raise RuntimeError("Read turn failed")
        return "The next task is to continue the objective."

    server = codexo.AppServer(StringIO(), StringIO())
    monkeypatch.setattr(server, "request", request)
    monkeypatch.setattr(server, "run_turn", run_turn)
    if (
        failure is None
        or (failure == "pin_failed" and not pinned)
        or (failure == "unchanged_handover" and not retire_prompt)
    ):
        assert codexo.replace_chat(server, "old", retire_prompt) == "new"
        assert turns == expected_turns
        assert archived == ["old"]
        assert started == [{"cwd": str(tmp_path)}]
        assert named == [{"threadId": "new", "name": "Original name"}]
        assert pins == (["first", "new", "last"] if pinned else ["first", "last"])
    else:
        with pytest.raises((RuntimeError, codexo.ValidationError)):
            codexo.replace_chat(server, "old", retire_prompt)
        assert not archived
        assert pins == (["first", "old", "last"] if pinned else ["first", "last"])
