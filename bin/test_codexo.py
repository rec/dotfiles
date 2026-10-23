import hashlib
import importlib.util
import json
import sys
from io import StringIO
from pathlib import Path

import pytest
from pytest_regressions.file_regression import FileRegressionFixture

spec = importlib.util.spec_from_file_location(
    "codexo", Path(__file__).with_name("codexo.py")
)
assert spec is not None and spec.loader is not None
codexo = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = codexo
spec.loader.exec_module(codexo)


@pytest.mark.parametrize("flag", ["-d", "--dry-run"])
@pytest.mark.parametrize("thread_id", [None, "old-chat-id"])
def test_dry_run_prints_operations_without_performing_them(
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    flag: str,
    thread_id: str | None,
) -> None:
    args = ["codexo.py", flag, "retire"]
    if thread_id is not None:
        args.append(thread_id)
    monkeypatch.setattr(sys, "argv", args)

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Dry-run must not start app-server or create files")

    monkeypatch.setattr(codexo.subprocess, "Popen", unexpected_operation)
    monkeypatch.setattr(codexo, "mkdtemp", unexpected_operation)
    codexo.main()
    output = capsys.readouterr().out
    assert "no app-server, file, or chat operations" in output
    assert ("thread/list:" in output) == (thread_id is None)
    assert (thread_id or "<selected-chat-id>") in output
    for operation in [
        "initialize",
        "initialized",
        "thread/read",
        "thread/resume",
        "handover.md",
        "turn/start",
        "turn/completed",
        "thread/start",
        "thread/name/set",
        "matching checksum",
        "thread/archive",
        "stop app-server",
    ]:
        assert operation in output


@pytest.mark.parametrize("flag", ["-d", "--dry-run"])
def test_fix_dry_run_lists_repositories_without_reading_them(
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    flag: str,
) -> None:
    monkeypatch.setattr(sys, "argv", ["codexo.py", flag, "fix", "safer", "tuney"])

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Dry-run must not start app-server or read repositories")

    monkeypatch.setattr(codexo.subprocess, "Popen", unexpected_operation)
    monkeypatch.setattr(codexo, "mkdtemp", unexpected_operation)
    monkeypatch.setattr(codexo, "report_repository", unexpected_operation)
    codexo.main()
    output = capsys.readouterr()
    assert "safer:" in output.out
    assert "tuney:" in output.out
    assert "project/list" in output.out
    assert "thread/list" in output.out
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
    monkeypatch.setattr(codexo, "codex_titles", lambda *a: ["safer"])
    server = codexo.AppServer(StringIO(), StringIO())
    codexo.report_repository(server, "safer")
    file_regression.check(capsys.readouterr().out)
    assert (directory / "pyproject.toml").read_text() == pyproject
    assert (directory / "README.md").read_text() == readme


@pytest.mark.parametrize("matches", [0, 1, 2])
def test_codex_titles_match_exact_roots_and_put_project_before_all_chats(
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
            codexo.codex_titles(server, tmp_path)
    else:
        titles = codexo.codex_titles(server, tmp_path)
        assert len(titles) == matches + 2
        if matches:
            assert titles[0] == "Project title"
        assert titles[-2] == "First chat"
        assert titles[-1] == "Second chat"
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
                "item": {"type": "agentMessage", "text": "receipt"},
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
    assert server.run_turn("old", "read") == "receipt"


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
        "wrong_digest",
        "refusal",
        "failed_read",
        "wrong_cwd",
        "busy",
    ],
)
def test_old_chat_is_archived_only_after_verified_handover(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, failure: str | None
) -> None:
    monkeypatch.setattr(codexo, "mkdtemp", lambda **_: str(tmp_path))
    archived: list[str] = []
    started: list[dict[str, object]] = []
    named: list[dict[str, object]] = []
    turns: list[str] = []
    handover = tmp_path / "handover.md"

    def request(method: str, params: dict[str, object]) -> dict[str, object]:
        if method in {"thread/read", "thread/resume"}:
            return {
                "thread": {
                    "id": "old",
                    "name": "Original name",
                    "cwd": str(tmp_path),
                    "status": {"type": "active" if failure == "busy" else "idle"},
                }
            }
        if method == "thread/start":
            started.append(params)
            assert turns == ["old"] and handover.is_file()
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
        assert method == "thread/archive"
        assert turns == ["old", "new"]
        archived.append(str(params["threadId"]))
        return {}

    def run_turn(thread_id: str, prompt: str, receipt: bool = False) -> str:
        turns.append(thread_id)
        assert str(handover) in prompt
        if thread_id == "old":
            assert not receipt
            if failure != "missing":
                handover.write_text(
                    "" if failure == "empty" else "Hand over this objective."
                )
            return "saved"
        assert receipt
        if failure == "failed_read":
            raise RuntimeError("Read turn failed")
        if failure == "refusal":
            return "Unable to read the file"
        digest = hashlib.sha256(handover.read_bytes()).hexdigest()
        return json.dumps({"sha256": "wrong" if failure == "wrong_digest" else digest})

    server = codexo.AppServer(StringIO(), StringIO())
    monkeypatch.setattr(server, "request", request)
    monkeypatch.setattr(server, "run_turn", run_turn)
    if failure is None:
        assert codexo.replace_chat(server, "old") == "new"
        assert archived == ["old"]
        assert started == [{"cwd": str(tmp_path)}]
        assert named == [{"threadId": "new", "name": "Original name"}]
    else:
        with pytest.raises((RuntimeError, codexo.ValidationError)):
            codexo.replace_chat(server, "old")
        assert not archived
