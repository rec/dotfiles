import importlib.util
import plistlib
import subprocess
import sys
from contextlib import nullcontext
from datetime import datetime
from io import StringIO
from pathlib import Path

import pytest

spec = importlib.util.spec_from_file_location(
    "codexo_watch", Path(__file__).with_name("codexo.py")
)
assert spec is not None and spec.loader is not None
codexo = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = codexo
spec.loader.exec_module(codexo)


@pytest.fixture
def watch_context(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> tuple[object, dict[str, object], list[dict[str, object]]]:
    server = codexo.AppServer(StringIO(), StringIO())
    state: dict[str, object] = {
        "primary": 0,
        "secondary": 0,
        "busy": False,
        "background": False,
        "chat": True,
        "resume_active": False,
        "second_page_busy": False,
    }
    sent: list[dict[str, object]] = []

    def request(method: str, params: dict[str, object]) -> dict[str, object]:
        chat = {
            "id": "latest",
            "name": "Latest chat",
            "cwd": str(tmp_path),
            "status": {"type": "idle"},
        }
        if method == "account/rateLimits/read":
            return {
                "rateLimits": {
                    n: None if state[n] is None else {"usedPercent": state[n]}
                    for n in ["primary", "secondary"]
                }
            }
        if method == "thread/loaded/list":
            if params["cursor"] == "next":
                return {"data": ["busy-on-second-page"]}
            return {
                "data": ["loaded"],
                "nextCursor": "next" if state["second_page_busy"] else None,
            }
        if method == "thread/read":
            if state["busy"] or params["threadId"] == "busy-on-second-page":
                chat["status"] = {
                    "type": "active",
                    "activeFlags": ["waitingOnApproval"],
                }
            return {"thread": chat}
        if method == "thread/backgroundTerminals/list":
            return {"data": ["terminal"] if state["background"] else []}
        if method == "thread/list":
            assert params["sortKey"] == "updated_at"
            assert params["archived"] is False
            assert params["limit"] == 1
            return {"data": [chat] if state["chat"] else []}
        if method == "thread/resume":
            assert params["threadId"] == "latest"
            if state["resume_active"]:
                chat["status"] = {"type": "active"}
            return {"thread": chat}
        assert method == "turn/start"
        sent.append(params)
        return {"turn": {"id": "turn", "status": "inProgress"}}

    monkeypatch.setattr(server, "request", request)
    return server, state, sent


@pytest.mark.parametrize("primary,secondary", [(0, 70), (70, 0), (0, 0)])
def test_initial_zero_in_either_window_sends_only_one_prompt(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    primary: int,
    secondary: int,
) -> None:
    server, state, sent = watch_context
    state.update(primary=primary, secondary=secondary)
    previous: dict[str, int] = {}
    poll = codexo.WatchPoll()
    assert codexo.watch_iteration(server, previous, "custom prompt", poll) is True
    assert poll.primary == primary
    assert poll.secondary == secondary
    assert poll.idle is True
    assert poll.prompt_sent is True
    assert len(sent) == 1
    assert sent[0]["threadId"] == "latest"
    assert sent[0]["input"] == [{"type": "text", "text": "custom prompt"}]
    assert (
        codexo.watch_iteration(server, previous, "custom prompt", codexo.WatchPoll())
        is False
    )
    assert len(sent) == 1
    assert previous["primary"] == primary
    assert previous["secondary"] == secondary


def test_windows_are_tracked_independently_and_simultaneous_resets_send_once(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
) -> None:
    server, state, sent = watch_context
    previous: dict[str, int] = {}
    state.update(primary=20, secondary=80)
    assert (
        codexo.watch_iteration(server, previous, "hello", codexo.WatchPoll()) is False
    )
    state["primary"] = 0
    assert codexo.watch_iteration(server, previous, "hello", codexo.WatchPoll()) is True
    state["secondary"] = 0
    assert codexo.watch_iteration(server, previous, "hello", codexo.WatchPoll()) is True
    state.update(primary=10, secondary=10)
    assert (
        codexo.watch_iteration(server, previous, "hello", codexo.WatchPoll()) is False
    )
    state.update(primary=0, secondary=0)
    assert codexo.watch_iteration(server, previous, "hello", codexo.WatchPoll()) is True
    assert len(sent) == 3


@pytest.mark.parametrize(
    "blocker", ["busy", "background", "second_page_busy", "resume_active"]
)
def test_any_chat_activity_prevents_prompt_and_zero_is_still_recorded(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    blocker: str,
) -> None:
    server, state, sent = watch_context
    state[blocker] = True
    previous: dict[str, int] = {}
    poll = codexo.WatchPoll()
    assert codexo.watch_iteration(server, previous, "hello", poll) is False
    assert poll.idle is False
    assert not sent
    state[blocker] = False
    poll = codexo.WatchPoll()
    assert codexo.watch_iteration(server, previous, "hello", poll) is False
    assert poll.idle is True
    assert poll.prompt_sent is False
    assert not sent


@pytest.mark.parametrize("missing", ["windows", "chat"])
def test_missing_usage_or_no_unarchived_chat_does_not_send(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    missing: str,
) -> None:
    server, state, sent = watch_context
    if missing == "windows":
        state.update(primary=None, secondary=None)
    else:
        state["chat"] = False
    assert codexo.watch_iteration(server, {}, "hello", codexo.WatchPoll()) is False
    assert not sent


@pytest.mark.parametrize("running", [False, True])
def test_watch_without_app_or_shared_socket_only_waits(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    running: bool,
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    monkeypatch.delenv("CODEX_HOME", raising=False)
    monkeypatch.setattr(codexo, "desktop_running", lambda: running)
    waits: list[float] = []

    def wait(interval: float) -> None:
        waits.append(interval)
        raise KeyboardInterrupt

    def unexpected_connection(*args: object, **kwargs: object) -> None:
        pytest.fail("Watch must not launch a new app-server")

    monkeypatch.setattr(codexo, "sleep", wait)
    monkeypatch.setattr(codexo, "app_server", unexpected_connection)
    with pytest.raises(KeyboardInterrupt):
        codexo.watch(codexo.WatchCommand(), False)
    assert waits == [60]
    output = capsys.readouterr()
    assert len(output.out.splitlines()) == 1
    assert (
        "primary.usedPercent=unavailable secondary.usedPercent=unavailable"
        in output.out
    )
    assert "idle=unknown prompt_sent=false" in output.out
    assert (
        "shared server socket unavailable" if running else "Codex is not running"
    ) in output.out
    assert output.err == ""


def test_watch_uses_the_running_server_proxy(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    server, _, sent = watch_context
    monkeypatch.setattr(codexo, "desktop_running", lambda: True)
    monkeypatch.setattr(codexo.Path, "is_socket", lambda p: True)
    monkeypatch.setenv("CODEX_HOME", str(tmp_path))

    def connect(*args: object, **kwargs: object) -> nullcontext[object]:
        assert (
            kwargs["proxy_socket"]
            == tmp_path / "app-server-control/app-server-control.sock"
        )
        return nullcontext(server)

    def wait(interval: float) -> None:
        raise KeyboardInterrupt

    monkeypatch.setattr(codexo, "app_server", connect)
    monkeypatch.setattr(codexo, "sleep", wait)
    with pytest.raises(KeyboardInterrupt):
        codexo.watch(codexo.WatchCommand(), False)
    assert len(sent) == 1
    output = capsys.readouterr().out
    assert len(output.splitlines()) == 1
    assert (
        "primary.usedPercent=0 secondary.usedPercent=0 idle=true prompt_sent=true"
        in output
    )
    assert "sent to latest" in output


def test_every_poll_logs_timestamp_usage_and_current_idle_state_without_reset(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    server, state, sent = watch_context
    state.update(primary=20, secondary=80, busy=True)
    monkeypatch.setattr(codexo, "desktop_running", lambda: True)
    monkeypatch.setattr(codexo.Path, "is_socket", lambda p: True)
    monkeypatch.setattr(codexo, "app_server", lambda *a, **k: nullcontext(server))
    waits: list[float] = []

    def wait(interval: float) -> None:
        waits.append(interval)
        state["busy"] = False
        if len(waits) == 2:
            raise KeyboardInterrupt

    monkeypatch.setattr(codexo, "sleep", wait)
    with pytest.raises(KeyboardInterrupt):
        codexo.watch(codexo.WatchCommand(), False)
    output = capsys.readouterr().out.splitlines()
    assert len(output) == 2
    for line in output:
        assert datetime.fromisoformat(line.split()[0]).tzinfo is not None
        assert "primary.usedPercent=20 secondary.usedPercent=80" in line
        assert "prompt_sent=false" in line
    assert "idle=false" in output[0]
    assert "idle=true" in output[1]
    assert not sent


@pytest.mark.parametrize("failure", ["connection", "usage", "idle"])
def test_poll_errors_log_only_known_values_and_stay_on_one_line(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    failure: str,
) -> None:
    server, _, sent = watch_context
    monkeypatch.setattr(codexo, "desktop_running", lambda: True)
    monkeypatch.setattr(codexo.Path, "is_socket", lambda p: True)
    monkeypatch.setattr(codexo, "app_server", lambda *a, **k: nullcontext(server))

    def unavailable(*args: object, **kwargs: object) -> None:
        raise RuntimeError("query failed\nwith details")

    if failure == "connection":
        monkeypatch.setattr(codexo, "app_server", unavailable)
    elif failure == "usage":
        monkeypatch.setattr(server, "request", unavailable)
    else:
        monkeypatch.setattr(codexo, "chats_idle", unavailable)

    def wait(interval: float) -> None:
        raise KeyboardInterrupt

    monkeypatch.setattr(codexo, "sleep", wait)
    with pytest.raises(KeyboardInterrupt):
        codexo.watch(codexo.WatchCommand(), False)
    output = capsys.readouterr()
    assert len(output.out.splitlines()) == 1
    expected = "0" if failure == "idle" else "unavailable"
    assert (
        f"primary.usedPercent={expected} secondary.usedPercent={expected}" in output.out
    )
    assert "idle=unknown prompt_sent=false" in output.out
    assert "error: query failed\\nwith details" in output.out
    assert output.err == ""
    assert not sent


@pytest.mark.parametrize(
    "operation", [[], ["--install"], ["--uninstall"], ["--restart"], ["--daemon"]]
)
def test_watch_dry_run_never_starts_processes_or_writes_files(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    operation: list[str],
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "codexo.py",
            "-d",
            "watch",
            *operation,
            "--interval",
            "12",
            "--prompt",
            "Hi there",
        ],
    )

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Dry-run must not start processes or write files")

    monkeypatch.setattr(codexo.subprocess, "run", unexpected_operation)
    monkeypatch.setattr(codexo.subprocess, "Popen", unexpected_operation)
    monkeypatch.setattr(codexo.Path, "write_bytes", unexpected_operation)
    codexo.main()
    assert capsys.readouterr().out
    assert not (tmp_path / "Library").exists()


@pytest.mark.parametrize("available", [False, True])
def test_daemon_restart_uses_existing_service_and_reports_failure(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    available: bool,
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    monkeypatch.setattr(sys, "argv", ["codexo.py", "watch", "--restart"])
    calls: list[list[str]] = []

    def run(args: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
        calls.append(args)
        assert kwargs["check"] is True
        if not available:
            raise subprocess.CalledProcessError(113, args)
        return subprocess.CompletedProcess(args, 0, "")

    monkeypatch.setattr(codexo.subprocess, "run", run)
    if available:
        codexo.main()
        assert capsys.readouterr().out == "Restarted codexo watch.\n"
    else:
        with pytest.raises(SystemExit, match="non-zero exit status 113"):
            codexo.main()
    assert len(calls) == 1
    assert calls[0][0] == "/bin/launchctl"
    assert calls[0][1] == "kickstart"
    assert calls[0][2] == "-k"
    assert calls[0][3].endswith("/local.codexo.watch")
    assert not (tmp_path / "Library").exists()


def test_daemon_install_uses_foreground_watch_with_configured_arguments(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    calls: list[list[str]] = []

    def run(args: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
        calls.append(args)
        return subprocess.CompletedProcess(args, 0, "")

    monkeypatch.setattr(codexo.subprocess, "run", run)
    command = codexo.WatchCommand(install=True, interval=123, prompt='Hello "there" 📦')
    codexo.watch_daemon(command, False)
    path = tmp_path / "Library/LaunchAgents/local.codexo.watch.plist"
    document = plistlib.loads(path.read_bytes())
    args = document["ProgramArguments"]
    assert args[0] == sys.executable
    assert args[2] == "watch"
    assert args[-1] == command.prompt
    assert args[args.index("--interval") + 1] == "123.0"
    assert "--install" not in args
    assert document["KeepAlive"] is True
    assert len(calls) == 1
    assert calls[0][1] == "bootstrap"
    with pytest.raises(RuntimeError, match="already installed"):
        codexo.watch_daemon(command, False)


@pytest.mark.parametrize("loaded,present", [(False, True), (True, True), (True, False)])
def test_daemon_uninstall_removes_registration_and_file_if_present(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    loaded: bool,
    present: bool,
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    path = tmp_path / "Library/LaunchAgents/local.codexo.watch.plist"
    if present:
        path.parent.mkdir(parents=True)
        path.touch()
    calls: list[list[str]] = []
    registered = loaded

    def run(args: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
        nonlocal registered
        calls.append(args)
        if args[1] == "bootout":
            registered = False
            return subprocess.CompletedProcess(args, 0, "")
        return subprocess.CompletedProcess(args, 0 if registered else 1, "")

    monkeypatch.setattr(codexo.subprocess, "run", run)
    codexo.watch_daemon(codexo.WatchCommand(uninstall=True), False)
    assert not path.exists()
    assert any(c[1] == "bootout" for c in calls) == loaded
    with pytest.raises(RuntimeError, match="not installed"):
        codexo.watch_daemon(codexo.WatchCommand(uninstall=True), False)


@pytest.mark.parametrize("status", ["absent", "unloaded", "running"])
def test_daemon_status_reports_loaded_and_installed_states(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    status: str,
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    if status != "absent":
        path = tmp_path / "Library/LaunchAgents/local.codexo.watch.plist"
        path.parent.mkdir(parents=True)
        path.touch()
    monkeypatch.setattr(
        codexo.subprocess,
        "run",
        lambda *a, **k: subprocess.CompletedProcess(
            [], 0 if status == "running" else 1, "state = running\n"
        ),
    )
    codexo.watch_daemon(codexo.WatchCommand(daemon=True), False)
    output = capsys.readouterr().out
    assert {
        "absent": "not installed",
        "unloaded": "installed but not loaded",
        "running": "state = running",
    }[status] in output


@pytest.mark.parametrize(
    "options",
    [
        {"interval": 0},
        {"interval": -1},
        {"install": True, "uninstall": True},
        {"daemon": True, "install": True},
        {"restart": True, "install": True},
        {"restart": True, "uninstall": True},
        {"restart": True, "daemon": True},
    ],
)
def test_invalid_watch_configuration_is_rejected(options: dict[str, object]) -> None:
    with pytest.raises(codexo.ValidationError):
        codexo.WatchCommand(**options)
