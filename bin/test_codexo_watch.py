import importlib.util
import plistlib
import subprocess
import sys
from contextlib import nullcontext
from datetime import datetime
from io import StringIO
from pathlib import Path
from typing import Never

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
        "chat": True,
        "resume_active": False,
        "latest_resume_active": False,
        "latest_busy": False,
        "test_chat_names": ["codexo"],
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
        if method == "thread/backgroundTerminals/list":
            pytest.fail("Background servers must not affect chat selection")
        if method == "thread/read":
            chat["id"] = params["threadId"]
            if params["threadId"] != "latest":
                chat["name"] = "codexo"
            if state["busy"] or (
                state["latest_busy"] and params["threadId"] == "latest"
            ):
                chat["status"] = {
                    "type": "active",
                    "activeFlags": ["waitingOnApproval"],
                }
            return {"thread": chat}
        if method == "thread/list":
            assert params["sortKey"] == "updated_at"
            assert params["archived"] is False
            assert params["limit"] == 100
            if params["cursor"] is None:
                return {
                    "data": [chat] if state["chat"] else [],
                    "nextCursor": "test-chats",
                }
            assert params["cursor"] == "test-chats"
            names = state["test_chat_names"]
            assert isinstance(names, list)
            return {
                "data": [
                    dict(chat, id=f"test-{i}", name=n) for i, n in enumerate(names)
                ]
                if state["chat"]
                else []
            }
        if method == "thread/resume":
            assert params["threadId"] in ["latest", "test-0"]
            if params["threadId"] == "test-0":
                chat.update(id="test-0", name="codexo")
            if state["resume_active"] or (
                state["latest_resume_active"] and params["threadId"] == "latest"
            ):
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
    history = codexo.WatchState()
    poll = codexo.WatchPoll()
    assert codexo.watch_iteration(server, history, "custom prompt", poll) is True
    assert poll.primary == primary
    assert poll.secondary == secondary
    assert poll.idle is True
    assert poll.prompt_sent is True
    assert len(sent) == 1
    assert sent[0]["threadId"] == "latest"
    assert sent[0]["input"] == [{"type": "text", "text": "custom prompt"}]
    assert (
        codexo.watch_iteration(server, history, "custom prompt", codexo.WatchPoll())
        is False
    )
    assert len(sent) == 1
    assert history.previous["primary"] == primary
    assert history.previous["secondary"] == secondary


def test_windows_are_tracked_independently_and_simultaneous_resets_send_once(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
) -> None:
    server, state, sent = watch_context
    history = codexo.WatchState()
    state.update(primary=20, secondary=80)
    assert codexo.watch_iteration(server, history, "hello", codexo.WatchPoll()) is False
    state["primary"] = 0
    assert codexo.watch_iteration(server, history, "hello", codexo.WatchPoll()) is True
    state["secondary"] = 0
    assert codexo.watch_iteration(server, history, "hello", codexo.WatchPoll()) is True
    state.update(primary=10, secondary=10)
    assert codexo.watch_iteration(server, history, "hello", codexo.WatchPoll()) is False
    state.update(primary=0, secondary=0)
    assert codexo.watch_iteration(server, history, "hello", codexo.WatchPoll()) is True
    assert len(sent) == 3


@pytest.mark.parametrize("blocker", ["busy", "resume_active"])
@pytest.mark.parametrize("test_mode", [False, True])
def test_blocked_reset_sends_once_when_idle_even_if_usage_increases(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    blocker: str,
    test_mode: bool,
) -> None:
    server, state, sent = watch_context
    state[blocker] = True
    history = codexo.WatchState()
    poll = codexo.WatchPoll()
    assert codexo.watch_iteration(server, history, "hello", poll, test_mode) is False
    assert poll.idle is False
    assert "Latest chat" in poll.detail
    assert "active" in poll.detail
    assert history.pending_reset is True
    assert not sent
    state[blocker] = False
    state.update(primary=5, secondary=10)
    poll = codexo.WatchPoll()
    assert codexo.watch_iteration(server, history, "hello", poll, test_mode) is True
    assert poll.idle is True
    assert poll.prompt_sent is True
    assert history.pending_reset is False
    assert len(sent) == 1
    assert sent[0]["input"] == [{"type": "text", "text": "hello"}]
    assert codexo.watch_iteration(server, history, "hello", codexo.WatchPoll()) is False
    assert len(sent) == 1


@pytest.mark.parametrize("blocker", ["latest_busy", "latest_resume_active"])
def test_reset_skips_busy_latest_chat_and_sends_to_idle_chat_on_next_page(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    blocker: str,
) -> None:
    server, state, sent = watch_context
    state[blocker] = True
    history = codexo.WatchState()
    poll = codexo.WatchPoll()
    assert codexo.watch_iteration(server, history, "hello", poll) is True
    assert sent[0]["threadId"] == "test-0"
    assert poll.idle is True
    assert "skipped busy chats: latest (Latest chat)" in poll.detail
    assert history.pending_reset is False


def test_test_prompt_ignores_activity_in_other_chats(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
) -> None:
    server, state, sent = watch_context
    state.update(primary=20, secondary=80, latest_busy=True)
    assert (
        codexo.watch_iteration(
            server, codexo.WatchState(), "hello", codexo.WatchPoll(), True
        )
        is True
    )
    assert sent[0]["threadId"] == "test-0"
    assert sent[0]["input"] == [{"type": "text", "text": 'Say "test"'}]


def test_pending_reset_survives_poll_errors_and_no_destination(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    server, state, sent = watch_context
    history = codexo.WatchState()
    assert isinstance(server, codexo.AppServer)
    request = server.request

    def fail_send(method: str, params: dict[str, object]) -> dict[str, object]:
        if method == "turn/start":
            raise RuntimeError("send failed")
        return request(method, params)

    monkeypatch.setattr(server, "request", fail_send)
    with pytest.raises(RuntimeError, match="send failed"):
        codexo.watch_iteration(server, history, "hello", codexo.WatchPoll())
    assert history.pending_reset is True
    monkeypatch.setattr(server, "request", request)
    state.update(primary=None, secondary=None, chat=False)
    assert codexo.watch_iteration(server, history, "hello", codexo.WatchPoll()) is False
    assert history.pending_reset is True
    state["chat"] = True
    assert codexo.watch_iteration(server, history, "hello", codexo.WatchPoll()) is True
    assert history.pending_reset is False
    assert len(sent) == 1


@pytest.mark.parametrize("missing", ["windows", "chat"])
@pytest.mark.parametrize("test_mode", [False, True])
def test_missing_usage_only_allows_test_prompts_and_missing_chat_never_sends(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    missing: str,
    test_mode: bool,
) -> None:
    server, state, sent = watch_context
    if missing == "windows":
        state.update(primary=None, secondary=None)
    else:
        state["chat"] = False
    should_send = test_mode and missing == "windows"
    assert (
        codexo.watch_iteration(
            server, codexo.WatchState(), "hello", codexo.WatchPoll(), test_mode
        )
        is should_send
    )
    assert len(sent) == int(should_send)


@pytest.mark.parametrize("interval", [60, 12])
def test_test_mode_sends_each_poll_and_preserves_reset_prompt(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    interval: int,
) -> None:
    server, state, sent = watch_context
    state.update(primary=20, secondary=80)
    monkeypatch.setattr(codexo, "desktop_running", lambda: True)
    monkeypatch.setattr(codexo.Path, "is_socket", lambda p: True)
    monkeypatch.setattr(codexo, "desktop_server", lambda *a, **k: nullcontext(server))
    waits: list[float] = []

    def wait(seconds: float) -> None:
        waits.append(seconds)
        if len(waits) == 2:
            state.update(primary=0, secondary=0)
        if len(waits) == 4:
            raise KeyboardInterrupt

    monkeypatch.setattr(codexo, "sleep", wait)
    with pytest.raises(KeyboardInterrupt):
        codexo.watch(
            codexo.WatchCommand(test=True, interval=interval, prompt="reset prompt"),
            False,
        )
    assert waits == [interval] * 4
    assert [s["input"] for s in sent] == [
        [{"type": "text", "text": p}]
        for p in ['Say "test"', 'Say "test"', "reset prompt", 'Say "test"']
    ]
    assert [s["threadId"] for s in sent] == ["test-0", "test-0", "latest", "test-0"]
    lines = capsys.readouterr().out.splitlines()
    assert len(lines) == 4
    assert all("prompt_sent=true" in s for s in lines)
    assert "test prompt sent to test-0 (codexo)" in lines[0]
    assert "test prompt" not in lines[2]


@pytest.mark.parametrize(
    "names,detail",
    [
        ([], "no unarchived chat titled codexo"),
        (["Codexo", "Other chat"], "no unarchived chat titled codexo"),
        (["codexo", "codexo"], "multiple unarchived chats titled codexo"),
    ],
)
def test_test_prompts_skip_missing_or_ambiguous_codexo_chat(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    names: list[str],
    detail: str,
) -> None:
    server, state, sent = watch_context
    state.update(primary=20, secondary=80, test_chat_names=names)
    poll = codexo.WatchPoll()
    assert (
        codexo.watch_iteration(server, codexo.WatchState(), "hello", poll, True)
        is False
    )
    assert poll.detail == detail
    assert poll.idle is None
    assert not sent


@pytest.mark.parametrize("running", [False, True])
def test_watch_without_app_or_desktop_bridge_only_waits(
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
        if running:
            raise RuntimeError("Desktop bridge unavailable")
        pytest.fail("Watch must not connect when Codex is not running")

    monkeypatch.setattr(codexo, "sleep", wait)
    monkeypatch.setattr(codexo, "desktop_server", unexpected_connection)
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
        "Desktop bridge unavailable" if running else "Codex is not running"
    ) in output.out
    assert output.err == ""


def test_watch_uses_the_desktop_server(
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
        assert not args and not kwargs
        return nullcontext(server)

    def wait(interval: float) -> None:
        raise KeyboardInterrupt

    monkeypatch.setattr(codexo, "desktop_server", connect)
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
    monkeypatch.setattr(codexo, "desktop_server", lambda *a, **k: nullcontext(server))
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


@pytest.mark.parametrize("failure", ["connection", "usage", "idle", "timeout"])
def test_poll_errors_log_only_known_values_and_stay_on_one_line(
    watch_context: tuple[object, dict[str, object], list[dict[str, object]]],
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    failure: str,
) -> None:
    server, _, sent = watch_context
    monkeypatch.setattr(codexo, "desktop_running", lambda: True)
    monkeypatch.setattr(codexo.Path, "is_socket", lambda p: True)
    monkeypatch.setattr(codexo, "desktop_server", lambda *a, **k: nullcontext(server))

    def unavailable(*args: object, **kwargs: object) -> Never:
        if failure == "timeout":
            raise TimeoutError("initialize: request timed out")
        raise RuntimeError("query failed\nwith details")

    if failure in {"connection", "timeout"}:
        monkeypatch.setattr(codexo, "desktop_server", unavailable)
    elif failure == "usage":
        monkeypatch.setattr(server, "request", unavailable)
    else:
        assert isinstance(server, codexo.AppServer)
        request = server.request

        def fail_idle(method: str, params: dict[str, object]) -> dict[str, object]:
            if method == "thread/read":
                return unavailable()
            return request(method, params)

        monkeypatch.setattr(server, "request", fail_idle)

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
    assert (
        "error: initialize: request timed out"
        if failure == "timeout"
        else "error: query failed\\nwith details"
    ) in output.out
    assert output.err == ""
    assert not sent


@pytest.mark.parametrize(
    "operation", [[], ["install"], ["uninstall"], ["restart"], ["daemon"]]
)
@pytest.mark.parametrize("test_mode", [False, True])
def test_watch_dry_run_never_starts_processes_or_writes_files(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    operation: list[str],
    test_mode: bool,
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
            *(["--test"] if test_mode else []),
        ],
    )

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Dry-run must not start processes or write files")

    monkeypatch.setattr(codexo.subprocess, "run", unexpected_operation)
    monkeypatch.setattr(codexo.subprocess, "Popen", unexpected_operation)
    monkeypatch.setattr(codexo.Path, "write_bytes", unexpected_operation)
    monkeypatch.setattr(codexo.Path, "write_text", unexpected_operation)
    codexo.main()
    output = capsys.readouterr().out
    assert output
    if test_mode and not operation:
        assert 'Say "test"' in output
    if operation == ["install"]:
        assert "launchctl setenv CODEX_CLI_PATH" in output
        assert "local.codexo.watch-environment.plist" in output
        assert "if loaded" in output
    elif operation == ["uninstall"]:
        assert "launchctl unsetenv CODEX_APP_SERVER_USE_LOCAL_DAEMON" in output
        assert "local.codexo.watch-environment.plist" in output
    assert not (tmp_path / "Library").exists()


@pytest.mark.parametrize("available", [False, True])
def test_daemon_restart_uses_existing_service_and_reports_failure(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    available: bool,
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    monkeypatch.setattr(sys, "argv", ["codexo.py", "watch", "restart"])
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


@pytest.mark.parametrize(
    "loaded,present", [(False, False), (False, True), (True, True), (True, False)]
)
@pytest.mark.parametrize("test_mode", [False, True])
def test_daemon_install_replaces_settings_and_reloads_existing_agents(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    loaded: bool,
    present: bool,
    test_mode: bool,
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    calls: list[list[str]] = []
    path = tmp_path / "Library/LaunchAgents/local.codexo.watch.plist"
    helper = path.with_name("local.codexo.watch-environment.plist")
    labels = ["local.codexo.watch-environment", "local.codexo.watch"]
    registered = set(labels) if loaded else set()
    unloading: dict[str, int] = {}
    monkeypatch.setattr(codexo, "sleep", lambda seconds: None)
    if present:
        path.parent.mkdir(parents=True)
        for p in [path, helper]:
            p.write_bytes(b"old settings")

    def run(args: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
        calls.append(args)
        if args[1] == "print":
            label = args[-1].rsplit("/", 1)[-1]
            if label in unloading:
                unloading[label] -= 1
                if unloading[label] == 0:
                    registered.remove(label)
                    del unloading[label]
            return subprocess.CompletedProcess(
                args, 0 if label in registered else 1, ""
            )
        if args[1] == "bootout":
            unloading[args[-1].rsplit("/", 1)[-1]] = 2
        elif args[1] == "bootstrap":
            label = plistlib.loads(Path(args[-1]).read_bytes())["Label"]
            assert label not in registered
            registered.add(label)
        return subprocess.CompletedProcess(args, 0, "")

    monkeypatch.setattr(codexo.subprocess, "run", run)
    command = codexo.WatchCommand(
        action=codexo.WatchInstallCommand(),
        interval=123,
        prompt='Hello "there" 📦',
        test=test_mode,
    )
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "codexo.py",
            "watch",
            "install",
            "--interval",
            "123",
            "--prompt",
            command.prompt,
            *(["--test"] if test_mode else []),
        ],
    )
    codexo.main()
    document = plistlib.loads(path.read_bytes())
    args = document["ProgramArguments"]
    assert args[0] == sys.executable
    assert args[2] == "watch"
    assert args[args.index("--prompt") + 1] == command.prompt
    assert ("--test" in args) is test_mode
    assert args[args.index("--interval") + 1] == "123.0"
    assert "install" not in args
    assert document["KeepAlive"] is True
    environment = plistlib.loads(helper.read_bytes())
    assert environment == {
        "Label": "local.codexo.watch-environment",
        "RunAtLoad": True,
        "ProgramArguments": [
            "/bin/launchctl",
            "setenv",
            "CODEX_CLI_PATH",
            str(tmp_path / "Library/Application Support/codexo/codex"),
        ],
    }
    assert calls[0] == [
        "/bin/launchctl",
        "unsetenv",
        "CODEX_APP_SERVER_USE_LOCAL_DAEMON",
    ]
    assert calls[1] == environment["ProgramArguments"]
    launcher = tmp_path / "Library/Application Support/codexo/codex"
    assert 'bridge -- "$@"' in launcher.read_text()
    assert launcher.stat().st_mode & 0o111
    assert len([c for c in calls if c[1] == "bootout"]) == (2 if loaded else 0)
    assert [c[-1] for c in calls if c[1] == "bootstrap"] == [str(helper), str(path)]
    calls.clear()
    updated = command.model_copy(
        update={"interval": 45.0, "prompt": "new prompt", "test": False}
    )
    codexo.watch_daemon(updated, False)
    args = plistlib.loads(path.read_bytes())["ProgramArguments"]
    assert args[args.index("--interval") + 1] == "45.0"
    assert args[-1] == "new prompt"
    assert "--test" not in args
    assert len([c for c in calls if c[1] == "bootout"]) == 2
    assert [c[-1] for c in calls if c[1] == "bootstrap"] == [str(helper), str(path)]
    assert not any(c[1:3] == ["unsetenv", "CODEX_CLI_PATH"] for c in calls)
    assert registered == set(labels)


@pytest.mark.parametrize(
    "bridge_state", ["ready", "missing-socket", "unavailable", "inspection-failed"]
)
def test_install_only_requests_an_app_restart_when_bridge_is_not_ready(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    bridge_state: str,
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    monkeypatch.setattr(
        codexo.subprocess,
        "run",
        lambda a, **k: subprocess.CompletedProcess(a, 1 if a[1] == "print" else 0, ""),
    )

    def desktop_socket() -> Path:
        if bridge_state == "unavailable":
            raise RuntimeError("Desktop bridge unavailable")
        if bridge_state == "inspection-failed":
            raise subprocess.CalledProcessError(1, ["ps"])
        return tmp_path / "app.sock"

    monkeypatch.setattr(codexo, "desktop_socket", desktop_socket)
    monkeypatch.setattr(codexo.Path, "is_socket", lambda p: bridge_state == "ready")
    codexo.watch_daemon(codexo.WatchCommand(action=codexo.WatchInstallCommand()), False)
    output = capsys.readouterr().out
    assert "Installed codexo watch:" in output
    assert ("Quit and reopen" in output) is (bridge_state != "ready")


def test_unload_times_out_if_launchd_keeps_the_registration(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    calls: list[list[str]] = []

    def run(args: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
        calls.append(args)
        return subprocess.CompletedProcess(args, 0, "")

    clock = iter([0, 11])
    monkeypatch.setattr(codexo, "monotonic", lambda: next(clock))
    monkeypatch.setattr(codexo.subprocess, "run", run)
    with pytest.raises(
        TimeoutError, match="waiting for launchd to unload gui/501/local.codexo.watch"
    ):
        codexo.unload_launch_agent("gui/501/local.codexo.watch")
    assert [c[1] for c in calls] == ["bootout", "print"]


@pytest.mark.parametrize(
    "labels",
    [
        ["local.codexo.watch"],
        ["local.codexo.watch-environment"],
        ["local.codexo.watch", "local.codexo.watch-environment"],
    ],
)
@pytest.mark.parametrize("loaded,present", [(False, True), (True, True), (True, False)])
def test_daemon_uninstall_removes_registration_and_file_if_present(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    loaded: bool,
    present: bool,
    labels: list[str],
) -> None:
    monkeypatch.setattr(codexo.Path, "home", lambda: tmp_path)
    directory = tmp_path / "Library/LaunchAgents"
    launcher = tmp_path / "Library/Application Support/codexo/codex"
    launcher.parent.mkdir(parents=True)
    launcher.write_text("launcher")
    if present:
        directory.mkdir(parents=True)
        for label in labels:
            (directory / f"{label}.plist").touch()
    calls: list[list[str]] = []
    registered = set(labels) if loaded else set()

    def run(args: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
        calls.append(args)
        if args[1] == "unsetenv":
            return subprocess.CompletedProcess(args, 0, "")
        label = args[-1].rsplit("/", 1)[-1]
        if args[1] == "bootout":
            registered.remove(label)
            return subprocess.CompletedProcess(args, 0, "")
        return subprocess.CompletedProcess(args, 0 if label in registered else 1, "")

    monkeypatch.setattr(codexo.subprocess, "run", run)
    codexo.watch_daemon(
        codexo.WatchCommand(action=codexo.WatchUninstallCommand()), False
    )
    assert not list(directory.glob("*.plist"))
    assert not launcher.exists()
    assert sorted(c[-1].rsplit("/", 1)[-1] for c in calls if c[1] == "bootout") == (
        sorted(labels) if loaded else []
    )
    assert calls[-1] == ["/bin/launchctl", "unsetenv", "CODEX_CLI_PATH"]
    with pytest.raises(RuntimeError, match="not installed"):
        codexo.watch_daemon(
            codexo.WatchCommand(action=codexo.WatchUninstallCommand()), False
        )


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
    codexo.watch_daemon(codexo.WatchCommand(action=codexo.WatchDaemonCommand()), False)
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
    ],
)
def test_invalid_watch_configuration_is_rejected(options: dict[str, object]) -> None:
    with pytest.raises(codexo.ValidationError):
        codexo.WatchCommand(**options)


@pytest.mark.parametrize(
    "arguments",
    [
        ["--install"],
        ["--uninstall"],
        ["--restart"],
        ["--daemon"],
        ["install", "restart"],
        ["uninstall", "daemon"],
    ],
)
def test_watch_rejects_old_flags_and_multiple_subcommands(
    monkeypatch: pytest.MonkeyPatch, arguments: list[str]
) -> None:
    monkeypatch.setattr(sys, "argv", ["codexo.py", "watch", *arguments])

    def unexpected_operation(*args: object, **kwargs: object) -> None:
        pytest.fail("Invalid arguments must not start processes")

    monkeypatch.setattr(codexo.subprocess, "run", unexpected_operation)
    with pytest.raises(SystemExit) as error:
        codexo.main()
    assert error.value.code == 2
