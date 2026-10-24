import importlib.util
import json
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from io import StringIO
from pathlib import Path
from socket import socketpair
from threading import Event
from unittest.mock import Mock

import pytest
from websockets.exceptions import ConnectionClosedOK
from websockets.frames import Close
from websockets.server import ServerProtocol
from websockets.sync.client import ClientConnection, connect
from websockets.sync.server import Server, ServerConnection

spec = importlib.util.spec_from_file_location(
    "codexo_bridge", Path(__file__).with_name("codexo.py")
)
assert spec is not None and spec.loader is not None
codexo = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = codexo
spec.loader.exec_module(codexo)


def test_bridge_accepts_desktop_cli_arguments_without_reinterpreting_them(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    arguments = [
        "-c",
        "features.code_mode_host=true",
        "app-server",
        "--analytics-default-enabled",
    ]
    monkeypatch.setattr(sys, "argv", ["codexo.py", "bridge", "--", *arguments])
    bridge = Mock()
    monkeypatch.setattr(codexo, "bridge_desktop", bridge)
    codexo.main()
    bridge.assert_called_once_with(arguments)


@pytest.mark.parametrize(
    "arguments,originator",
    [
        (["--version"], "Codex Desktop"),
        (["app-server", "daemon", "version"], "Codex Desktop"),
        (["app-server", "proxy"], "Codex Desktop"),
        (["app-server"], "codex_latex_preview"),
    ],
)
def test_auxiliary_cli_commands_keep_the_original_executable(
    monkeypatch: pytest.MonkeyPatch, arguments: list[str], originator: str
) -> None:
    monkeypatch.setenv("CODEX_INTERNAL_ORIGINATOR_OVERRIDE", originator)
    execute = Mock(side_effect=SystemExit(0))
    monkeypatch.setattr(codexo.os, "execv", execute)
    with pytest.raises(SystemExit):
        codexo.bridge_desktop(arguments)
    execute.assert_called_once_with(
        codexo.CODEX_EXECUTABLE, [codexo.CODEX_EXECUTABLE, *arguments]
    )


@pytest.mark.parametrize("parent,bridges", [(100, 1), (999, 1), (100, 0), (100, 2)])
def test_watcher_requires_one_bridge_owned_by_the_desktop_app(
    monkeypatch: pytest.MonkeyPatch, parent: int, bridges: int
) -> None:
    output = (
        "PID PPID COMMAND\n100 1 /Applications/ChatGPT.app/Contents/MacOS/ChatGPT\n"
    )
    script = Path(__file__).with_name("codexo.py").resolve()
    for pid in range(200, 200 + bridges):
        output += (
            f"{pid} {parent} {sys.executable} {script} bridge -- "
            "-c features.code_mode_host=true app-server\n"
        )
    monkeypatch.setattr(
        codexo.subprocess,
        "run",
        lambda *a, **k: subprocess.CompletedProcess([], 0, output),
    )
    if parent == 100 and bridges == 1:
        assert codexo.desktop_socket() == codexo.bridge_socket(200)
    else:
        with pytest.raises(RuntimeError, match="Desktop bridge unavailable"):
            codexo.desktop_socket()


def test_websocket_requests_send_json_frames_and_receive_results() -> None:
    connection = Mock(spec=ClientConnection)
    connection.recv.side_effect = [
        json.dumps({"method": "account/rateLimits/updated", "params": {}}),
        json.dumps(
            {"id": 1, "result": {"rateLimits": {"primary": {"usedPercent": 20}}}}
        ),
    ]
    server = codexo.AppServer(connection)
    assert server.request("account/rateLimits/read", {}) == {
        "rateLimits": {"primary": {"usedPercent": 20}}
    }
    assert json.loads(connection.send.call_args.args[0]) == {
        "id": 1,
        "method": "account/rateLimits/read",
        "params": {},
    }
    assert not connection.send.call_args.args[0].endswith("\n")
    assert 0 < connection.recv.call_args.kwargs["timeout"] <= 10


def test_rpc_round_trip_over_real_local_websocket_frames() -> None:
    left, right = socketpair()
    manager = Mock(spec=Server)
    manager.socket_closed = Event()

    def respond() -> None:
        peer = ServerConnection(right, ServerProtocol(), manager, close_timeout=1)
        try:
            peer.handshake(timeout=1)
            request = json.loads(peer.recv(timeout=1))
            assert request["method"] == "account/rateLimits/read"
            peer.send(json.dumps({"id": request["id"], "result": {"rateLimits": {}}}))
        finally:
            peer.close()

    with left, right, ThreadPoolExecutor(max_workers=1) as executor:
        response = executor.submit(respond)
        with connect(
            "ws://localhost/rpc", sock=left, open_timeout=1, close_timeout=1
        ) as connection:
            assert codexo.AppServer(connection).request(
                "account/rateLimits/read", {}
            ) == {"rateLimits": {}}
        response.result(timeout=2)


def test_unresponsive_websocket_reports_the_method_that_timed_out() -> None:
    connection = Mock(spec=ClientConnection)
    connection.recv.side_effect = TimeoutError
    with pytest.raises(TimeoutError, match="initialize: request timed out"):
        codexo.AppServer(connection).request("initialize", {})


def test_notifications_cannot_keep_a_request_waiting_forever(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    connection = Mock(spec=ClientConnection)
    connection.recv.return_value = json.dumps({"method": "notification", "params": {}})
    clock = iter([0, 9, 11])
    monkeypatch.setattr(codexo, "monotonic", lambda: next(clock))
    with pytest.raises(TimeoutError, match="thread/read: request timed out"):
        codexo.AppServer(connection).request("thread/read", {"threadId": "chat"})
    assert connection.recv.call_count == 1


def test_desktop_handshake_uses_websocket_and_initializes_the_watcher(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    connection = Mock(spec=ClientConnection)
    connection.recv.return_value = json.dumps({"id": 1, "result": {}})
    context = Mock()
    context.__enter__ = Mock(return_value=connection)
    context.__exit__ = Mock(return_value=False)
    connect = Mock(return_value=context)
    path = tmp_path / "app.sock"
    monkeypatch.setattr(codexo, "desktop_socket", lambda: path)
    monkeypatch.setattr(codexo, "unix_connect", connect)
    with codexo.desktop_server() as server:
        assert server.reader is connection
    assert connect.call_args.args == (path,)
    assert connect.call_args.kwargs["uri"] == "ws://localhost/rpc"
    assert connect.call_args.kwargs["open_timeout"] == 10
    assert [
        json.loads(c.args[0]).get("method") for c in connection.send.call_args_list
    ] == ["initialize", "initialized"]


def test_bridge_forwards_stdio_and_server_messages_without_extra_output() -> None:
    connection = Mock(spec=ClientConnection)
    closed = ConnectionClosedOK(Close(1000, ""), Close(1000, ""), True)
    sent = Event()
    connection.send.side_effect = lambda message: sent.set()
    replies = iter(['{"id":1,"result":{}}', closed])

    def receive() -> str:
        assert sent.wait(timeout=1)
        reply = next(replies)
        if isinstance(reply, ConnectionClosedOK):
            raise reply
        return reply

    connection.recv.side_effect = receive
    writer = StringIO()
    codexo.forward_desktop(
        connection, StringIO('{"id":1,"method":"initialize"}\n'), writer
    )
    assert writer.getvalue() == '{"id":1,"result":{}}\n'
    connection.send.assert_called_once_with('{"id":1,"method":"initialize"}')


@pytest.mark.parametrize("startup", ["ready", "failed", "timeout"])
def test_bridge_owns_the_original_server_and_cleans_it_up(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path, startup: str
) -> None:
    monkeypatch.setenv("CODEX_INTERNAL_ORIGINATOR_OVERRIDE", "Codex Desktop")
    path = tmp_path / "server.sock"
    monkeypatch.setattr(codexo, "bridge_socket", lambda p: path)
    monkeypatch.setattr(codexo.Path, "is_socket", lambda p: startup == "ready")
    monkeypatch.setattr(codexo.signal, "signal", Mock())
    process = Mock()
    process.__enter__ = Mock(return_value=process)
    process.__exit__ = Mock(return_value=False)
    process.poll.return_value = 1 if startup == "failed" else None
    process.returncode = 1
    start = Mock(return_value=process)
    monkeypatch.setattr(codexo.subprocess, "Popen", start)
    connection = Mock(spec=ClientConnection)
    context = Mock()
    context.__enter__ = Mock(return_value=connection)
    context.__exit__ = Mock(return_value=False)
    monkeypatch.setattr(codexo, "unix_connect", Mock(return_value=context))
    forward = Mock()
    monkeypatch.setattr(codexo, "forward_desktop", forward)
    arguments = [
        "-c",
        "features.code_mode_host=true",
        "app-server",
        "--analytics-default-enabled",
    ]
    clock = iter([0, 11])
    monkeypatch.setattr(codexo, "monotonic", lambda: next(clock))
    if startup == "ready":
        codexo.bridge_desktop(arguments)
        forward.assert_called_once_with(connection, sys.stdin, sys.stdout)
    else:
        with pytest.raises((RuntimeError, TimeoutError)):
            codexo.bridge_desktop(arguments)
        forward.assert_not_called()
    assert start.call_args.args[0] == [
        codexo.CODEX_EXECUTABLE,
        *arguments,
        "--listen",
        f"unix://{path}",
    ]
    process.terminate.assert_called_once()
    process.wait.assert_called_once_with(timeout=10)
