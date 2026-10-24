#!/Users/tom/code/dotfiles/.venv/bin/python
"""Retire Codex chats and report repository metadata.

Retire preserves cwd and name; application Project membership is unverified.
Run `retire` to select a chat, or `retire THREAD_ID` for a specific chat.
"""

from __future__ import annotations

import hashlib
import json
import os
import plistlib
import shlex
import signal
import subprocess
import sys
from collections import deque
from collections.abc import Callable, Iterator
from contextlib import ExitStack, contextmanager
from datetime import datetime
from pathlib import Path
from tempfile import mkdtemp
from threading import Thread
from time import monotonic, sleep
from types import FrameType
from typing import IO, Annotated, cast

import tomlkit
import tomllib
import tyro
from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    TypeAdapter,
    ValidationError,
)
from websockets.exceptions import ConnectionClosedOK, WebSocketException
from websockets.sync.client import ClientConnection, unix_connect


class RetireCommand(BaseModel, frozen=True):
    """Replace a chat using a verified handover, then archive the old chat."""

    thread_id: Annotated[str | None, tyro.conf.Positional] = None


class FixCommand(BaseModel, frozen=True):
    """Report project descriptions and Codex titles without changing them."""

    repos: Annotated[list[str], tyro.conf.Positional]
    codex: Annotated[bool, tyro.conf.arg(prefix_name=False)] = True
    """Include Codex Project and chat titles."""


class BridgeCommand(BaseModel, frozen=True):
    """Internal CLI launcher used by the desktop app."""

    args: Annotated[list[str], tyro.conf.Positional]


class WatchInstallCommand(BaseModel, frozen=True):
    """Install the watcher and login environment helper."""


class WatchUninstallCommand(BaseModel, frozen=True):
    """Uninstall the watcher and login environment helper."""


class WatchRestartCommand(BaseModel, frozen=True):
    """Restart the installed daemon."""


class WatchDaemonCommand(BaseModel, frozen=True):
    """Print the installed daemon's status."""


class WatchCommand(BaseModel, frozen=True):
    """Prompt an idle Codex chat when either usage window resets."""

    interval: Annotated[
        float, tyro.conf.arg(prefix_name=False), tyro.conf.CascadeSubcommandArgs
    ] = Field(default=60, gt=0)
    """Polling interval in seconds."""
    prompt: Annotated[
        str, tyro.conf.arg(prefix_name=False), tyro.conf.CascadeSubcommandArgs
    ] = "hello"
    test: Annotated[
        bool, tyro.conf.arg(prefix_name=False), tyro.conf.CascadeSubcommandArgs
    ] = False
    """Send Say \"test\" on idle polls, using the configured interval."""
    action: tyro.conf.DisallowNone[
        Annotated[
            WatchInstallCommand, tyro.conf.subcommand(name="install", prefix_name=False)
        ]
        | Annotated[
            WatchUninstallCommand,
            tyro.conf.subcommand(name="uninstall", prefix_name=False),
        ]
        | Annotated[
            WatchRestartCommand, tyro.conf.subcommand(name="restart", prefix_name=False)
        ]
        | Annotated[
            WatchDaemonCommand, tyro.conf.subcommand(name="daemon", prefix_name=False)
        ]
        | None
    ] = None


class CodexoCommand(BaseModel, frozen=True):
    action: (
        Annotated[RetireCommand, tyro.conf.subcommand(name="retire", prefix_name=False)]
        | Annotated[FixCommand, tyro.conf.subcommand(name="fix", prefix_name=False)]
        | Annotated[WatchCommand, tyro.conf.subcommand(name="watch", prefix_name=False)]
        | Annotated[
            BridgeCommand, tyro.conf.subcommand(name="bridge", prefix_name=False)
        ]
    )
    dry_run: Annotated[bool, tyro.conf.arg(aliases=["-d"])] = False
    """Print planned operations without starting app-server or making changes."""


class Chat(BaseModel, frozen=True):
    id: str
    name: str | None = None
    cwd: Path
    status: dict[str, object]


class ChatPage(BaseModel, frozen=True):
    data: list[Chat]
    nextCursor: str | None = None


class LoadedChats(BaseModel, frozen=True):
    data: list[str]
    nextCursor: str | None = None


class UsageWindow(BaseModel, frozen=True):
    usedPercent: int


class UsageLimits(BaseModel, frozen=True):
    primary: UsageWindow | None = None
    secondary: UsageWindow | None = None


class WatchPoll(BaseModel):
    primary: int | None = None
    secondary: int | None = None
    idle: bool | None = None
    prompt_sent: bool = False
    detail: str = ""


class ProjectRoot(BaseModel, frozen=True):
    path: Path


class AppProject(BaseModel, frozen=True):
    name: str
    roots: list[ProjectRoot]


class ProjectPage(BaseModel, frozen=True):
    data: list[AppProject]
    nextCursor: str | None = None


class CodexInfo(BaseModel, frozen=True):
    project: str | None = None
    chats: list[str] = Field(default_factory=list)


class RpcMessage(BaseModel, frozen=True):
    id: int | str | None = None
    method: str | None = None
    params: dict[str, object] = Field(default_factory=dict)
    result: dict[str, object] | None = None
    error: dict[str, object] | None = None


class Turn(BaseModel, frozen=True):
    id: str
    status: str
    error: dict[str, object] | None = None


class ReadReceipt(BaseModel, frozen=True):
    sha256: str

    model_config = ConfigDict(extra="forbid")


class AppServer:
    """One synchronous JSON-RPC connection, retaining early turn events."""

    def __init__(
        self, reader: IO[str] | ClientConnection, writer: IO[str] | None = None
    ) -> None:
        self.reader = reader
        self.writer = writer
        self.sequence = 0
        self.events: deque[RpcMessage] = deque()

    def send(self, message: dict[str, object]) -> None:
        if isinstance(self.reader, ClientConnection):
            self.reader.send(json.dumps(message))
            return
        assert self.writer is not None
        self.writer.write(json.dumps(message) + "\n")
        self.writer.flush()

    def receive(self, timeout: float | None = None) -> RpcMessage:
        line = (
            self.reader.recv(timeout=timeout)
            if isinstance(self.reader, ClientConnection)
            else self.reader.readline()
        )
        if not line:
            raise RuntimeError("Codex app-server closed the connection")
        message = RpcMessage.model_validate_json(line)
        if message.method is not None and message.id is not None:
            self.send(
                {
                    "id": message.id,
                    "error": {"code": -32601, "message": "Unsupported client request"},
                }
            )
            raise RuntimeError(
                f"Codex requested {message.method}; this standalone client cannot "
                "provide approvals, user input, or app-only tools. Old chat retained."
            )
        return message

    def request(self, method: str, params: dict[str, object]) -> dict[str, object]:
        self.sequence += 1
        request_id = self.sequence
        self.send({"id": request_id, "method": method, "params": params})
        deadline = monotonic() + WATCH_REQUEST_TIMEOUT
        while True:
            if isinstance(self.reader, ClientConnection):
                remaining = deadline - monotonic()
                if remaining <= 0:
                    raise TimeoutError(f"{method}: request timed out")
                try:
                    message = self.receive(remaining)
                except TimeoutError:
                    raise TimeoutError(f"{method}: request timed out") from None
            else:
                message = self.receive()
            if message.method is not None:
                if message.method in {"turn/completed", "item/completed"}:
                    self.events.append(message)
                continue
            if message.id != request_id:
                raise RuntimeError(f"Unexpected app-server response ID: {message.id}")
            if message.error is not None:
                raise RuntimeError(f"{method}: {message.error}")
            if message.result is None:
                raise RuntimeError(f"{method}: missing result")
            return message.result

    def run_turn(self, thread_id: str, prompt: str, receipt: bool = False) -> str:
        params: dict[str, object] = {
            "threadId": thread_id,
            "input": [{"type": "text", "text": prompt}],
        }
        if receipt:
            params["outputSchema"] = ReadReceipt.model_json_schema()
        result = self.request("turn/start", params)
        started = Turn.model_validate(result["turn"])
        reply = ""
        while True:
            event = self.events.popleft() if self.events else self.receive()
            if event.params.get("threadId") != thread_id:
                continue
            if event.method == "item/completed":
                if event.params.get("turnId") != started.id:
                    continue
                item = cast(dict[str, object], event.params["item"])
                if item.get("type") == "agentMessage":
                    text = item.get("text")
                    if isinstance(text, str):
                        reply = text
            elif event.method == "turn/completed":
                turn = Turn.model_validate(event.params["turn"])
                if turn.id != started.id:
                    continue
                if turn.status != "completed":
                    raise RuntimeError(f"Turn {turn.id} {turn.status}: {turn.error}")
                return reply


def main() -> None:
    command = tyro.cli(CodexoCommand)
    if isinstance(command.action, BridgeCommand):
        if command.dry_run:
            print("Bridge desktop stdio to its private Unix WebSocket server.")
            return
        try:
            bridge_desktop(command.action.args)
        except (OSError, RuntimeError, WebSocketException) as error:
            sys.exit(str(error))
        return
    if isinstance(command.action, WatchCommand):
        try:
            watch(command.action, command.dry_run)
        except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
            sys.exit(str(error))
        except KeyboardInterrupt:
            return
        return
    if isinstance(command.action, FixCommand):
        fix_repositories(command.action, command.dry_run)
        return
    if command.dry_run:
        print_dry_run(command.action.thread_id)
        return
    try:
        with app_server("codex") as server:
            thread_id = command.action.thread_id or select_chat(server)
            replace_chat(server, thread_id)
    except (OSError, RuntimeError, ValidationError, EOFError) as error:
        sys.exit(str(error))
    except KeyboardInterrupt:
        sys.exit("Interrupted; any unfinished handover has not archived the old chat.")


@contextmanager
def app_server(
    executable: str,
    experimental: bool = False,
    quiet: bool = False,
) -> Iterator[AppServer]:
    arguments = [executable, "app-server"]
    with subprocess.Popen(
        arguments,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL if quiet else None,
        text=True,
        bufsize=1,
    ) as process:
        assert process.stdout is not None
        assert process.stdin is not None
        try:
            server = AppServer(process.stdout, process.stdin)
            server.request(
                "initialize",
                {
                    "clientInfo": {"name": "codexo", "version": "0.1.0"},
                    "capabilities": {"experimentalApi": experimental},
                },
            )
            server.send({"method": "initialized", "params": {}})
            yield server
        finally:
            process.terminate()


def bridge_socket(pid: int) -> Path:
    return Path(f"/tmp/codexo-{os.getuid()}/app-{pid}.sock")


def stop_bridge(signum: int, frame: FrameType | None) -> None:
    raise SystemExit(0)


def bridge_desktop(arguments: list[str]) -> None:
    server_args = (
        arguments[arguments.index("app-server") + 1 :]
        if "app-server" in arguments
        else []
    )
    if (
        "app-server" not in arguments
        or (server_args and not server_args[0].startswith("-"))
        or os.environ.get("CODEX_INTERNAL_ORIGINATOR_OVERRIDE") != "Codex Desktop"
    ):
        os.execv(CODEX_EXECUTABLE, [CODEX_EXECUTABLE, *arguments])
    path = bridge_socket(os.getpid())
    path.parent.mkdir(mode=0o700, exist_ok=True)
    signal.signal(signal.SIGTERM, stop_bridge)
    with subprocess.Popen(
        [CODEX_EXECUTABLE, *arguments, "--listen", f"unix://{path}"],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
    ) as process:
        try:
            deadline = monotonic() + WATCH_REQUEST_TIMEOUT
            while not path.is_socket():
                if process.poll() is not None:
                    raise RuntimeError(
                        f"Desktop app-server exited with status {process.returncode}"
                    )
                if monotonic() >= deadline:
                    raise TimeoutError("Desktop app-server socket startup timed out")
                sleep(0.05)
            with unix_connect(
                path,
                uri="ws://localhost/rpc",
                open_timeout=WATCH_REQUEST_TIMEOUT,
                close_timeout=1,
                max_size=None,
            ) as connection:
                forward_desktop(connection, sys.stdin, sys.stdout)
        finally:
            process.terminate()
            try:
                process.wait(timeout=WATCH_REQUEST_TIMEOUT)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()


def forward_desktop(
    connection: ClientConnection, reader: IO[str], writer: IO[str]
) -> None:
    errors: list[OSError | WebSocketException] = []

    def send_input() -> None:
        try:
            for line in reader:
                connection.send(line.rstrip("\n"))
        except (OSError, WebSocketException) as error:
            errors.append(error)
        finally:
            connection.close()

    Thread(target=send_input, daemon=True).start()
    try:
        while True:
            message = connection.recv()
            writer.write(
                (message.decode() if isinstance(message, bytes) else message) + "\n"
            )
            writer.flush()
    except ConnectionClosedOK:
        if errors:
            raise errors[0] from None


def desktop_socket() -> Path:
    output = subprocess.run(
        ["/bin/ps", "-axo", "pid,ppid,command"],
        check=True,
        capture_output=True,
        text=True,
        timeout=WATCH_REQUEST_TIMEOUT,
    ).stdout
    processes = [s.split(maxsplit=2) for s in output.splitlines()[1:] if s.strip()]
    app_pids = {
        p
        for p, _, c in processes
        if c.split(" ", 1)[0] == "/Applications/ChatGPT.app/Contents/MacOS/ChatGPT"
    }
    prefix = f"{sys.executable} {Path(__file__).resolve()} bridge -- "
    paths = [
        bridge_socket(int(p))
        for p, parent, c in processes
        if parent in app_pids and c.startswith(prefix)
    ]
    if len(paths) != 1:
        raise RuntimeError(
            "Desktop bridge unavailable: run codexo watch install, "
            "then quit and reopen Codex"
        )
    return paths[0]


@contextmanager
def desktop_server() -> Iterator[AppServer]:
    with unix_connect(
        desktop_socket(),
        uri="ws://localhost/rpc",
        open_timeout=WATCH_REQUEST_TIMEOUT,
        close_timeout=1,
        max_size=None,
    ) as connection:
        server = AppServer(connection)
        server.request(
            "initialize",
            {
                "clientInfo": {"name": "codexo", "version": "0.1.0"},
                "capabilities": {"experimentalApi": True},
            },
        )
        server.send({"method": "initialized", "params": {}})
        yield server


def watch(command: WatchCommand, dry_run: bool) -> None:
    if command.action is not None:
        watch_daemon(command, dry_run)
        return
    if dry_run:
        print(f"Poll running Codex every {command.interval:g} seconds.")
        print("Read both usage windows; record every successful observation.")
        print(
            "Check all loaded chats on every poll and log usage, idle and send status."
        )
        print("An initial zero or nonzero-to-zero transition can trigger a prompt.")
        print("If idle, send one prompt to the most recently updated unarchived chat:")
        print(command.prompt)
        if command.test:
            print('On other idle polls, send Say "test" to that chat.')
        return
    previous: dict[str, int] = {}
    while True:
        poll = WatchPoll()
        try:
            if desktop_running():
                with desktop_server() as server:
                    watch_iteration(
                        server, previous, command.prompt, poll, command.test
                    )
            else:
                poll.detail = "Codex is not running"
        except (
            OSError,
            RuntimeError,
            ValueError,
            EOFError,
            WebSocketException,
            subprocess.SubprocessError,
        ) as error:
            poll.detail = f"error: {error}"
        finally:
            log_watch_poll(poll)
        sleep(command.interval)


def log_watch_poll(poll: WatchPoll) -> None:
    timestamp = datetime.now().astimezone().isoformat(timespec="seconds")
    primary = "unavailable" if poll.primary is None else str(poll.primary)
    secondary = "unavailable" if poll.secondary is None else str(poll.secondary)
    idle = "unknown" if poll.idle is None else str(poll.idle).lower()
    detail = (
        f" detail={json.dumps(poll.detail, ensure_ascii=False)}" if poll.detail else ""
    )
    print(
        f"{timestamp} codexo watch: primary.usedPercent={primary} "
        f"secondary.usedPercent={secondary} idle={idle} "
        f"prompt_sent={str(poll.prompt_sent).lower()}{detail}",
        flush=True,
    )


def desktop_running() -> bool:
    return (
        subprocess.run(
            [
                "/usr/bin/pgrep",
                "-f",
                "^/Applications/ChatGPT.app/Contents/MacOS/ChatGPT$",
            ],
            capture_output=True,
            check=False,
        ).returncode
        == 0
    )


def watch_iteration(
    server: AppServer,
    previous: dict[str, int],
    prompt: str,
    poll: WatchPoll,
    test: bool = False,
) -> bool:
    limits = UsageLimits.model_validate(
        server.request("account/rateLimits/read", {})["rateLimits"]
    )
    poll.primary = limits.primary.usedPercent if limits.primary is not None else None
    poll.secondary = (
        limits.secondary.usedPercent if limits.secondary is not None else None
    )
    current = {
        n: w.usedPercent
        for n, w in [("primary", limits.primary), ("secondary", limits.secondary)]
        if w is not None
    }
    reset = any(v == 0 and previous.get(n) != 0 for n, v in current.items())
    previous.update(current)
    poll.idle = chats_idle(server)
    if not (reset or test) or not poll.idle:
        return False
    page = ChatPage.model_validate(
        server.request(
            "thread/list",
            {
                "limit": 1,
                "sortKey": "updated_at",
                "archived": False,
                "modelProviders": [],
                "sourceKinds": THREAD_SOURCE_KINDS,
                "useStateDbOnly": True,
            },
        )
    )
    if not page.data:
        poll.detail = "no unarchived chat available"
        return False
    chat = Chat.model_validate(
        server.request(
            "thread/resume",
            {"threadId": page.data[0].id, "excludeTurns": True},
        )["thread"]
    )
    if chat.status["type"] != "idle":
        poll.idle = False
        return False
    server.request(
        "turn/start",
        {
            "threadId": chat.id,
            "input": [{"type": "text", "text": prompt if reset else 'Say "test"'}],
        },
    )
    poll.prompt_sent = True
    poll.detail = f"sent to {chat.id} ({chat.name or '(unnamed)'})"
    if not reset:
        poll.detail = f"test prompt {poll.detail}"
    return True


def chats_idle(server: AppServer) -> bool:
    cursor: str | None = None
    while True:
        page = LoadedChats.model_validate(
            server.request("thread/loaded/list", {"cursor": cursor})
        )
        for thread_id in page.data:
            chat = Chat.model_validate(
                server.request("thread/read", {"threadId": thread_id})["thread"]
            )
            if chat.status["type"] not in {"idle", "notLoaded"}:
                return False
            terminals = server.request(
                "thread/backgroundTerminals/list", {"threadId": thread_id, "limit": 1}
            )
            if terminals["data"]:
                return False
        if page.nextCursor is None:
            return True
        cursor = page.nextCursor


def watch_launch_agent(command: WatchCommand) -> dict[str, object]:
    arguments = [
        sys.executable,
        str(Path(__file__).resolve()),
        "watch",
        "--interval",
        str(command.interval),
        "--prompt",
        command.prompt,
    ]
    if command.test:
        arguments.append("--test")
    document: dict[str, object] = {
        "Label": WATCH_LABEL,
        "ProgramArguments": arguments,
        "RunAtLoad": True,
        "KeepAlive": True,
        "ProcessType": "Background",
        "StandardOutPath": str(Path.home() / "Library/Logs/codexo-watch.log"),
        "StandardErrorPath": str(Path.home() / "Library/Logs/codexo-watch.log"),
    }
    if "CODEX_HOME" in os.environ:
        document["EnvironmentVariables"] = {"CODEX_HOME": os.environ["CODEX_HOME"]}
    return document


def watch_daemon(command: WatchCommand, dry_run: bool) -> None:
    path = Path.home() / "Library/LaunchAgents" / f"{WATCH_LABEL}.plist"
    environment_path = path.with_name(f"{WATCH_ENVIRONMENT_LABEL}.plist")
    launcher = Path.home() / "Library/Application Support/codexo/codex"
    launcher_text = (
        f"#!/bin/sh\nexec {shlex.quote(sys.executable)} "
        f'{shlex.quote(str(Path(__file__).resolve()))} bridge -- "$@"\n'
    )
    domain = f"gui/{os.getuid()}"
    service = f"{domain}/{WATCH_LABEL}"
    if isinstance(command.action, WatchInstallCommand):
        agents = {
            environment_path: {
                "Label": WATCH_ENVIRONMENT_LABEL,
                "ProgramArguments": [
                    "/bin/launchctl",
                    "setenv",
                    "CODEX_CLI_PATH",
                    str(launcher),
                ],
                "RunAtLoad": True,
            },
            path: watch_launch_agent(command),
        }
    if dry_run:
        if isinstance(command.action, WatchInstallCommand):
            print(f"Write executable {launcher}:\n{launcher_text}", end="")
            print(f"launchctl setenv CODEX_CLI_PATH {shlex.quote(str(launcher))}")
            print("launchctl unsetenv CODEX_APP_SERVER_USE_LOCAL_DAEMON")
            for p, document in agents.items():
                print(f"Unload {domain}/{document['Label']} if loaded.")
                print(f"Write {p} and launchctl bootstrap {domain} {p}:")
                print(plistlib.dumps(document).decode(), end="")
        elif isinstance(command.action, WatchUninstallCommand):
            print(
                f"Unload {service} and {domain}/{WATCH_ENVIRONMENT_LABEL} "
                f"if loaded, then remove {path} and {environment_path}; "
                "error if not installed."
            )
            print("launchctl unsetenv CODEX_APP_SERVER_USE_LOCAL_DAEMON")
            print(f"Remove {launcher}; launchctl unsetenv CODEX_CLI_PATH")
        elif isinstance(command.action, WatchRestartCommand):
            print(f"Restart the daemon with launchctl kickstart -k {service}.")
        else:
            print(f"Print daemon status with launchctl print {service}.")
        return
    if isinstance(command.action, WatchInstallCommand):
        path.parent.mkdir(parents=True, exist_ok=True)
        (Path.home() / "Library/Logs").mkdir(parents=True, exist_ok=True)
        launcher.parent.mkdir(parents=True, exist_ok=True)
        launcher.write_text(launcher_text)
        launcher.chmod(0o755)
        subprocess.run(
            ["/bin/launchctl", "unsetenv", "CODEX_APP_SERVER_USE_LOCAL_DAEMON"],
            check=True,
        )
        subprocess.run(
            ["/bin/launchctl", "setenv", "CODEX_CLI_PATH", str(launcher)],
            check=True,
        )
        for p, document in agents.items():
            target = f"{domain}/{document['Label']}"
            status = subprocess.run(
                ["/bin/launchctl", "print", target],
                capture_output=True,
                text=True,
                check=False,
            )
            if status.returncode == 0:
                subprocess.run(["/bin/launchctl", "bootout", target], check=True)
            p.write_bytes(plistlib.dumps(document))
            subprocess.run(["/bin/launchctl", "bootstrap", domain, str(p)], check=True)
        print(f"Installed codexo watch: {path}")
        print("Quit and reopen the desktop app once to use the codexo bridge.")
        return
    if isinstance(command.action, WatchRestartCommand):
        subprocess.run(["/bin/launchctl", "kickstart", "-k", service], check=True)
        print("Restarted codexo watch.")
        return
    if isinstance(command.action, WatchUninstallCommand):
        installed = False
        for p, label in [
            (environment_path, WATCH_ENVIRONMENT_LABEL),
            (path, WATCH_LABEL),
        ]:
            target = f"{domain}/{label}"
            status = subprocess.run(
                ["/bin/launchctl", "print", target],
                capture_output=True,
                text=True,
                check=False,
            )
            installed |= p.is_file() or status.returncode == 0
            if status.returncode == 0:
                subprocess.run(["/bin/launchctl", "bootout", target], check=True)
            p.unlink(missing_ok=True)
        if not installed:
            raise RuntimeError("codexo watch is not installed")
        subprocess.run(
            ["/bin/launchctl", "unsetenv", "CODEX_APP_SERVER_USE_LOCAL_DAEMON"],
            check=True,
        )
        subprocess.run(["/bin/launchctl", "unsetenv", "CODEX_CLI_PATH"], check=True)
        launcher.unlink(missing_ok=True)
        print("Uninstalled codexo watch.")
        return
    status = subprocess.run(
        ["/bin/launchctl", "print", service],
        capture_output=True,
        text=True,
        check=False,
    )
    if status.returncode == 0:
        print(status.stdout, end="")
    else:
        print(
            "codexo watch: installed but not loaded"
            if path.is_file()
            else "codexo watch: not installed"
        )


def fix_repositories(command: FixCommand, dry_run: bool) -> None:
    directories = [
        p for r in command.repos if (p := (Path.home() / "code" / r).resolve()).is_dir()
    ]
    if not directories:
        return
    if dry_run:
        if command.codex:
            print("Start the desktop app's bundled codex app-server; initialize.")
        for p in directories:
            print(f"{p.name}:")
            print("    Read pyproject.toml description and first README.md heading.")
            print("    gh repo view: read the GitHub description.")
            if command.codex:
                print("    project/list: find at most one project with this root.")
                print("    thread/list: list all unarchived chats at this directory.")
            print("    Print available values as TOML; omit failed sources.")
        if command.codex:
            print("Stop app-server; no project information is changed.")
        return
    with ExitStack() as stack:
        server = None
        if command.codex:
            try:
                server = stack.enter_context(
                    app_server(
                        CODEX_EXECUTABLE,
                        experimental=True,
                        quiet=True,
                    )
                )
            except Exception:
                # Codex is optional, including when startup or initialization fails.
                pass
        for p in directories:
            report_repository(server, p)


def report_repository(server: AppServer | None, directory: Path) -> None:
    sources: dict[str, Callable[[], object]] = {
        "pyproject": lambda: pyproject_description(directory),
        "README": lambda: readme_heading(directory),
        "github": lambda: github_description(directory),
    }
    if server is not None:
        sources["codex"] = lambda: codex_info(server, directory).model_dump(
            exclude_none=True
        )
    report = tomlkit.table()
    for name, read in sources.items():
        try:
            value = read()
            if name == "codex":
                inline = tomlkit.inline_table()
                inline.update(cast(dict[str, object], value))
                value = inline
            report[name] = value
        except Exception:
            # Each source is independent; omit any information that cannot be read.
            continue
    if report:
        document = tomlkit.document()
        document[directory.name] = report
        print(tomlkit.dumps(document))


def pyproject_description(directory: Path) -> str:
    with (directory / "pyproject.toml").open("rb") as source:
        return TypeAdapter(str).validate_python(
            tomllib.load(source)["project"]["description"]
        )


def readme_heading(directory: Path) -> str:
    return next(
        s[2:].strip()
        for s in (directory / "README.md").read_text().splitlines()
        if s.startswith("# ")
    )


def github_description(directory: Path) -> str:
    return subprocess.run(
        ["gh", "repo", "view", "--json", "description", "--jq", ".description"],
        cwd=directory,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.rstrip("\n")


def codex_info(server: AppServer, directory: Path) -> CodexInfo:
    projects: list[str] = []
    cursor: str | None = None
    while True:
        page = ProjectPage.model_validate(
            server.request("project/list", {"cursor": cursor})
        )
        projects.extend(
            p.name
            for p in page.data
            if any(r.path.resolve() == directory for r in p.roots)
        )
        if page.nextCursor is None:
            break
        cursor = page.nextCursor
    if len(projects) > 1:
        raise RuntimeError(f"Multiple Codex projects at {directory}: {projects}")
    info = CodexInfo(project=projects[0] if projects else None)
    cursor = None
    while True:
        chats = ChatPage.model_validate(
            server.request(
                "thread/list",
                {
                    "cursor": cursor,
                    "cwd": str(directory),
                    "archived": False,
                    "modelProviders": [],
                    "sourceKinds": THREAD_SOURCE_KINDS,
                    "useStateDbOnly": True,
                },
            )
        )
        info.chats.extend(
            c.name if c.name is not None else "(unnamed)" for c in chats.data
        )
        if chats.nextCursor is None:
            return info
        cursor = chats.nextCursor


def print_dry_run(thread_id: str | None) -> None:
    print("Dry run: no app-server, file, or chat operations will be performed.")
    print("Values in <angle brackets> are determined during a real run.")
    print("Start codex app-server; send initialize and initialized.")
    if thread_id is None:
        print("thread/list: list unarchived app-server chats, 25 per page.")
        print("Ask for a chat number, fetching further pages as requested.")
    old_id = thread_id if thread_id is not None else "<selected-chat-id>"
    print(f"thread/read: read {old_id!r}; require a display name and an idle chat.")
    print("Preserve <old-chat-name> and <old-working-directory>.")
    print("Application Project membership is unverified.")
    print(f"thread/resume: resume {old_id!r}; require it to remain idle.")
    print("Create a unique codexo- temporary directory.")
    print("Use <temporary-directory>/handover.md as the handover path.")
    print(f"turn/start: ask {old_id!r} to write the handover document.")
    print("Wait for successful turn/completed; require a nonempty handover file.")
    print("Compute the handover file's SHA-256 checksum.")
    print("thread/start: create <replacement-chat-id> in <old-working-directory>.")
    print("Require the replacement working directory to match the old one.")
    print("thread/name/set: name <replacement-chat-id> <old-chat-name>.")
    print(
        "turn/start: ask the replacement to read the handover and return its SHA-256."
    )
    print("Wait for successful turn/completed; require a matching checksum receipt.")
    print(f"thread/archive: archive {old_id!r} only after verification succeeds.")
    print("Print the replacement ID; stop app-server. Keep the handover file.")


def select_chat(server: AppServer) -> str:
    cursor: str | None = None
    while True:
        page = ChatPage.model_validate(
            server.request(
                "thread/list",
                {
                    "cursor": cursor,
                    "limit": 25,
                    "sourceKinds": ["appServer"],
                    "archived": False,
                    "sortKey": "updated_at",
                },
            )
        )
        if not page.data:
            raise RuntimeError("No Codex app-server chats found")
        for i, chat in enumerate(page.data, 1):
            print(f"{i}. {chat.name or '(unnamed)'} [{chat.id}] ({chat.cwd})")
        choice = input(
            "Select chat number"
            + (" or n for next page" if page.nextCursor else "")
            + ": "
        )
        if choice == "n" and page.nextCursor:
            cursor = page.nextCursor
        elif choice.isdecimal() and 1 <= int(choice) <= len(page.data):
            return page.data[int(choice) - 1].id
        else:
            print("Choose a number from this page.", file=sys.stderr)


def replace_chat(server: AppServer, thread_id: str) -> str:
    old = Chat.model_validate(
        server.request("thread/read", {"threadId": thread_id})["thread"]
    )
    if not old.name:
        raise RuntimeError("Selected chat has no display name to preserve")
    if old.status["type"] not in {"idle", "notLoaded"}:
        raise RuntimeError("Selected chat must be idle before handover")
    print(f"Replacing {old.name!r}; preserving {old.cwd}.")
    print("Application Project membership is unverified.")
    resumed = Chat.model_validate(
        server.request("thread/resume", {"threadId": old.id, "excludeTurns": True})[
            "thread"
        ]
    )
    if resumed.status["type"] != "idle":
        raise RuntimeError("Selected chat is not idle after resume")
    handover = Path(mkdtemp(prefix="codexo-")) / "handover.md"
    print(f"Handover: {handover}", flush=True)
    server.run_turn(
        old.id,
        f"Create a Markdown handover document at {json.dumps(str(handover))}. "
        "Include the objective, user instructions and decisions, completed work, "
        "relevant paths and commits, verification results, remaining work "
        "and blockers. "
        "Write the file using your file tools. Do not continue implementation, change "
        "other files, commit, push, create chats, or archive chats. "
        "Stop when it is saved.",
    )
    if not handover.is_file() or not handover.read_text().strip():
        raise RuntimeError(f"Handover was not written: {handover}; old chat retained")
    digest = hashlib.sha256(handover.read_bytes()).hexdigest()
    new = Chat.model_validate(
        server.request("thread/start", {"cwd": str(old.cwd)})["thread"]
    )
    print(f"Replacement chat: {new.id}", flush=True)
    if new.cwd != old.cwd:
        raise RuntimeError("Replacement working directory differs; old chat retained")
    server.request("thread/name/set", {"threadId": new.id, "name": old.name})
    reply = server.run_turn(
        new.id,
        f"Read the complete handover document at {json.dumps(str(handover))} "
        "using your file tools and retain its context for future work. Compute the "
        "SHA-256 checksum of the file's bytes using a tool and return it in the sha256 "
        "field. If you cannot read it, report failure instead of claiming success. "
        "Do not continue the work, modify files, commit, push, or archive chats.",
        receipt=True,
    )
    if ReadReceipt.model_validate_json(reply).sha256 != digest:
        raise RuntimeError("Replacement did not verify the handover; old chat retained")
    server.request("thread/archive", {"threadId": old.id})
    print(f"Archived {old.id}. Continue in {new.id} ({old.name}).")
    return new.id


CODEX_EXECUTABLE = "/Applications/ChatGPT.app/Contents/Resources/codex-cli/bin/codex"
WATCH_LABEL = "local.codexo.watch"
WATCH_ENVIRONMENT_LABEL = "local.codexo.watch-environment"
WATCH_REQUEST_TIMEOUT = 10


THREAD_SOURCE_KINDS = [
    "cli",
    "vscode",
    "exec",
    "appServer",
    "subAgent",
    "subAgentReview",
    "subAgentCompact",
    "subAgentThreadSpawn",
    "subAgentOther",
    "unknown",
]


if __name__ == "__main__":
    main()
