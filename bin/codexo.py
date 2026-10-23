#!/Users/tom/code/dotfiles/.venv/bin/python
"""Retire Codex chats and report repository metadata.

Retire preserves cwd and name; application Project membership is unverified.
Run `retire` to select a chat, or `retire THREAD_ID` for a specific chat.
"""

from __future__ import annotations

import hashlib
import json
import subprocess
import sys
from collections import deque
from pathlib import Path
from tempfile import mkdtemp
from typing import IO, Annotated, cast

import tomlkit
import tomllib
import tyro
from pydantic import BaseModel, ConfigDict, Field, ValidationError


class RetireCommand(BaseModel, frozen=True):
    """Replace a chat using a verified handover, then archive the old chat."""

    thread_id: Annotated[str | None, tyro.conf.Positional] = None


class FixCommand(BaseModel, frozen=True):
    """Report project descriptions and Codex titles without changing them."""

    repos: Annotated[list[str], tyro.conf.Positional]
    codex: Annotated[bool, tyro.conf.arg(prefix_name=False)] = True
    """Include Codex Project and chat titles."""


class CodexoCommand(BaseModel, frozen=True):
    action: (
        Annotated[RetireCommand, tyro.conf.subcommand(name="retire", prefix_name=False)]
        | Annotated[FixCommand, tyro.conf.subcommand(name="fix", prefix_name=False)]
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

    def __init__(self, reader: IO[str], writer: IO[str]) -> None:
        self.reader = reader
        self.writer = writer
        self.sequence = 0
        self.events: deque[RpcMessage] = deque()

    def send(self, message: dict[str, object]) -> None:
        self.writer.write(json.dumps(message) + "\n")
        self.writer.flush()

    def receive(self) -> RpcMessage:
        if not (line := self.reader.readline()):
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
        while True:
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
    directories: list[Path] = []
    if isinstance(command.action, FixCommand):
        directories = [
            p
            for r in command.action.repos
            if (p := (Path.home() / "code" / r).resolve()).is_dir()
            and (p / "pyproject.toml").is_file()
        ]
        if not directories:
            return
    if command.dry_run:
        if isinstance(command.action, FixCommand):
            if command.action.codex:
                print("Start the desktop app's bundled codex app-server; initialize.")
            for p in directories:
                print(f"{p.name}:")
                print(
                    "    Read pyproject.toml description and first README.md heading."
                )
                print("    gh repo view: read the GitHub description.")
                if command.action.codex:
                    print("    project/list: find at most one project with this root.")
                    print(
                        "    thread/list: list all unarchived chats at this directory."
                    )
                    print("    Print TOML with codex.project and codex.chats.")
                else:
                    print("    Print local and GitHub values as TOML.")
            if command.action.codex:
                print("Stop app-server; no project information is changed.")
        else:
            print_dry_run(command.action.thread_id)
        return
    try:
        if isinstance(command.action, FixCommand) and not command.action.codex:
            for p in directories:
                report_repository(None, p)
            return
        executable = (
            "/Applications/ChatGPT.app/Contents/Resources/codex-cli/bin/codex"
            if isinstance(command.action, FixCommand)
            else "codex"
        )
        with subprocess.Popen(
            [executable, "app-server"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
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
                        "capabilities": {
                            "experimentalApi": isinstance(command.action, FixCommand)
                        },
                    },
                )
                server.send({"method": "initialized", "params": {}})
                if isinstance(command.action, FixCommand):
                    for p in directories:
                        report_repository(server, p)
                else:
                    thread_id = command.action.thread_id or select_chat(server)
                    replace_chat(server, thread_id)
            finally:
                process.terminate()
    except (
        OSError,
        RuntimeError,
        ValidationError,
        EOFError,
        tomllib.TOMLDecodeError,
        subprocess.CalledProcessError,
    ) as error:
        sys.exit(str(error))
    except KeyboardInterrupt:
        sys.exit("Interrupted; any unfinished handover has not archived the old chat.")


def report_repository(server: AppServer | None, directory: Path) -> None:
    try:
        with (directory / "pyproject.toml").open("rb") as source:
            description: object = tomllib.load(source)["project"]["description"]
    except (OSError, UnicodeDecodeError, tomllib.TOMLDecodeError, KeyError, TypeError):
        return
    if not isinstance(description, str):
        return
    remotes = subprocess.run(
        ["git", "remote"],
        cwd=directory,
        stdout=subprocess.PIPE,
        text=True,
        check=True,
    ).stdout
    if not remotes.strip():
        return
    try:
        readme = (directory / "README.md").read_text()
    except FileNotFoundError:
        heading = "(missing)"
    else:
        heading = next(
            (s[2:].strip() for s in readme.splitlines() if s.startswith("# ")),
            "(missing)",
        )
    github = subprocess.run(
        ["gh", "repo", "view", "--json", "description", "--jq", ".description"],
        cwd=directory,
        stdout=subprocess.PIPE,
        text=True,
        check=True,
    ).stdout.rstrip("\n")
    report = tomlkit.table()
    report["pyproject"] = description
    report["README"] = heading
    report["github"] = github
    if server is not None:
        report["codex"] = codex_info(server, directory).model_dump(exclude_none=True)
    document = tomlkit.document()
    document[directory.name] = report
    print(tomlkit.dumps(document))


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
