# codexo: Codex chat maintenance from Python

## Goal

Use a Python program running on this Mac to maintain Codex chats in the desktop
application. Ordinary ChatGPT chats are outside the scope.

The desired workflow is:

1. Select an existing chat and read its display name.
2. Send it a prompt asking it to create a handover document.
3. Wait for the document to be created.
4. Create a new chat in the same application Project with the same display name.
5. Tell the new chat to read the handover document.
6. Archive the old chat after the handover succeeds.

## Documented interface

[Codex app-server](https://learn.chatgpt.com/docs/app-server) is the relevant
programmatic interface. A local Python program can launch `codex app-server`
and exchange JSON-RPC messages over stdin/stdout. This is a protocol interface,
rather than a requirement to use a dedicated Python SDK.

The documented operations cover the core sequence:

| Purpose | Operation |
| --- | --- |
| Find and inspect the old chat | `thread/list`, `thread/read` |
| Read its display name | `thread.name` in the returned thread |
| Resume it and request a document | `thread/resume`, `turn/start` |
| Wait for generation to finish | Observe `turn/completed` and check success |
| Create the replacement | `thread/start` |
| Set the replacement's display name | `thread/name/set` |
| Request that it read the document | `turn/start` with the accessible document path |
| Archive the old chat | `thread/archive` |

The script creates a unique temporary directory and asks the old chat to save
`handover.md` there. The file remains available after the script exits, until the
system cleans its temporary directory. The replacement must return a matching
SHA-256 checksum before the old chat is archived.

## Project and working directory

The user always creates chats from the application Project and never manually
changes their working directory. The intended approach is therefore to read the
old chat's recorded working directory and supply it when creating the replacement,
without asking the user to choose a directory.

An application Project and a working directory must not be assumed to be identical.
Reusing the directory does not establish that the replacement will belong to
the same application Project. The user has accepted preserving the working
directory with Project membership explicitly unverified. The installed app-server
schema has no application Project membership operation.

## Remaining verification

- Verify that a separately launched app-server can see the existing chats in this
  desktop installation.
- Verify the complete handover on live chats. Unit tests cover the archive gate,
  but do not establish desktop integration or model behavior.

The current chat also exposes app-provided chat management tools, but their
availability does not establish a supported interface callable directly by an
independent Python program.

## Status

Implemented in `bin/codexo.py`, using the existing dotfiles environment's Tyro
and Pydantic dependencies. The script launches `codex app-server` over stdio.
No live app-server session has been launched and no chats have been changed
during implementation. The installed protocol schema was generated and inspected.

Run `/Users/tom/code/dotfiles/bin/codexo.py` to select a chat from paginated
app-server results, or pass a thread ID as the positional argument. It preserves
the display name and recorded working directory, using configured model and
permission defaults. Run only when the old chat is idle, and leave it idle in
the desktop app throughout the handover. A separate server may not observe work
running in another process.

Add `-d` or `--dry-run` to print the planned operations without starting
app-server, creating files, or changing chats. This mode runs offline, so it shows
placeholders for metadata and paths that a real run would discover or create.

The script stops if Codex requests approvals, user input, or app-only tools that
this standalone client cannot supply. Missing or empty documents, unsuccessful
turns, and invalid or mismatching read receipts prevent archiving. If failure
occurs after creating the replacement, it remains available; the script prints
its ID and the handover path so work can be recovered manually. It does not
retry, clean up chats, or continue the handed-over task automatically.

## Additional work beyond the prompt

None.
