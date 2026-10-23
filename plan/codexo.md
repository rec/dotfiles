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

The document must be saved somewhere the replacement chat can read. A local file
accessible to both chats is a possible handover mechanism; its exact location and
format have not been chosen.

## Project and working directory

The user always creates chats from the application Project and never manually
changes their working directory. The intended approach is therefore to read the
old chat's recorded working directory and supply it when creating the replacement,
without asking the user to choose a directory.

An application Project and a working directory must not be assumed to be identical.
Reusing the directory does not yet establish that the replacement will belong to
the same application Project.

## Remaining verification

- Verify that a separately launched app-server can see the existing chats in this
  desktop installation.
- Verify how to create the replacement in the same application Project, including
  whether supplying the old working directory is sufficient.
- Verify that the replacement can read the completed handover document before
  archiving the old chat.

The current chat also exposes app-provided chat management tools, but their
availability does not establish a supported interface callable directly by an
independent Python program.

## Status

Only documentation research and discussion have been completed. No Python program
has been written, no app-server has been launched, and no chats have been changed.
The documented thread operations fit the core workflow; access to this
installation's chats and application Project placement remain unverified.

## Additional work beyond the prompt

None.
