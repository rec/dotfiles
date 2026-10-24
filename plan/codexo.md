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
directory with Project membership explicitly unverified. The older `codex` on
PATH used by `retire` has no application Project membership operation. The newer
desktop-bundled executable used by `fix` does expose Project operations.

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

Run `/Users/tom/code/dotfiles/bin/codexo.py retire` to select a chat from paginated
app-server results, or pass a thread ID after `retire`. It preserves
the display name and recorded working directory, using configured model and
permission defaults. Run only when the old chat is idle, and leave it idle in
the desktop app throughout the handover. A separate server may not observe work
running in another process.

Add the top-level `-d` or `--dry-run` flag before the command, for example
`/Users/tom/code/dotfiles/bin/codexo.py -d retire`, to print the planned operations
without starting app-server, creating files, or changing chats. This mode runs
offline, so it shows placeholders for metadata and paths that a real run would
discover or create.

`fix <repo> [<repo>...]` currently reports metadata without changing it. Bare
repository names resolve under `~/code`; absolute paths also work. Each report
prints the pyproject description, first level-one README heading (without `# `),
GitHub description, Codex Project name if present, and every unarchived chat name
at that working directory. Output is TOML, with the directory name as the top-level
table and string fields `pyproject`, `README`, and `github`. The optional nested
`codex` inline table contains `project` when present and a `chats` array of strings.
`--no-codex` omits that table; querying a directory with no chats gives `chats = []`.
Icons, backticks, and differing descriptions are preserved as found. Serialization
uses the existing tomlkit dependency. Entries that are not directories are silently
ignored, including in dry-run mode. If every entry is ignored, nothing is printed
and no external commands are run.

The four sources are read independently. Any failed source is silently omitted,
including missing or invalid pyproject metadata, missing README headings, failed
GitHub lookups, and Codex failures. GitHub stderr is captured rather than printed.
Missing README values no longer produce `(missing)` placeholders. Repos with no
available information produce no table; reporting continues with later repos.
There is no pyproject requirement or Git remote precheck for reading other sources.
Codex startup or initialization failure disables only the Codex source for that
run. Multiple Projects at one directory make that directory's Codex source fail.

`fix --no-codex <repo>...` omits Codex titles and does not start app-server.
Codex interrogation is enabled by default and can be explicitly enabled with
`fix --codex <repo>...`. These flags also control which operations dry-run prints.

`fix` uses the desktop app's bundled executable at
`/Applications/ChatGPT.app/Contents/Resources/codex-cli/bin/codex`, whose installed
schema supports `project/list`. The older `codex` on PATH used by `retire` lacks
that operation. This reads Projects through app-server rather than relying on
internal desktop state files. `gh repo view` reads the GitHub description using
the repository's configured remote. `-d fix <repo>...` prints only the planned
reads, without starting app-server or contacting GitHub.

`retire` stops if Codex requests approvals, user input, or app-only tools that
this standalone client cannot supply. Missing or empty documents, unsuccessful
turns, and invalid or mismatching read receipts prevent archiving. If failure
occurs after creating the replacement, it remains available; the script prints
its ID and the handover path so work can be recovered manually. It does not
retry, clean up chats, or continue the handed-over task automatically.

## Watch

`/Users/tom/code/dotfiles/bin/codexo.py watch` polls every 60 seconds by default.
Use `watch --interval 120 --prompt "hello"` to configure the interval in seconds
and the prompt. The default prompt is `hello`. The first poll runs immediately.

The watcher requires the desktop app to use its shared local app-server. The
installed app supports this launch mode, verified in its bundled implementation:
quit the desktop app first, then launch it with:

```sh
CODEX_APP_SERVER_USE_LOCAL_DAEMON=1 /Applications/ChatGPT.app/Contents/MacOS/ChatGPT
```

This is a setting for launching the installed desktop app, not a `config.toml`
key. The watcher connects through the bundled CLI's `app-server proxy` to
`$CODEX_HOME/app-server-control/app-server-control.sock`, defaulting to
`~/.codex/app-server-control/app-server-control.sock`. It never starts an
app-server. When the desktop app is closed or the shared socket is absent, it
waits for the next poll. A privately launched desktop app-server cannot be used
for this watcher; the idle check must observe the same server as the desktop app.

Both primary and secondary `usedPercent` values are recorded in memory. A window
qualifies when its first observed value is zero, or its most recent recorded
value was nonzero and the current value is zero. Missing windows are not treated
as zero. Either or both qualifying in the same poll causes at most one prompt.

Before sending, all loaded local chats are checked across every page. Active
chats, including those waiting for approval or input, and running background
terminals block the prompt. The watcher selects the most recently updated
unarchived chat, resumes it, verifies that it is still idle, and starts a turn
with the configured prompt. Activity is checked through API snapshots; there is
no atomic server-wide idle-and-send operation. No new chat is created.

Observations are recorded even when the app is busy or no destination chat
exists. A blocked reset is not queued for a later poll. App closure preserves
the in-memory history; restarting the watcher begins a new observation history.
Normal unchanged polls stay quiet. Successful sends and query failures are logged.

Daemon commands use a per-user launchd agent:

- `watch --install` writes
  `~/Library/LaunchAgents/local.codexo.watch.plist` and loads it into the current
  GUI login session. The configured interval and prompt are saved as arguments.
  It starts immediately and runs at subsequent logins. An existing installation
  is an error; uninstall it before installing different settings.
- `watch --uninstall` unloads the agent if loaded and removes its plist. A missing
  installation is an error.
- `watch --daemon` prints launchctl status, or reports that the agent is absent
  or installed but not loaded.

Logs go to `~/Library/Logs/codexo-watch.log`. The agent runs this script using the
current Python interpreter, preserving its virtual environment. `CODEX_HOME` is
preserved when set during installation. Top-level `-d` prints the planned watch
or daemon operations without connecting, sending prompts, or installing anything.

Implementation verification uses unit tests only: no watcher was started, no
daemon was installed, and no live watch prompt was sent.

## Additional work beyond the prompt

None.
