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

`watch --test` also sends `Say "test"` to the most recently updated unarchived
chat on idle polls without a reset. It uses the same interval, 60 seconds by
default; use `watch --test --interval 120` to change it. Reset detection continues
normally, and a qualifying reset sends the configured reset prompt instead of
the test prompt, with at most one turn started per poll. Test prompts use the
same idle checks and require the running app's bridge. The first test
poll runs immediately. Poll logs identify test sends in their detail field.
Use `watch install --test` to enable this in the daemon; reinstall without
`--test` to disable it.

The watcher connects to the desktop app's own server through a Unix WebSocket
bridge. Run `watch install`, then fully quit and reopen the desktop app once.
Installation sets the GUI login environment's `CODEX_CLI_PATH` to an executable
launcher at `~/Library/Application Support/codexo/codex`. The login helper
restores that setting at future logins. Existing app processes retain their
old environment until restarted. Login agents have no guaranteed ordering,
so an app started automatically at login may precede the helper.

The launcher runs this script's internal `bridge` command. Main desktop
app-server launches retain the app's original arguments and environment, with
`--listen unix://PATH` added. The bridge forwards desktop stdio JSON to that
server as WebSocket messages. Auxiliary CLI commands, including version probes,
`--print-startup-config` requests (with or without `--startup-sign-out`), and
non-desktop servers, execute the original bundled CLI directly. Startup policy
JSON must reach the desktop app unchanged. The bridge
owns its server child and shuts it down when the desktop connection ends.
The private endpoint is `/tmp/codexo-UID/app-PID.sock`, where PID is the bridge
process's ID. The watcher discovers that process as a direct child of the
running desktop app. If it cannot identify exactly one bridge, it logs an
error and sends nothing. It never substitutes a separate app-server for an
idle check. A separate shared daemon is neither required nor used.

WebSocket connection setup and individual RPC requests time out after ten
seconds. Notifications do not extend an RPC deadline. Poll failures are logged
before polling continues. The existing stdio connection remains in use for
`retire` and `fix`; watch no longer sends JSON through `app-server proxy`.

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
Every poll writes a flushed log line containing a local ISO timestamp with UTC
offset, both `usedPercent` values, idle status, and whether a prompt was sent.
Idle status is checked on every connected poll, even without a reset. Values
that could not be queried are logged as `unavailable` or `unknown`, with a reason
when the app is closed, its bridge is unavailable, or a query fails. Available
partial results are retained in that poll's log; stale prior values are not used.

Daemon commands use a per-user launchd agent:

- `watch install` writes
  `~/Library/LaunchAgents/local.codexo.watch.plist` and loads it into the current
  GUI login session. The configured interval and prompt are saved as arguments.
  It starts immediately and runs at subsequent logins. Repeating installation
  replaces the saved settings and reloads both agents, unloading each first if
  it is already loaded. Removal is polled for up to ten seconds before loading
  a replacement, since `bootout` can return before teardown completes. A timeout
  stops installation with an error. No separate uninstall is needed. It also writes
  `~/Library/LaunchAgents/local.codexo.watch-environment.plist`, a RunAtLoad
  helper that runs `launchctl setenv CODEX_CLI_PATH` with the launcher's path,
  and sets that variable immediately during installation. It clears the old
  `CODEX_APP_SERVER_USE_LOCAL_DAEMON` login setting. The launcher and helper live
  outside the app bundle; app updates do not overwrite them. Updater relaunch
  inheritance still requires live verification. The quit-and-reopen reminder
  is suppressed when the running desktop app has a detected bridge process
  and its socket exists. If that check fails, installation still succeeds
  and prints the reminder.
- `watch uninstall` unloads both agents if loaded, removes both plists, and
  removes the launcher and unsets both `CODEX_CLI_PATH` and the old
  `CODEX_APP_SERVER_USE_LOCAL_DAEMON` login setting. Existing app processes
  retain their environment until restarted. A missing installation
  is an error. Either agent or plist is sufficient to count as installed.
- `watch daemon` prints launchctl status, or reports that the agent is absent
  or installed but not loaded.
- `watch restart` restarts the registered agent with its saved settings using
  `launchctl kickstart -k`. It reports an error if the service is not loaded.

Use `watch install --interval 120 --prompt "hello"` to save custom settings.
Each installation uses the supplied settings and CLI defaults for omitted options.
Plain `watch` continues to poll in the foreground. Management operations are
subcommands; the former `--install`, `--uninstall`, `--restart`, and `--daemon`
flags are no longer accepted.

Logs go to `~/Library/Logs/codexo-watch.log`. The agent runs this script using the
current Python interpreter, preserving its virtual environment. `CODEX_HOME` is
preserved when set during installation. Top-level `-d` prints the planned watch
or daemon operations without connecting, sending prompts, or installing anything.

Verification includes unit tests and live checks of the installed launcher's
startup-config response and an isolated desktop bridge. The bridge completed
its stdio handshake and accepted a second WebSocket client on the same server.
The isolated server was stopped afterward. The running desktop app was not
restarted, no daemon was installed, and no live watch prompt was sent.

## Additional work beyond the prompt

None.
