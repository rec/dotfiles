# Usual suspects

Snapshot of the eight repositories in `~/code`, based on their current
`pyproject.toml`, entry points, and direct service integrations. A dependency is
listed when it is declared as a project dependency; Showco's optional Lyte
service integration is called out separately because it is not declared in its
package metadata.

| Project | Direct usual-suspect dependencies | Usual suspects that depend on it | Interfaces | Reccy configuration |
| --- | --- | --- | --- | --- |
| Lyte | Reccy, Ufor | Showco optionally integrates its service | CLI: `lyte`<br>TUI: no<br>GUI: no<br>Pure library: no | Two Reccy services share `name = "lyte"`, service identity `com.swirly.lyte`, `LYTE_DAEMON`, and `\\.\pipe\lyte`; both enable RPC. `LyteMidiDaemon` publishes `LyteMidiStatus`; `InstallationService` publishes `InstallationStatus`. |
| Streamo | Reccy | Showco | CLI: `streamo`<br>TUI: no<br>GUI: no<br>Pure library: no | `Streamo` uses `name = "streamo"`, `com.swirly.streamo`, `STREAMO_DAEMON`, and `\\.\pipe\streamo`; RPC is enabled and it publishes the base `ReccyStatus`. Its TOML model supplies capture, ingest, encoding, title-card, local-display, and participant-image settings. |
| Recs | Reccy, Ufor | Showco | CLI: `recs`<br>TUI: yes, Rich live display<br>GUI: yes, optional PySide6 live window<br>Pure library: no | `ExternalServer` uses `name = "recs"`, `com.swirly.recs`, `RECS_DAEMON`, and `\\.\pipe\recs`; RPC is enabled. It accepts optional control and event endpoint overrides and publishes control, row, and waveform events. It does not set a Reccy status model. |
| Showco | Reccy, Recs, Streamo; optional runtime integration with Lyte | None | CLI: `showco`<br>TUI: no<br>GUI: yes, browser web UI<br>Pure library: no | `ShowcoDaemon` uses `name = "showco"`, `com.swirly.showco`, `SHOWCO_DAEMON`, and `\\.\pipe\showco`, with daemon module `showco`. It uses Reccy's service installation/status support only: RPC and status publishing remain disabled. Its service registry also loads the Lyte, Recs, and Streamo specifications. |
| Enge | Ufor | Tuney | CLI: no<br>TUI: no<br>GUI: no<br>Pure library: yes | No Reccy subclass. |
| Ufor | None | Lyte, Enge, Recs, Tuney | CLI: no<br>TUI: no<br>GUI: no<br>Pure library: yes | No Reccy subclass. |
| Tuney | Enge, Reccy, Ufor | None | CLI: `tuney`<br>TUI: no<br>GUI: yes, PySide6 desktop instrument<br>Pure library: no | No Reccy subclass. It uses Reccy configuration utilities, including unit parsing. |
| Reccy | None | Lyte, Streamo, Recs, Showco, Tuney | CLI: no installed command<br>TUI: no<br>GUI: no<br>Pure library: yes | Shared configuration, service, IPC/RPC, logging, and runtime library. |

## Reccy conventions

`Reccy` itself provides these shared configuration points to subclasses:

- `name` is required. Optional class settings are `service_spec`,
  `settings_model`, `status_model`, `rpc_enabled`, `rpc_role`, `logger_name`,
  and `daemon_module`.
- It stores settings at `~/.config/<name>/settings.json`. Without a service
  specification, status is at `~/.local/state/<name>/status.json` and the
  control and event endpoints are `control.sock` and `events.sock` in that
  directory.
- A service specification gives the cross-platform service identity: display
  name, description, LaunchAgent label, daemon environment variable, and
  Windows named pipe. Reccy derives the systemd unit, desktop file, metadata,
  status, socket, and log paths from it.
- Enabling RPC starts the local control and event server. The base commands are
  `status`, `mutable_attributes`, and `set_attr`; applications add their own
  commands and status model.
