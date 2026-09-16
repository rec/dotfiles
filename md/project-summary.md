# Usual suspects

`Depends on` lists package and direct runtime dependencies. `RPC`, `Daemon`,
and `Status` describe the corresponding Reccy subclass settings: RPC enabled,
a service specification, and a custom status model.

| Project | Depends on | Dependent | CLI | TUI | GUI | Library | RPC | Daemon | Status |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| lyte | reccy, uFor | showCo | X | - | - | - | X | X | X |
| streamO | reccy | showCo | X | - | - | - | X | X | X |
| recs | reccy, uFor | showCo | X | X | X | - | X | X | - |
| showCo | reccy, recs, streamO; lyte | - | X | - | X | - | - | X | - |
| enge | uFor | tuney | - | - | - | X | - | - | - |
| uFor | - | lyte, enge, recs, tuney | - | - | - | X | - | - | - |
| tuney | enge, reccy, uFor | - | X | - | X | - | - | - | - |
| reccy | - | lyte, streamO, recs, showCo, tuney | - | - | - | X | - | - | - |

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
