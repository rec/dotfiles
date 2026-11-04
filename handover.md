# Handover for the next context

The latest request is to retire this context by reviewing documentation and
writing this handover. That work is complete; there is no new feature request
waiting to be implemented. The documentation review was committed and pushed
as `b1bfbd6` (`Update doc/ and plan/`). Read the retained plan for the current
interface, known limitations, operational setup, and remaining verification.

## Decisions and user expectations

- The user personally rewrote the retirement prompts to clean up documentation,
  commit a handover, and ask the replacement for a short summary. When asked
  about the resulting checksum mismatch, they explicitly chose summary-only
  behavior and removal of checksum verification. Do not restore the old receipt
  contract or rewrite their prompt text without a new request.
- Repeated manual desktop restarts after updates are not an acceptable permanent
  solution to the user. Future work on launch persistence should address ordinary
  launch and updater behavior, rather than treating another manual launch as a
  completed solution.
- The background server in the `nmr` chat was intentional. Do not stop that server
  merely to make a reset prompt eligible for delivery.

## Operational uncertainties

- After the executable was renamed to `codexo`, the user was told to reinstall
  the watcher and reopen the desktop app. This chat contains no confirmation
  that they completed those steps after the rename. If a bridge or daemon path
  fails, establish the installed paths before assuming a new protocol defect.
- Earlier diagnostic extracts are in `/private/tmp/codexo-inspect/`; the pin and
  section protocol schema was generated in `/private/tmp/codexo-pin-schema/`.
  These are temporary snapshots of an earlier desktop version. They may have
  disappeared or become stale after an app update.
- Commit IDs quoted in older chat replies no longer all match the current
  repository history. Resolve relevant commits from the current Git log.

## Verification and workspace state

The last code validation performed in this chat, after the executable rename,
passed 223 tests and 9 subtests, plus Ruff, type checks, pyupgrade, and diff
checks. That run predates the repository's subsequent Python 3.15 update; do
not present it as validation of the current interpreter. This documentation-only
turn ran diff checks and did not rerun Python tests or execute retirement.

At the start of this retirement request, `git-commit-ids/git-commit-ids.txt` had
an unrelated local change. It was left out of both requested commits. Preserve
any such work that remains when the new context starts.
