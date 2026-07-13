#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: issue.sh ISSUE_ID" >&2
  exit 2
fi

issue_json=$(gh issue view "$1" --json title,body --jq '{title, body}')

printf '# %s\n\n%s\n' \
  "$(jq -r '.title' <<<"$issue_json")" \
  "$(jq -r '.body' <<<"$issue_json")"
