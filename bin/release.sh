#!/usr/bin/env bash
set -euxo pipefail

part="${1:-patch}"

if [[ "$part" != "patch" && "$part" != "minor" && "$part" != "major" ]]; then
  echo "usage: release.sh [patch|minor|major]" >&2
  exit 2
fi

if [[ -n "$(git status --porcelain)" ]]; then
  echo "release requires a clean working tree" >&2
  exit 1
fi

branch="$(git branch --show-current)"
if [[ "$branch" != "main" ]]; then
  echo "release must run from main, not $branch" >&2
  exit 1
fi

project_name="$(
  uv run python - <<'PY'
from pathlib import Path
try:
    import tomllib
except ModuleNotFoundError:
    import tomli as tomllib

data = tomllib.loads(Path("pyproject.toml").read_text())
print(data["project"]["name"])
PY
)"
app_name="$(printf '%s' "$project_name" | awk '{print toupper(substr($0,1,1)) substr($0,2)}')"
project_dependencies="$(
  uv run python - <<'PY'
from pathlib import Path
try:
    import tomllib
except ModuleNotFoundError:
    import tomli as tomllib

def dependency_name(dependency):
    for separator in ";[=<>~!":
        dependency = dependency.split(separator, 1)[0]
    return dependency.strip().lower()

data = tomllib.loads(Path("pyproject.toml").read_text())
for dependency in data["project"].get("dependencies", []):
    print(dependency_name(dependency))
PY
)"

target_version="$(uv version --bump "$part" --dry-run --short)"
tag="v$target_version"

if git rev-parse --verify --quiet "$tag" >/dev/null; then
  echo "tag already exists: $tag" >&2
  exit 1
fi

if git ls-remote --exit-code --tags origin "refs/tags/$tag" >/dev/null; then
  echo "remote tag already exists: $tag" >&2
  exit 1
fi

ruff_paths=("$project_name")
find_paths=("$project_name")

if [[ -d install ]]; then
  ruff_paths+=(install)
  find_paths+=(install)
fi

for path in test*; do
  if [[ -e "$path" ]]; then
    ruff_paths+=("$path")
  fi
done

if [[ -d test ]]; then
  find_paths+=(test)
fi

uv run pytest
uv run ruff check --fix --select B,E,F,I "${ruff_paths[@]}"
uv run ruff format --check "${ruff_paths[@]}"
uv run ty check "$project_name"
python_version="$(cat .python-version)"
python_version="${python_version//./}"
find "${find_paths[@]}" -name '*.py' | xargs uv run pyupgrade --py"${python_version}"-plus
git diff --check

if [[ -n "$(git status --porcelain)" ]]; then
  echo "release verification changed the working tree" >&2
  exit 1
fi

build_root="${TMPDIR:-/tmp}/${project_name}-release-build"
repo_root="$(pwd)"
pyinstaller_args=(
  --noconfirm
  --distpath "$build_root/dist"
  --workpath "$build_root/build"
  --specpath "$build_root"
  --windowed
  --disable-windowed-traceback
  --name "$app_name"
)

if [[ -f "$repo_root/icon.png" ]]; then
  pyinstaller_args+=(--icon "$repo_root/icon.png")
fi

case "$project_name" in
  recs)
    pyinstaller_args+=(--hidden-import recs.ui.gui_child)
    ;;
esac

if grep -qx mido <<<"$project_dependencies"; then
  pyinstaller_args+=(--hidden-import mido.backends.rtmidi)
fi

if grep -qx pynput <<<"$project_dependencies"; then
  case "$(uname)" in
    Darwin)
      pyinstaller_args+=(
        --hidden-import pynput.keyboard._darwin
        --hidden-import pynput._util.darwin
      )
      ;;
    Linux)
      pyinstaller_args+=(
        --hidden-import pynput.keyboard._xorg
        --hidden-import pynput._util.xorg
        --hidden-import pynput._util.uinput
      )
      ;;
  esac
fi

if grep -qx sounddevice <<<"$project_dependencies"; then
  pyinstaller_args+=(
    --hidden-import _sounddevice
    --collect-binaries _sounddevice_data
  )
fi

if grep -qx soundfile <<<"$project_dependencies"; then
  pyinstaller_args+=(
    --hidden-import _soundfile
    --collect-binaries _soundfile_data
  )
fi

pyinstaller_args+=(--add-data "$repo_root/$project_name:$project_name")

for data_file in \
  README.md \
  packaging/README-WINDOWS.txt \
  packaging/README-MACOS.txt \
  packaging/README-LINUX.txt; do
  if [[ -f "$data_file" ]]; then
    pyinstaller_args+=(--add-data "$repo_root/$data_file:$(basename "$data_file")")
  fi
done

pyinstaller_args+=(install/pyinstaller_entrypoint.py)

uv run --with pyinstaller --with pillow pyinstaller "${pyinstaller_args[@]}"

if [[ "$(uname)" == "Darwin" ]]; then
  codesign --force --deep --sign - "$build_root/dist/$app_name.app"
fi

uv version --bump "$part" --no-sync
version="$(uv version --short)"
tag="v$version"

if [[ "$version" != "$target_version" ]]; then
  echo "version changed after dry run: expected $target_version, got $version" >&2
  exit 1
fi

git add pyproject.toml uv.lock
git commit -m "Release $tag"
git push
git tag "$tag"
git push origin "$tag"
