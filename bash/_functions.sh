build-stubs() {
    (cd build && cmake --build . --target torch_python_stubs)
}

r() {
    local current_project target
    local -a projects

    read -r -a projects <<< "$R_PROJECTS"
    target=${projects[0]}

    for ((i = 0; i < ${#projects[@]}; i++)); do
        current_project=${projects[i]}
        if [[ "$PWD/" == "$HOME/code/$current_project/"* ]]; then
            target=${projects[(i + 1) % ${#projects[@]}]}
            break
        fi
    done

    cd "$HOME/code/$target" && act
}

sleep-safe() {
  disks=$(diskutil list external physical | awk '/^\/dev\// { sub("/dev/", "", $1); print $1 }')

  for disk in $disks; do
    diskutil eject "$disk" || return 1
  done

  pmset sleepnow
}

commits-today() {
    local repo count total=0

    for repo in "$@"; do
        git -C "$repo" rev-parse --git-dir >/dev/null 2>&1 || continue
        count=$(git -C "$repo" rev-list --all --count --since=midnight) || return
        (( count > 0 )) || continue
        printf '%s: %s\n' "$repo" "$count"
        total=$((total + count))
    done

    printf 'Total: %s\n' "$total"
}
