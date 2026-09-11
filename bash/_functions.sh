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
