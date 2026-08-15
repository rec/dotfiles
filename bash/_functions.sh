build-stubs() {
    (cd build && cmake --build . --target torch_python_stubs)
}

rotp() {
    local project
    local root

    for project in lyte recs showco twitcho; do
        root="$HOME/code/$project"
        case "$PWD/" in
            "$root"/*)
                case "$project" in
                    lyte) cd "$HOME/code/recs" ;;
                    recs) cd "$HOME/code/showco" ;;
                    showco) cd "$HOME/code/twitcho" ;;
                    twitcho) cd "$HOME/code/lyte" ;;
                esac
                return
                ;;
        esac
    done

    cd "$HOME/code/lyte"
}
