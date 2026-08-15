build-stubs() {
    (cd build && cmake --build . --target torch_python_stubs)
}

rp() {
    local target=lyte

    case "$PWD/" in
        "$HOME/code/lyte"/*) target=recs ;;
        "$HOME/code/recs"/*) target=showco ;;
        "$HOME/code/showco"/*) target=twitcho ;;
        "$HOME/code/twitcho"/*) target=lyte ;;
    esac

    cd "$HOME/code/$target" && act
}
