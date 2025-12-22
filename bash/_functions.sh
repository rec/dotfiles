build-stubs() {
    (cd build && cmake --build . --target torch_python_stubs)
}
