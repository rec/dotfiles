pyr() {(
    set -e
    rm -Rf build dist

    BIN=$PENV_ROOT/util/bin

    $BIN/python setup.py sdist bdist_wheel
    $BIN/twine check dist/*
    $BIN/twine upload dist/*

    rm -Rf build dist
    head -25 CHANGELOG | pbcopy
    go n
)}

pyr3.7() {(
    set -e
    rm -Rf build dist

    BIN=$PENV_ROOT/util3.7/bin

    $BIN/python setup.py sdist bdist_wheel
    $BIN/twine check dist/*
    $BIN/twine upload dist/*

    rm -Rf build dist
    head -25 CHANGELOG | pbcopy
    go n
)}
