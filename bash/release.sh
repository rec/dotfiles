pyr-old() {(
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

pyr() {(
    set -e
    rm -Rf build dist

    BIN=/code/util/.direnv/python-3.7.9/bin

    $BIN/python setup.py sdist
    $BIN/twine check dist/*
    $BIN/twine upload dist/*

    rm -Rf build dist
    head -25 CHANGELOG | pbcopy
    go n
)}
