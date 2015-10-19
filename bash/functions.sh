function ..() {
  cd ..
}

function ...() {
  cd ../..
}

function ....() {
  cd ../../..
}

function .....() {
  cd ../../../..
}

function ......() {
  cd ../../../../..
}

function em {
    EMACS_PROJECT=$1 emacs --name "$1 -- $2" 2>/dev/null & disown
}

function rmpyc {
   find . -name \*.pyc | xargs rm
}

function add_suffix() {
    for suffix
        do
            find . -name \*.$suffix -print0 | xargs -0 git add
    done
}

function maxcom () {
    add_suffix js maxpat maxhelp txt \
        && git commit -m "$1" \
        && git push
}

function maxcom_old () {
    find . -name \*.js -or -name \*.maxpat -or -name \*.maxhelp -or -name \*.txt \
        | xargs git add \
        && git commit -m "$1" \
        && git push
}

function gop() {
    git push -f && sleep 1 && g o c
}

function penv() {
    source /development/env/$*/bin/activate
}
