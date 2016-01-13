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

function bp() {
    git checkout -b "$1" && git push --set-upstream origin "$1"
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
    git push $@ && sleep 1 && g o c
}

function penv() {
    source /development/env/$*/bin/activate
}

function gnew() {
    git checkout -b $1 && git push --set-upstream origin $1
}

function gdelete() {
    for i in "$@"
    do
        git checkout master
        git branch -D $i
        git push --delete origin $i
    done
}

function grename() {
    git checkout $1 && \
        git pull && \
        git branch -m $2 && \
        git push origin :$1 && \
        git push --set-upstream origin $2
}

function gcom () {
    git commit -am $1 && git push
}

# based on https://github.com/jimeh/git-aware-prompt
find_git_branch() {
  # Based on: http://stackoverflow.com/a/13003854/170413
  local branch
  if branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null); then
    if [[ "$branch" == "HEAD" ]]; then
      branch='detached*'
    fi
    git_branch="[$branch]"
  else
    git_branch=""
  fi
}

find_git_dirty() {
  local status=$(git status --porcelain 2> /dev/null)
  if [[ "$status" != "" ]]; then
    git_dirty='*'
  else
    git_dirty=''
  fi
}

function do_echo() {
    echo \""$@\""
}

function bformat() {
    python -c 'import shlex, sys; print " \\\n  ".join(shlex.split(" ".join(sys.argv[1:])))' $@
}

function bsort() {
    python -c 'import shlex, sys; print " \\\n  ".join(sorted(shlex.split(" ".join(sys.argv[1:]))))' $@
}
