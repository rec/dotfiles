#
# Experimental
#

record() {
    /code/env/gitz/bin/termtosvg record /code/gitz/cast/git-$1.cast
}

tcom() {
    for i in $@; do
        echo $i > $i.txt && git add $i.txt && git commit -m $i && git push
    done
}


tcomp() {
    for i in $@; do
        g new -f $i && tcom $i
    done
}

# https://medium.com/the-lazy-developer/an-alias-for-new-aliases-c6500ae0f73e
function new-alias() {
    local last_command=$(echo `history |tail -n2 |head -n1` | sed 's/[0-9]* //')
    echo alias $1="'""$last_command""'" >> /code/dotfiles/bash/aliases.sh
    source /code/dotfiles/bash/aliases.sh
}

# `git add` all .js, .max, maxhelp and .txt files, commit and push.
function maxcom () {
    add_suffix js maxpat maxhelp txt \
        && git commit -m $1 \
        && git push
}

# Start a new named emacs session.
function em {
    EMACS_PROJECT=$1 emacs --name "$1 -- $2" 2>/dev/null & disown
}

# Remove all compiled .pyc files.
function rmpyc {
   find . -name \*.pyc | xargs rm
}

# `git add` every file with a suffix from the given list.
function add_suffix() {
    for suffix
        do
            find . -name \*.$suffix -print0 | xargs -0 git add
    done
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

#rmpyc() {
#   find . -name \*.pyc | xargs rm
#}

add_suffix() {
    for suffix
    do
        find . -name \*.$suffix -print0 | xargs -0 git add
    done
}

maxcom () {
    add_suffix js maxpat maxhelp txt \
        && git commit -m "$1" \
        && git push
}
