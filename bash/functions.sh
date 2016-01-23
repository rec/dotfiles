#
# cd ..: Move to the parent directory one or more times.
#

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

function .......() {
  cd ../../../../../..
}

#
# Git functions.
#
# Many of these are "slightly dangerous" so use with care.
#

# Check out a copy of the current branch under a new name and push it to your
# origin directory.
function gnew() {
    git checkout -b $1 && git push --set-upstream origin $1
}

# Amend the previous change to include all the changes you have currently.
# Slightly dangerous, don't use this on master.
function gamend() {
    git commit --amend -a --noedit
}

# Amend the previous commit to include all the changes you have currently,
# and force push. gcap is "git commit, amend, push"
# Slightly dangerous, don't use this on master.
function gcap() {
    git commit --amend -a --no-edit && git push -f
}

# Check out a fresh copy of master under a new name and push it to your origin
# directory.
function gfresh() {
    git checkout master && git pull && gnew $1
}

# Delete branches that have been merged to master.
function gdelete() {
    for i in $@
    do
        git checkout master
        git branch -d $i && git push --delete origin $i
    done
}

# Delete branches that might not have been merged to master.
function gdelete-f() {
    for i in $@
    do
        git checkout master
        git branch -D $i
        git push --delete origin $i
    done
}

# Move an existing branch to a new name.
function gmove() {
    git checkout $1 && \
        git pull && \
        git branch -m $2 && \
        git push origin :$1 && \
        git push --set-upstream origin $2
}

# Merge a branch onto master.
function gmerge() {
    git checkout master && \
        git merge --ff-only $1 && \
        git push && \
        gdelete $1
}

# Commit everything with a message and push it.
function gcom() {
    git commit -am "$*"
}

# Commit everything with a message and push it.
function gcomp() {
    git commit -am "$*" && git push
}

#
# Python virtualenv tools.
#

# Create a new virtualenv.
function nenv() {
    virtualenv /development/env/$1 && penv $1
}

# Activate a virtualenv.
function penv() {
    source /development/env/$1/bin/activate
}

#
# Experimental
#

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
