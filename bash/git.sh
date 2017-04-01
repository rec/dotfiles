#
# Git functions.
#
# Many of these are "slightly dangerous" so use with care.
#

# Commit everything with a message.
function gcom() {
    git commit -am "$*"
}

# Commit everything with a message and push it.
function gcomp() {
    git commit -am "$*" && \
        git push
}

# Check out a copy of the current branch under a new name and push it to your
# origin directory.
function gnew() {
    git checkout -b $1 && \
        git push --set-upstream origin $1
}

# Commit everything, then revert to the previous commit.
# Think of it as a safer reset.

function greset() {
    git commit -am "reset" && \
        git reset --hard HEAD^
}

# Back up the current branch.
function gback() {
    BRANCH=`git symbolic-ref --short HEAD`
    BACKUP=b-$BRANCH

    gdelete-f $BACKUP&& \
        git checkout $BRANCH && \
        gnew $BACKUP && \
        git checkout $BRANCH
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
    git commit --amend -a --no-edit && \
        git push -f
}

function gfresh-f() {
    BASE=`/development/dotfiles/python/base_branch.py`

    git checkout -b $1 && \
        git fetch upstream && \
        git reset --hard upstream/$BASE && \
        git push --set-upstream origin $1
}

# Check out a fresh copy of master under a new name and push it to your origin
# directory.
function gfresh() {
    if git diff-index --quiet HEAD -- ; then
        gfresh-f "$1"
    else
        echo "ERROR: Changes in your workspace would be overwritten."
     fi
}

# Delete branches that have been merged to master.
function gdelete() {
    BRANCH=`git symbolic-ref --short HEAD`

    for i in $@
    do
        git checkout `/development/dotfiles/python/base_branch.py` && \
            git branch -d $i && \
            git push --delete origin $i
    done

    git checkout $BRANCH
}

# Delete branches that might not have been merged to master.
function gdelete-f() {
    BRANCH=`git symbolic-ref --short HEAD`

    for i in $@
    do
        git checkout `/development/dotfiles/python/base_branch.py` && \
            git branch -D $i && \
            git push --delete origin $i
    done

    git checkout $BRANCH
}

# Move an existing branch to a new name.
function gmove() {
    BRANCH=`git symbolic-ref --short HEAD`

    git checkout $1 && \
        git pull && \
        git branch -m $1 $2 && \
        git push origin :$1 && \
        git push --set-upstream origin $2

    git checkout $BRANCH
}

function gri() {
    git rebase -i HEAD~$1
}

function gr() {
    gri 10
}
