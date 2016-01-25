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
    git commit -am "$*" && git push
}

# Check out a copy of the current branch under a new name and push it to your
# origin directory.
function gnew() {
    git checkout -b $1 && git push --set-upstream origin $1
}

# Back up the current branch.
function gback() {
    BRANCH=`git symbolic-ref --short HEAD`
    BACKUP=b-$BRANCH
    gdelete-f $BACKUP
    git checkout $BRANCH
    gnew $BACKUP
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
