# Git aliases

alias g=git
alias go='g go'

alias gb='git branch'
alias gc='git switch'
alias gbr='git symbolic-ref --short HEAD'

alias gi='git infer -a'
alias gi='git infer -a && git push'

alias glm='git l master..'
alias gl='git l'

alias gri='git rebase -i upstream/dev'
alias grc='git rebase --continue'
alias gs='git st'
alias gst='git st'

alias gcp='git cherry-pick'
alias gnew='git new'

alias gp='git push'
alias gpf='git push --force-with-lease'
alias gps='git push --set-upstream origin'

alias gsh='git show > /tmp/git.diff'
alias gdiff='git diff > /tmp/git.diff'

alias grs='g reset --soft HEAD~'
alias greb='git fetch upstream && git rebase upstream/dev'
alias gdam='gc master && git merge dev && git push && gc dev'

# alias gc='git commit'
alias gfix='git commit -a --fixup'
alias gca='git commit --amend'
alias gcam='git commit --amend --no-edit'
alias gcama='git commit --amend --no-edit -a'

gcop() {
    git commit $* && git push
}

gcom() {
    git commit -am "$*"
}

# Commit everything with a message and push it.
gcomp() {
    gcom $* && git push
}

gcomp-f() {
    gcom $* && git push --force-with-lease
}

# Check out a copy of the current branch under a new name and push it to your
# origin directory.
gcopy() {
    git checkout -b $1 && git push --set-upstream origin $1
}

gbs() {
    for i in $@ ; do
        git checkout -b $i && greset HEAD~ && git push --set-upstream origin $i
    done
}

alias gbss='gbs one two three four five six'
alias gbsss='gbss seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen'

greset() {
    if [ $1 ] ; then
        NAME=$1
    else
        NAME=HEAD
    fi
    git reset --hard $NAME
}

# Back up the current branch.
gback() {
    branch=`git symbolic-ref --short HEAD`
    back=b-$branch

    gdelete $back && gc $branch && gcopy $back && gc $branch
}

# Amend the previous commit to include all the changes you have currently,
# and force push. gcap is "git commit, amend, push"
# Slightly dangerous, don't use this on master.
gcap() {
    if [[ $1 ]] ; then
        echo "ERROR: gcap doesn't take any commands"
        return 1
    fi
    gcam -a && git push --force-with-lease
}

gunused() {
    gc `/code/dotfiles/python/unused_branch.py $@`
}

# Delete branches that have been merged to master.
gdelete-safe() {
    branch=`git symbolic-ref --short HEAD`

    for i in $@ ; do
        gunused $@ && git branch -d $i && git push --delete origin $i
    done

    gc $branch
}

# Delete branches that might not have been merged to master.
gdelete() {
    branch=`git symbolic-ref --short HEAD`

    for i in $@ ; do
        gunused $@ && ( git branch -D $i ; git push --delete origin $i )
    done

    gc $branch
    return 0
}

# Move an existing branch to a new name.
gmove() {
    if [[ -z "$1" ]] ; then
        echo "Usage: gmove [from] to"
        return 1
    fi

    branch=`git symbolic-ref --short HEAD`
    if [[ "$2" ]] ; then
        from=$1
        to=$2
    else
        from=$branch
        to=$1
    fi

    git checkout $from && \
        git pull && \
        git branch -m $from $to && \
        git push origin :$from && \
        git push --set-upstream origin $to && \
        git checkout $branch
}

gr() {
    if [ -z "$1" ] ; then
        commits=12
    else
        commits="$1"
    fi
    git rebase -i HEAD~$commits
}

gmaf() {
    if git diff-index --quiet HEAD -- ; then
        git commit -a --amend -m "$*" && git push --force-with-lease
    else
        echo "ERROR: Changes in your workspace would be overwritten."
     fi
}

gbase-f() {
    BASE=`/code/dotfiles/python/base_branch.py`

    git fetch upstream && git rebase upstream/$BASE
}

gbase() {
    if git diff-index --quiet HEAD -- ; then
        gbase-f
    else
        echo "ERROR: Changes in your workspace would be overwritten."
     fi
}

# List branches
_glist() {
    branch=`git symbolic-ref --short HEAD`

    for i in $@ ; do
        gc $i 1> /dev/null && git l -8 && echo
    done

    gc $branch
}

# List branches
glist() {
    if [[ $1 ]] ; then
        _glist $@
    else
        branches=`git branch | sed -e 's/*//' | xargs echo`
        echo "branches=$branches"
        _glist $branches
    fi
}

gupdate() {
    if [[ "$1" ]] ; then
        branch=`git symbolic-ref --short HEAD`
        for i in $@ ; do
            echo "gupdating: $i"
            gc "$i" && \
                git fetch upstream && \
                git rebase upstream/dev && \
                git push --force-with-lease
            if [[ -z "$?" ]] ; then
                return 1
            fi
            sleep 0.5
        done
        gc $branch
    else
        git fetch upstream && \
            git rebase upstream/dev && \
            git push --force-with-lease
    fi
}
