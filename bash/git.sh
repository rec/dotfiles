# Git aliases


# alias gc='git commit'
alias g=git

alias garc='git add . && git rebase --continue'

alias gb='git branch'
alias gbr='git symbolic-ref --short HEAD'

alias gc='git switch'
alias gca='git commit --amend'
alias gcaa='git commit --amend -a'
alias gcam='git commit --amend --no-edit'
alias gcama='git commit --amend --no-edit -a'
alias gcp='git cherry-pick'

alias gdiff='git diff > /tmp/git.diff'
alias gdu='g delete . && g update'

alias gf='git for-each - git log --oneline --decorate -4'
alias gfix='git commit -a --fixup'

alias gi='git infer -a && git push'

alias gl='git l'
alias glm='git l master..'

alias gnew='git new'

alias go='g go'
alias gob='g go b'
alias goc='g go c'
alias gop='g go p'

alias gp='git push'
alias gpf='git push --force-with-lease'
alias gps='git push --set-upstream origin'
alias gpu='git push upstream `git branch --show-current`'
alias gpuf='git push upstream --force-with-lease `git branch --show-current`'
alias gpum='git pull upstream main'

alias gr='git rot'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias gri='git rebase -i upstream/dev'
alias grm='g reset --soft main'
alias grs='g reset --soft HEAD~'

alias gs='git st'
alias gsh='git show > /tmp/git.diff'
alias gsp='git split'
alias gsp='git split && gp'

alias gu='git update'
alias gw='git switch'

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

gcompf() {
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

gre() {
    if [ -z "$1" ] ; then
        commits=16
    else
        commits="$1"
    fi
    git rebase -i HEAD~$commits
}

grec() {
    git \
        -c rebase.instructionFormat='%s%nexec GIT_COMMITTER_DATE="%cD" git commit --amend --no-edit' \
        rebase -i $@
}

gren() {
    if [ -z "$1" ] ; then
        commits=16
    else
        commits="$1"
    fi
    grec HEAD~$commits
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
