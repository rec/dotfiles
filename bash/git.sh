# Git aliases

# alias gc='git commit'
# alias g=git
g () {
    if [[ $# -gt 0 ]]; then git "$@"; else git st; fi
}

torch-clean() {
    python setup.py clean && \
        git clean -xdf aten build third_party torch && \
        git submodule foreach "git clean -xdf" && \
        git submodule update --init --recursive
}

alias garc='git add . && git rebase --continue'

alias gb='git branch'
alias gbo='rl g fetch origin && gb -r | grep " origin/"'
alias gbr='git symbolic-ref --short HEAD'

alias gc='git switch'
alias gca='git commit --amend'
alias gcaa='git commit --amend -a'
alias gcam='git commit --amend -m'
alias gcan='git commit --amend --no-edit'
alias gcama='git commit --amend --no-edit -a'
alias gcp='git cherry-pick'

strict() {
    rl git fetch upstream viable/strict \
        && rl git rebase upstream/viable/strict \
        && rl git submodule update --init --recursive \
        && rl lintrunner init
}

gd() {
    out=/tmp/rec/$(git symbolic-ref --short HEAD).diff
    g diff HEAD~ > $out
    echo "Written to $out"
}

alias gdiff='git diff > /tmp/git.diff'
alias gdu='g delete . && g update'

alias gf='git for-each - git log --oneline --decorate -4'
alias gfix='git commit -a --fixup'

alias gi='git infer -a && git push'

alias gl='git l'
alias glm='git l master..'

alias gmr='gr main && g merge working && gp && gr working'

gnew() {
    g switch -c $1 \
        && git push --set-upstream origin $1
}

alias go='g go'
alias gob='g go b'
alias goc='g go c'
alias goi='g go i'
alias gom='g go m 1'
alias gop='g go p'
alias got='g go t'

alias gp='git push'
alias gpf='git push --force-with-lease'
alias gps='git push --set-upstream origin'
alias gpu='git push upstream `git branch --show-current`'
alias gpuf='git push upstream --force-with-lease `git branch --show-current`'
alias gpum='git pull upstream main && git st'

alias gr='git rot'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias gri='git rebase -i upstream/dev'
alias grm='g reset --soft main'
alias grs='g restore --source=HEAD~ --'

alias gs='gl; echo; g st'
alias gsh='git show > /tmp/git.diff'
alias gsp='git split && gp'

alias gu='git update'
alias gw='git switch'

ghc() {
    if [[ $1 == https* ]] ; then
        arg=$1
    else
        arg=https://github.com/pytorch/pytorch/pull/$s
    fi
    ghstack checkout $arg
}

cg() {
    cd $CODE_ROOT/$1
}

gcamp() {
    gcam "$*" && git push --force-with-lease
}

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
    gcama && git push --force-with-lease
}

gunused() {
    gc `$CODE_ROOT/dotfiles/python/unused_branch.py $@`
}

branch-to-tag() {
    if [[ $1 ]]; then
        branch=$1
    else
        branch=$(git symbolic-ref -q --short HEAD)
    fi
    if [[ $2 ]]; then
        tag=$2
    else
        tag=branch-$branch
    fi

    msg="$branch to tag $tag"
    git tag $tag $branch && \
        git push --tag && \
        git delete $branch && \
        echo "Renamed $msg and pushed"
}

branches-to-tags() {
    for var in "$@"
    do
        branch-to-tag "$var"
    done
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
    BASE=`$CODE_ROOT/dotfiles/python/base_branch.py`

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

gversion() {
    if [ -z $1 ] ; then
        echo 'ERROR: gversion needs arguments for `poetry version`'
        return 1
    fi
    VERSION=v$(poetry version $@ | awk 'NF{ print $NF }')
    git commit pyproject.toml -m "Update version to $VERSION" \
        && git push
}

gpy() {
    VERSION=v$(poetry version | awk 'NF{ print $NF }')

    gh release create $VERSION --generate-notes \
        && git push --tag --force-with-lease \
        && git pull --tag \
        && poetry publish --build
}

gver() {
    gversion $1 && gpy
}
