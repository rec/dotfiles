# Git aliases
read_git() { source ~/code/dotfiles/bash/git.sh ; }

psp() { ~/code/psplit/psplit.py ; }

_gclean() {
    git clean -xfd
    git reset --hard
    git submodule foreach --recursive git clean -xfd
    git submodule foreach --recursive git reset --hard
}

gclean() { quiet _gclean ; }

# alias gc='git commit'
# alias g=git
g() {
    if [[ $# -gt 0 ]]; then git "$@"; else git st; fi
}

strict() {
    git fetch upstream viable/strict \
        && git rebase upstream/viable/strict \
        && git submodule update --init --recursive
}

mstrict() {
    git fetch upstream main \
        && git rebase upstream/main \
        && git submodule update --init --recursive
}

torch-clean() {
    python setup.py clean && \
        git clean -xdf aten build third_party torch && \
        git submodule foreach "git clean -xdf" && \
        git submodule update --init --recursive
}

garc() { git add . && git rebase --continue ; }

gb() { git branch ; }
gbi() { git bisect ; }
gbis() { git bisect start HEAD ; }
gbiru() { git bisect run ; }
gbir() { git bisect reset ; }
gbo() { gb -r | grep " origin/" ; }
gbr() { git symbolic-ref --short HEAD ; }
ghs() { ghstack submit -u ; }

gc() { git switch ; }
gca() { git commit --amend ; }
gcaa() { git commit --amend -a ; }
gcam() { git commit --amend -m ; }
gcan() { git commit --amend --no-edit ; }
gcama() { git commit --amend --no-edit -a ; }
gcp() { git cherry-pick ; }

gcon() { git st | grep -w UU ; }

gd() {
    out=/tmp/rec/$(git symbolic-ref --short HEAD).diff
    g diff HEAD~ > $out
    echo $out
}

gdiff() { git diff > /tmp/git.diff ; }
gdu() { g delete . && g update ; }

gfx() { git commit --fixup ; }
gf() { g fetch upstream && g fetch ; }

gi() { git infer -a && git push ; }

gl() { git l ; }
glm() { git l master.. ; }

gmr() { gr main && g merge working && gp && gr working ; }

go() { g go ; }
gob() { g go b ; }
goc() { g go c ; }
goi() { g go i ; }
gom() { g go m 1 ; }
gop() { g go p ; }
got() { g go t ; }

gp() { git push ; }
gpf() { git push --force-with-lease ; }
gps() { git push --set-upstream origin ; }
gpu() { git push upstream `git branch --show-current` ; }
gpuf() { git push upstream --force-with-lease `git branch --show-current` ; }
gpum() { git pull upstream main && git st ; }

gr() { git rot ; }
gra() { git rebase --abort ; }
grc() { GIT_EDITOR=true git rebase --continue ; }
gri() { git rebase -i upstream/dev ; }
grm() { g reset --soft main ; }
grs() { g restore --source=HEAD~ -- ; }

gs() { gl; echo; g st ; }
gsh() { git show > /tmp/git.diff ; }
gsn() { git show --name-only ; }
gsp() { git split && gp ; }

gu() { git update ; }

gv() { git revert --no-edit ; }

gw() { git switch ; }

grh() {
    if [ -z "$1" ] ; then
        count=1
    else
        count="$1"
    fi
    git reset --hard HEAD@{$count}
}

ghc() {
    if [[ $1 == https* ]] ; then
        arg=$1
    else
        arg=https://github.com/pytorch/pytorch/pull/$s
    fi
    ghstack checkout $arg
}

ghcs() {
    ghc $* && strict
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
