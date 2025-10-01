#
# Experimental
#

wt() {
    pushd ~/code/test/ && \
        gi
    popd
}

wat() {
    pushd ~/code/test/ && \
        git add . && \
        gi
    popd
}

all() {
    cmd=${@:-g l -1}
    pushd ~ > /dev/null

    for p in git*/pytorch; do
        echo; echo ~/$(dirname $p)
        cd $p
        $cmd
        cd
    done
    popd > /dev/null
}

cdt() {
    cd ~/git${PYTORCH_BUILD_SUFFIX}/pytorch
}

ca() {
    conda activate pytorch-dev${PYTORCH_BUILD_SUFFIX}
}

c() {
    if [ -z $1 ] || [[ $1 = -* ]]; then
        export PYTORCH_BUILD_SUFFIX=$1
    else
        export PYTORCH_BUILD_SUFFIX=-$1
    fi
    cdt && ca
}

t() {
    if [ -z $1 ]; then
        tmux list-sessions
    else
        tmux attach -t $@
    fi
}

tn() {
    if [ -z $1 ]; then
        tmux list-sessions
    else
        tmux new-session -A -s $@
    fi
}

build() {
    cd ~/git/torch-build \
        && ./pytorch-build.sh \
        && cdt \
        && echo "$(parse_git_branch)$(git rev-parse HEAD) $(pwd -P)" >> ~/compilations.txt
}

export GIT_API_ROOT=https://api.github.com/repos/pytorch/pytorch

_git_api() {
    curl \
        -L \
        -H "Accept: application/vnd.github+json" \
        -H "Authorization: Bearer $GIT_TOKEN" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        $GIT_API_ROOT/$1?per_page=2
}

load-log() {
    if [[ -z "$1" ]] ; then
        echo "Usage: load-log <JOB ID>"
        return 1
    fi
    _git_api actions/jobs/$1/logs
}

path() {
    echo $PATH | \
        python3 -c 'import sys; print(*sys.stdin.read().strip().split(":"), sep="\n")'
}

flac2mp3() {
    find . \
         -type f \
         -name '*.flac' \
         -print \
         -exec sh -c \
         'i="{}"; ffmpeg -i "$i" -y -v 0 -vcodec copy -acodec alac "${i%.flac}".m4a && rm -f "$i"' \;
}

wav2mp3() {
    find . \
         -type f \
         -name '*.wav' \
         -print \
         -exec sh -c \
         'i="{}"; ffmpeg -i "$i" -b:a 256k -ar 48000 "${i}".mp3 && rm -f "$i"' \;
}


pyv() {
    if [[ -z "$1" ]] ; then
        echo "Usage: pver <version number>"
        return 1
    fi
    echo "layout python python$1" > .envrc
    direnv allow
}

poa() {
    if [ "$#" == 1 ]; then
        msg="Add $@ dependency"
    else
        files=$(echo "$@" | sed "s/ /, /g")
        msg="Add dependencies $files"
    fi

    poetry add $@ &&\
         git commit pyproject.toml poetry.lock -m "$msg" &&\
         git push
}

e() {
    $EDITOR -n $@
}

filepy() {
    python -c "import $1; print($1.__file__)"
}

openpy() {
    if [[ -z "$1" ]] ; then
        echo "Usage: openpy <module>"
        return 1
    fi
    $EDITOR -n $(filepy $1)
}

record() {
    $CODE_ROOT/env/gitz/bin/termtosvg record $CODE_ROOT/gitz/cast/git-$1.cast
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

add_suffix() {
    for suffix
    do
        find . -name \*.$suffix -print0 | xargs -0 git add
    done
}

gdd() {
    if git diff-index --quiet HEAD -- ; then
        echo unchanged
    else
        echo git changes
    fi
}


ffm() {
    if [ -z "$2" ] ; then
        echo "USAGE: ffm <infile> <outfile> [bandwidth in k]"
        return 1
    fi
    if [ $3 ] ; then
        bps=$3k
    else
        bps=6400k
    fi
    ffmpeg -i "$1" -c:v h264_videotoolbox -b:v $bps "$2"
}

branch-to-tag() {
    if [[ $1 ]]; then
        branch=$1
    else
        branch=$(git symbolic-ref -q --short HEAD)
    fi
}

# find . -type f -name '*.flac' -print -exec sh -c 'i="{}";

flac-to-m4a() {
    find .\
         -type f\
         -name '*.flac'\
         -print\
         -exec sh -c '\
             i="{}"; \
             ffmpeg -i "$i" -y -v 0 -vcodec copy -acodec alac  "${i%.flac}".m4a && \
             rm -f "$i"' \;
}

m4a-to-mp3() {
    find .\
         -type f\
         -name '*.m4a'\
         -print\
         -exec sh -c '\
             i="{}"; \
             ffmpeg -i "$i" -q:a 8 "${i%.m4a}".mp3 && \
             rm -f "$i"' \;
}
