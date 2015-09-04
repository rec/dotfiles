#!/bin/bash

alias g=/development/grit/Grit.py
alias grit=/development/grit/Grit.py

function em {
    EMACS_PROJECT=$1 emacs --name "$1 -- $2" 2>/dev/null & disown
}

source /development/grit/for-your-bashrc.sh

alias LT=/development/rippled/bin/LT
alias sa="sudo apt-get -y"
alias sag="sudo apt-get install -y"

alias gu='cd `'
alias gl='git l'
alias gb='\gb ripple'
alias gri='git rebase -i upstream/develop --autosquash'
alias gfu='git fetch upstream develop'
alias gup='git checkout develop && git fetch upstream develop && git pull --ff-only upstream develop && git push'
alias fresh='git checkout develop && git fetch upstream develop && git pull --ff-only upstream develop && git push && grit fresh '
alias rtags="find ripple -name \*.h -or -name \*.cpp | xargs etags"
alias gdiff="git diff > /tmp/git.diff"
alias sb='source ~/.bashrc'
alias sc='sleep 1 && g o c'
alias gpo='git push -f && sleep 1 && g o c'


function rmpyc {
   find . -name \*.pyc | xargs rm
}

function rtags () {
    find src/ripple -name \*.h -or -name \*.cpp | xargs etags
}

function gop() {
    git push -f && sleep 1 && g o c
}

function penv() {
    source /development/env/$*/bin/activate
}
