#!/bin/bash

alias g=/development/grit/Grit.py
alias grit=/development/grit/Grit.py
alias LT=/development/rippled/bin/LT
alias sa="sudo apt-get -y"
alias sag="sudo apt-get install -y"

alias gu='cd `'
alias gl='git l'
alias gb='\gb ripple'
alias gr='git rebase -i upstream/master --autosquash'
alias gfu='git fetch upstream develop'
alias gup='git checkout develop && git fetch upstream develop && git pull --ff-only upstream develop && git push'
alias fresh='git checkout develop && git fetch upstream develop && git pull --ff-only upstream develop && git push && grit fresh '
alias rtags="find src/ripple -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias gdiff="git diff > /tmp/git.diff"
alias sb='source ~/.bashrc'
alias sc='sleep 1 && g o c'
alias gpo='git push -f && sleep 1 && g o c'
