#!/bin/bash

alias g=/development/grit/Grit.py
alias grit=/development/grit/Grit.py

alias gb='git branch'
alias gdiff="git diff > /tmp/git.diff"
alias gfu='git fetch upstream develop'
alias gl='git l'
alias gpo='git push -f && sleep 1 && g o c'
alias gr='git rebase -i ripple/master --autosquash'
alias rtags="find src/ripple -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias sa="sudo apt-get -y"
alias sag="sudo apt-get install -y"
alias sb='source ~/.bashrc'
alias sc='sleep 1 && g o c'
alias format='clang-format -i -style="{BasedOnStyle: google, IndentWidth: 4}'
