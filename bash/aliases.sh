#!/bin/bash

alias g=/development/grit/Grit.py
alias grit=/development/grit/Grit.py

alias gb='git branch'
alias gc='git checkout'
alias gdiff="git diff > /tmp/git.diff"
alias gl='git l'
alias gpo='git push -f && sleep 1 && g o c'
alias gr='git rebase -i origin/master --autosquash'
alias rtags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias sa="sudo apt-get -y"
alias sag="sudo apt-get install -y"
alias sb='source ~/.bashrc'
alias sc='sleep 1 && g o c'
alias format='clang-format -i -style="{BasedOnStyle: google, IndentWidth: 4}'
alias rmpyc='find . -name \*.html | xargs rm'
