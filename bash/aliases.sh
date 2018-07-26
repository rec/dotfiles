#!/bin/bash

alias g='python2.7 /development/grit/Grit.py'
alias grit='python2.7 /development/grit/Grit.py'

alias go='grit open'
alias goc='grit open c'
alias gop='grit open pull'

alias cpptags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias pytags="find . -name \*.py | xargs etags"
alias rmpyc='find . -name \*.pyc | xargs rm'

alias s1='sleep 1'
alias sb='source ~/.bashrc'
alias sc='s1 && goc'
alias ssp='ssh -l pi $CHOPIN'

alias kill_simpixel='sudo lsof -t -i tcp:1337 | xargs kill -9'

# alias format='clang-format -i -style="{BasedOnStyle: google, IndentWidth: 4}'
