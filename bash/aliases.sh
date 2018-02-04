#!/bin/bash

alias g='python2.7 /development/grit/Grit.py'
alias grit='python2.7 /development/grit/Grit.py'

alias go='grit open'
alias goc='grit open c'
alias gop='grit open pull'

alias gb='git branch'
alias gc='git checkout'
alias gcp='git cherry-pick'
alias gl='git l'
alias gs='git status'

alias gdiff="git diff > /tmp/git.diff"
alias gpo='git push -f && sleep 1 && g o c'
alias gsh='git show > /tmp/git.diff'

alias cpptags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias pytags="find . -name \*.py | xargs etags"
alias rmpyc='find . -name \*.pyc | xargs rm'

alias s1='sleep 1'
alias sb='source ~/.bashrc'
alias sc='s1 && goc'
alias ssp='ssh -l pi chopin.ax.to'

#alias format='clang-format -i -style="{BasedOnStyle: google, IndentWidth: 4}'
eval "$(thefuck --alias)"
