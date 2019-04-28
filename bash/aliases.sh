#!/bin/bash

alias cpptags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias pytags="find . -name \*.py | xargs etags"
alias rmpyc='find . -name \*.pyc | xargs rm'
alias ob='open /code/BiblioPixel/html/index.html'
alias om='open /code/swirly/max/nanolaser/delay-laser.maxpat'

alias s1='sleep 1'
alias sb='source ~/.bashrc'
alias sc='s1 && goc'
alias ssp='ssh -l pi $CHOPIN'
alias ppy='python3.5 setup.py sdist && python3.5 setup.py sdist upload'

alias kill_simpixel='sudo lsof -t -i tcp:1337 | xargs kill -9'

# alias format='clang-format -i -style="{BasedOnStyle: google, IndentWidth: 4}'
