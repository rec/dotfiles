#!/bin/bash

alias black='find . -name \*.py | xargs /code/env/black/bin/black -l 79 -S'
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

alias rs='rsync --archive --verbose /Volumes/Matmos/iTunes/Music /Volumes/McLuhan/Matmos'
alias bbb='find . -name \*.py | xargs /code/env/black/bin/black -l 79 -S'
