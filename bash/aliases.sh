#!/bin/bash

alias d=docker
alias dc=docker-compose
alias dcup='docker-compose up --abort-on-container-exit'


alias black='find . -name \*.py | xargs /code/env/black/bin/black -l 79 -S'
alias cpptags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias pytags="find . -name \*.py | xargs etags"
alias rmpyc='find . -name \*.pyc | xargs rm'
alias ob='open /code/BiblioPixel/html/index.html'
alias om='open /code/swirly/max/nanolaser/delay-laser.maxpat'
alias git-tags='find . -name \*.py -or -name git-\* | xargs etags > TAGS'
alias hg='history | grep'

alias simp='/code/simp/simp'

alias s1='sleep 1'
alias sb='source ~/.bashrc'
alias sc='s1 && goc'
alias ssp='ssh -l pi $CHOPIN'
# alias ppy='python3.5 setup.py sdist && python3.5 setup.py sdist upload'
alias ppy='\
rm -Rf build dist &&\
/code/env/pypi/bin/python setup.py sdist bdist_wheel &&\
 /code/env/pypi/bin/twine upload dist/* &&\
 rm -Rf dist\
'

alias rs='rsync --archive --verbose /Volumes/Matmos/iTunes/Music /Volumes/McLuhan/Matmos'
alias bbb='find . -name \*.py | xargs /code/env/black/bin/black -l 79 -S'
alias black='/code/env/black/bin/black -l 79 -S'
alias tp=/code/dotfiles/bash/test_python.sh
