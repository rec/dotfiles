alias bbb='find . -name \*.py | xargs /code/env/black/bin/black -l 79 -S'
alias black='/Users/tom/.virtualenvs/util/bin/black -l 79 -S'

alias cde='cd /code/multi'
alias cpptags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias cra='ssh vultacrawl.duckdns.org'

alias d='TERM=dumb direnv exec . time arch -arm64'

alias ffpmeg='ffpmeg -hide_banner'
alias ffprobe='ffprobe -hide_banner'

alias gtags='find . -name \*.py -or -name git-\* | xargs etags > TAGS'

alias hg='history | grep'

alias l='exa -lF --git'

alias mkdocs='/code/multi/.direnv/python-3.11.1/bin/mkdocs'

alias pop='popd; pushd .'
alias ppb='poetry publish --build'
alias py='python -c'
alias pytags="find . -name \*.py | xargs etags"

alias rmpyc='find . -name \*.pyc | xargs rm'
alias rs='rsync --archive --verbose /Volumes/Matmos/iTunes/Music /Volumes/McLuhan/Matmos'

alias s1='sleep 1'
alias sb='source ~/.bashrc'
alias sea='ssh search.engora.tech'
alias ssp='ssh -l pi $CHOPIN'

alias ts='date "+%Y%m%d_%H%M%S"'

alias web='ssh vultaweb.duckdns.org'
