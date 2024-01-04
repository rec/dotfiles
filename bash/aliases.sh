alias bbb='find . -name \*.py | xargs /code/env/black/bin/black -l 79 -S'

alias cds='cd /code/recs'
alias cdl='cd /code/litoid'
alias cdm='cd /code/multi'
alias cdr='cd /code/recs'
alias cpptags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias cra='ssh vultacrawl.duckdns.org'

alias d='TERM=dumb direnv exec . time arch -arm64'
alias d86='TERM=dumb direnv exec . time arch -x86_64'
alias da='direnv allow'
alias dboard='/code/multi/.direnv/python-3.11/bin/python -m multi dashboard --push'
alias dashboard='/code/multi/.direnv/python-3.11/bin/python -m multi dashboard --push'

alias ffpmeg='ffpmeg -hide_banner'
alias ffprobe='ffprobe -hide_banner'
alias fm='python3.11 /code/test/python/ffmpeg.py'

alias gtags='find . -name \*.py -or -name git-\* | xargs etags > TAGS'

alias hg='history | grep'

alias l='exa -lF --git'

alias multi='d /code/multi/.direnv/python-3.11/bin/python -m multi'
alias mkdocs='/code/multi/.direnv/python-3.11.1/bin/mkdocs'

alias pi='pip install --upgrade pip && python3 -m pip install -e .[dev]'
alias pop='popd; pushd .'
alias ppb='poetry publish --build'
alias py='arch -arm64 python'
alias pytags="find x -name \*.py | xargs etags"

alias recs='/code/recs/.direnv/python-3.10/bin/python -m recs'
alias rmpyc='find . -name \*.pyc | xargs rm'
alias rs='rsync --archive --verbose /Volumes/Matmos/iTunes/Music /Volumes/McLuhan/Matmos'

alias s1='sleep 1'
alias sb='source ~/.bashrc'
alias sea='ssh search.engora.tech'
alias ssp='ssh -l pi $CHOPIN'

alias ts='date "+%Y%m%d_%H%M%S"'
