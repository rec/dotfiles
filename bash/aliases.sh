alias bbb="find . -name \*.py | xargs $CODE_ROOT/env/black/bin/black -l 79 -S"

alias cds="cd $CODE_ROOT/recs"
alias cdl="cd $CODE_ROOT/litoid"
alias cdm="cd $CODE_ROOT/multi"
alias cdr="cd $CODE_ROOT/recs"
alias cdt="cd ~/git/pytorch"
alias cpptags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"
alias cra="ssh vultacrawl.duckdns.org"

alias d="TERM=dumb direnv exec . time arch -arm64"
alias d86="TERM=dumb direnv exec . time arch -x86_64"
alias da="direnv allow"
alias dboard="$CODE_ROOT/multi/.direnv/python-3.11/bin/python -m multi dashboard --push"
alias dashboard="$CODE_ROOT/multi/.direnv/python-3.11/bin/python -m multi dashboard --push"
alias dev="ssh -p 2223 rec@eu.quansight.dev"

alias ffpmeg="ffpmeg -hide_banner"
alias ffprobe="ffprobe -hide_banner"
alias fm="python3.11 $CODE_ROOT/test/python/ffmpeg.py"

alias gtags="find . -name \*.py -or -name git-\* | xargs etags > TAGS"

alias hg="history | grep"

alias l="exa -lF --git"
alias lb='lintrunner -m $(git symbolic-ref -q --short HEAD)'

alias multi="d $CODE_ROOT/multi/.direnv/python-3.11/bin/python -m multi"
alias mkdocs="$CODE_ROOT/multi/.direnv/python-3.11.1/bin/mkdocs"

alias pi="pip install --upgrade pip && python3 -m pip install -e .[dev]"
alias pop="popd; pushd ."
alias ppb="poetry publish --build"
alias py="arch -arm64 python"
alias pytags="find . -name \*.py | xargs etags"

alias recs="$CODE_ROOT/recs/.direnv/python-3.10/bin/python -m recs"
alias rmpyc="find . -name \*.pyc | xargs rm"
alias rs="rsync --archive --verbose /Volumes/Matmos/iTunes/Music /Volumes/McLuhan/Matmos"

alias s1="sleep 1"
alias sb="source ~/.bash_profile"
alias ssp="ssh -l pi $CHOPIN"

alias ts='date "+%Y%m%d_%H%M%S"'
