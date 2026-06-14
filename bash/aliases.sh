alias read_aliases="source ~/code/dotfiles/bash/aliases.sh"

alias act="source .venv/bin/activate"

alias bui='pip uninstall -y torch && python setup.py develop'

alias ctest='pushd ~/code/test && git add . && gi ; popd'

alias cds="cd $CODE_ROOT/recs"
alias cdl="cd $CODE_ROOT/litoid"
alias cdm="cd $CODE_ROOT/multi"
alias cdr="cd $CODE_ROOT/recs"
alias cpptags="find src -name \*.h -or -name \*.hpp -or -name \*.cpp | xargs etags"

alias da="direnv allow"
# alias dboard="$CODE_ROOT/multi/.direnv/python-3.11/bin/python -m multi dashboard --push"
# alias dashboard="$CODE_ROOT/multi/.direnv/python-3.11/bin/python -m multi dashboard --push"
alias dev="ssh -p 2223 rec@eu.quansight.dev"

alias ffpmeg="ffpmeg -hide_banner"

alias gtags="find . -name \*.py -or -name git-\* | xargs etags > TAGS"

alias hg="history 250 | grep"
alias hi="history 20"

alias l="exa -lF --git"
alias lr='quiet lintrunner init && lintrunner -a | python ~/code/test/python/fix_lint.py | tee lint.grep'
alias lrp='quiet lintrunner init && lintrunner -a --take=PYREFLY | python ~/code/test/python/fix_lint.py | tee lint.grep'
alias lrr='quiet lintrunner init && lintrunner --take=MYPY,RUFF -a | python ~/code/test/python/fix_lint.py | tee lint.grep'
alias lsd='ls -d ~/git*'

alias md='g update-ref refs/head/main dev && g push origin main'
alias multi="$CODE_ROOT/multi/.venv/bin/multi"
alias mkdocs="$CODE_ROOT/multi/.venv/bin/mkdocs"

alias pcit="python -c 'import torch'"
alias pi="pip install --upgrade pip && python3 -m pip install -e .[dev]"
alias pm='python ~/code/pullman/pullman.py'
alias pop="popd; pushd ."
alias ppb="poetry publish --build"
alias py="arch -arm64 python"
alias pytags="find . -name \*.py | xargs etags"

alias q=quiet

alias recs="/Users/tom/code/recs/.venv/bin/python -m recs"

alias sb="source ~/.bash_profile"

alias ta='type -a'
alias tl='tmux list-sessions'
alias totm='echo "NO recs -R -i Mac -i FLOW+9-10 -o "/Users/tom/Documents/Work/Music/T.O.™/{ddate}/{device}""'

alias bem='recs -R -i Mac -i FLOW -o "/Users/tom/Documents/Work/Music/BEM/{ddate}/{ptime}/{channel} + {time}" -f wav -f mp3'
# alias bem='recs -R -i Mac -i FLOW -o "/Users/tom/Documents/Work/Music/BEM/{ddate}/{device}" -f wav -f mp3'

alias tm='tmux new-session -A -s'
alias ts='date "+%Y%m%d_%H%M%S"'
