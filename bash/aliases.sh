read_aliases() { source ~/code/dotfiles/bash/aliases.sh ; }

act() { source .venv/bin/activate ; }

bbb() { find . -name \*.py | xargs $CODE_ROOT/env/black/bin/black -l 79 -S ; }

ctest() { pushd ~/code/test && git add . && gi ; popd ; }

d() { TERM=dumb direnv exec . time arch -arm64 ; }
d86() { TERM=dumb direnv exec . time arch -x86_64 ; }
da() { direnv allow ; }
dboard() { $CODE_ROOT/multi/.direnv/python-3.11/bin/python -m multi dashboard --push ; }
dashboard() { $CODE_ROOT/multi/.direnv/python-3.11/bin/python -m multi dashboard --push ; }
dev() { ssh -p 2223 rec@eu.quansight.dev ; }

ffpmeg() { ffpmeg -hide_banner ; }
ffprobe() { ffprobe -hide_banner ; }
fm() { python3.11 $CODE_ROOT/test/python/ffmpeg.py ; }

gtags() { find . -name \*.py -or -name git-\* | xargs etags > TAGS ; }

hg() { history 250 | grep ; }
hi() { history 20 ; }

l() { exa -lF --git ; }
lr() { quiet lintrunner init && lintrunner -a | python ~/code/test/python/fix_lint.py | tee lint.grep ; }
lrr() { quiet lintrunner init && lintrunner --take=MYPY,RUFF -a | python ~/code/test/python/fix_lint.py | tee lint.grep ; }
lsd() { ls -d ~/git* ; }

multi() { d $CODE_ROOT/multi/.direnv/python-3.11/bin/python -m multi ; }
mkdocs() { $CODE_ROOT/multi/.direnv/python-3.11.1/bin/mkdocs ; }

pcit() { python -c 'import torch' ; }
pi() { pip install --upgrade pip && python3 -m pip install -e .[dev] ; }
pm() { python ~/code/pullman/pullman.py ; }
pop() { popd; pushd . ; }
ppb() { poetry publish --build ; }
py() { arch -arm64 python ; }
pytags() { find . -name \*.py | xargs etags ; }

q() { quiet ; }

recs() { $CODE_ROOT/recs/.direnv/python-3.10/bin/python -m recs ; }
rs() { rsync --archive --verbose /Volumes/Matmos/iTunes/Music /Volumes/McLuhan/Matmos ; }

s1() { sleep 1 ; }
sb() { source ~/.bash_profile ; }
ssp() { ssh -l pi $CHOPIN ; }

ta() { type -a ; }
tl() { tmux list-sessions ; }
totm() { echo "NO recs -R -i Mac -i FLOW+9-10 -o "/Users/tom/Documents/Work/Music/T.O.™/{ddate}/{device}"" ; }
tm() { tmux new-session -A -s ; }
ts() { date "+%Y%m%d_%H%M%S" ; }
