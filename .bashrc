source ~/.profile

alias s="ssh -l tritchford tritchford-macbookpro.local"
alias x="ssh -l admin ax.to"
export EDITOR="emacs -nw"
alias python=python2.7-32
export ROOT=/development
export PATH="/usr/local/git/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/Users/tom/bin:$ROOT/rec/scripts:/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin"
# /opt/local/bin/fortune
alias git=hub
export APPENGINE="/Speedy applications/GoogleAppEngineLauncher.app/Contents/Resources/GoogleAppEngine-default.bundle/Contents/Resources"
#export PYTHONPATH="$APPENGINE/google_appengine:$APPENGINE/google_appengine/lib"
#export PYTHONPATH=""
export D=/development

alias pr="pushd $ROOT"
alias p="popd"
alias new="$ROOT/rec/scripts/new/new.py"
alias Aquamacs="/Applications/Aquamacs.app/Contents/MacOS/Aquamacs"
alias echomesh="python /development/echomesh/code/python/Echomesh.py"
alias em="python /development/echomesh/code/python/Echomesh.py"

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../.././../..."

alias server-backup="rsync --verbose --archive --compress root@ax.to:/ --bwlimit=200 --exclude=/proc --exclude=/etc --exclude=/dev /Users/tom/Documents/backups/server/"
alias gdiff="git diff > /tmp/git.diff"
alias new="$ROOT/rec/scripts/new/new.py"

export MANPATH=/usr/local/git/man:$MANPATH
export PS1="\t [\h:\w]\$ "
alias commit="git commit -a -m"
alias emacs="\\emacs -nw"
alias regen="makeAllFiles.sh"

untab() {
   cat $1 | python -c "import sys,re;[sys.stdout.write(re.compile('\t').sub('  ', line)) for line in sys.stdin]" > $1.tab
   mv $1 $1.bak
   mv $1.tab $1
}

alias lint="find . \( -name \*.h -or -name \*.cpp \) -and -not -path ./data/yaml/\* | xargs /development/rec/scripts/cpplint.py"

alias GitTwit="python2.7-32 /development/git-twit/GitTwit.py"