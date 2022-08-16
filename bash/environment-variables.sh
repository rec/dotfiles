export BASH_SILENCE_DEPRECATION_WARNING=1

export CHOPIN=192.168.178.108

export DIRENV_LOG_FORMAT=

export EDITOR=emacsclient
export ENGORA_BACKGROUND_MUSIC=false

export GIT_USER=rec

export HISTFILESIZE=100000
export HISTSIZE=10000

export LANG=en_US.UTF-8

export PENV_PYTHON=python3.8
export PENV_ROOT=~/.virtualenvs
export PKG_CONFIG_PATH="/usr/local/opt/qt/lib/pkgconfig"

export SQLALCHEMY_TRACK_MODIFICATIONS=False

export VIRTUALENV_CONFIG_FILE=/code/dotfiles/virtualenv.ini

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

export PS1="\[\033[32m\]\$(parse_git_branch)\[\033[00m\]\u@\h:\w$ "

export PATH=\
/Users/tom/bin:\
/code/gitz:\
/code/dotfiles/bash/:\
/code/engora-search/scripts:\

if [[ "$(/bin/hostname)" == "bantam.local" ]]; then
    PV=/Library/Frameworks/Python.framework/Versions
    PATH=\
$PV/2.7/bin:\
$PV/3.6/bin:\
$PV/3.7/bin:\
$PV/3.8/bin:\
$PV/3.9/bin:\
$PV/3.10/bin:\
/Applications/Postgres.app/Contents/Versions/13/bin:\
/usr/local/opt/openssl/bin:\
/usr/local/bin

fi

if [[ "$(/bin/hostname)" == "bolt.local" ]]; then
    PATH=$PATH:/opt/homebrew/bin
fi

PATH=$PATH:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin

export PATH

# export FLASK_APP=/code/microblog/microblog.py
# export LDFLAGS="-L/usr/local/opt/qt/lib"
# export CPPFLAGS="-I/usr/local/opt/qt/include"
# export PYTHONSTARTUP=~/bin/python_startup.py

# /usr/local/opt/qt/bin
# /opt/X11/bin:\
# /usr/local/sbin:\
# /Applications/Aquamacs.app/Contents/MacOS/bin/:\
# /opt/local/bin:\
# /Users/tom/.virtualenvs/util/bin:\
