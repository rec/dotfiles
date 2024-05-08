export BASH_SILENCE_DEPRECATION_WARNING=1

export CHOPIN=192.168.178.108

export DIRENV_LOG_FORMAT=

export DYLD_LIBRARY_PATH=/opt/homebrew/lib

export EDITOR=emacsclient
export ENGORA_COLORIZE=false

export HISTFILESIZE=100000000
export HISTSIZE=100000

export LANG=en_US.UTF-8

export PYTHONSTARTUP=/code/dotfiles/bin/python_startup.py

export TEST_SUPERDUPERDB_COMMITS=false

export VIRTUALENV_CONFIG_FILE=/code/dotfiles/virtualenv.ini

if [ ! -z "$INSIDE_EMACS" ]; then
    export TERM=emacs
fi

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

export PS1="\[\033[32m\]\$(parse_git_branch)\[\033[00m\]\u@\h:\w$ "

PV=/Library/Frameworks/Python.framework/Versions

export PATH=\
~/.local/bin:\
~/bin:\
/code/gitz:\
/code/dotfiles/bin/:\
/usr/local/bin:\
/opt/homebrew/bin:\
/opt/homebrew/opt/postgresql@13/bin:\
/opt/homebrew/opt/ruby/bin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin

export DEV=rec:2223@eu.quansight.dev

# /usr/local/texlive/2023basic/bin/universal-darwin
