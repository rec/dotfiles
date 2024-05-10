export BASH_SILENCE_DEPRECATION_WARNING=1
export CHOPIN=192.168.178.108
export DEV=rec:2223@eu.quansight.dev
export DIRENV_LOG_FORMAT=
export DYLD_LIBRARY_PATH=/opt/homebrew/lib
export EDITOR=emacsclient
export HISTFILESIZE=100000000
export HISTSIZE=100000
export LANG=en_US.UTF-8
export PYTHONSTARTUP=$CODE_ROOT/dotfiles/bin/python_startup.py

if [ ! -z "$INSIDE_EMACS" ]; then
    export TERM=emacs
fi

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

export PS1="\[\033[32m\]\$(parse_git_branch)\[\033[00m\]\u@\h:\w$ "
