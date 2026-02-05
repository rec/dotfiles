export BASH_SILENCE_DEPRECATION_WARNING=1
export CHOPIN=192.168.178.108
export DEV=rec:2223@eu.quansight.dev
export DIRENV_LOG_FORMAT=
export DYLD_LIBRARY_PATH=/opt/homebrew/lib
export EDITOR=emacs
export HISTFILESIZE=100000000
export HISTSIZE=100000
export LANG=en_US.UTF-8
export PYTHONSTARTUP=$CODE_ROOT/dotfiles/bin/python_startup.py
export PYTORCH_GIT_USER=rec

if [ ! -z "$INSIDE_EMACS" ]; then
    export TERM=emacs
fi
