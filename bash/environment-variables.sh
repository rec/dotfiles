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

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

if [ tput setaf 128 2> /dev/null ] ; then
    branch_c="\[$(tput setaf 226)\]"
    time_c="\[$(tput setaf 226)\]"
    user_c="\[$(tput setaf 226)\]"
    dir_c="\[$(tput setaf 226)\]"
else
    branch_c="\[\033[32m\]"
    time_c="\[\033[36m\]"
    user_c="\[\033[00m\]"
    dir_c="\[\033[00m\]"
fi

export PS1="\
$branch_c\$(parse_git_branch)\
$time_c\D{%m/%d}-\t\
 $user_c\u\
$dir_c\h:\
$dir_c\w$ "

# export PS1="\[\033[32m\]\$(parse_git_branch)\[\033[36m\]\D{%m/%d}-\t \[\033[00m\]\u@\h:\w$ "
