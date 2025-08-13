# Code for fancy prompts
#
parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

if tput setaf 128 2> /dev/null ; then
    branch_c="\[$(tput setaf 95)\]"
    time_c="\[$(tput setaf 143)\]"
    user_c="\[$(tput setaf 95)\]"
    dir_c="\[$(tput setaf 21)\]"
    terminal_c="\[$(tput setaf 0)\]"
else
    branch_c="\[\033[32m\]"
    time_c="\[\033[36m\]"
    user_c="\[\033[00m\]"
    dir_c="\[\033[00m\]"
    terminal_c="\[\033[0m\]"
fi

export PS1="\
$branch_c\$(parse_git_branch)\
$time_c\D{%m/%d}-\t\
 $user_c\u@\
\h:\
$dir_c\w\$\
$terminal_c "

# export PS1="\[\033[32m\]\$(parse_git_branch)\[\033[36m\]\D{%m/%d}-\t \[\033[00m\]\u@\h:\w$ "
