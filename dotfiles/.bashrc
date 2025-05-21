SECRETS=~/.secrets.sh

if [ -f $SECRETS ]; then
    source $SECRETS
fi

IFS=. read -ra hosts <<< $(hostname)
set -- $hosts
host=$1

export CODE_ROOT=~/code
bash_root=$CODE_ROOT/dotfiles/bash

if [[ "$host" == "bolt" ]]; then
    . $bash_root/init-bolt.sh
else
    . $bash_root/init-qgpu3.sh
fi

. $bash_root/aliases.sh
. $bash_root/environment-variables.sh
. $bash_root/functions.sh
. $bash_root/rl.sh
. $bash_root/interactive.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/rec/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/rec/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/rec/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/rec/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
