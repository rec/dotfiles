SECRETS=~/.secrets.sh

if [ -f $SECRETS ]; then
    source $SECRETS
fi

IFS=. read -ra hosts <<< $(hostname)
host=$hosts

if [[ $host == "qgpu3" ]]; then
    export CODE_ROOT=~/code
else
    export CODE_ROOT=/code
    host=bolt
fi

bash_root=$CODE_ROOT/dotfiles/bash
bash_file=$bash_root/init-${host}.sh
bash_file=${bash_file}

. $bash_file

. $bash_root/aliases.sh
. $bash_root/environment-variables.sh
. $bash_root/functions.sh
. $bash_root/interactive.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/conda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/conda/etc/profile.d/conda.sh" ]; then
        . "/opt/conda/etc/profile.d/conda.sh"
    else
        export PATH="/opt/conda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

