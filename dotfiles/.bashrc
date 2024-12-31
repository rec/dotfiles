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
