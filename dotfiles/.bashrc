SECRETS=~/.secrets.sh

if [ -f $SECRETS ]; then
    . $SECRETS
fi

IFS=. read -ra host <<< $(hostname)

if [[ $host == "qgpu3" ]]; then
    export CODE_ROOT=~/git
else
    export CODE_ROOT=/code
fi

bash_root=$CODE_ROOT/dotfiles/bash

. $bash_root/init-$host.sh

. $bash_root/aliases.sh
. $bash_root/environment-variables.sh
. $bash_root/functions.sh
. $bash_root/interactive.sh
