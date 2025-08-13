export CODE_ROOT=~/code
export BASH_ROOT=$CODE_ROOT/dotfiles/bash
export SECRETS=~/.secrets.sh

if [ -f $SECRETS ]; then
    . $SECRETS
fi

IFS=. read -ra hosts <<< $(hostname)
set -- $hosts
host=$1

if [[ "$host" == "bolt" ]]; then
    . $BASH_ROOT/init-bolt.sh
else
    . $BASH_ROOT/init-qgpu3.sh
fi

. $BASH_ROOT/aliases.sh
. $BASH_ROOT/environment-variables.sh
. $BASH_ROOT/functions.sh
. $BASH_ROOT/interactive.sh
. $BASH_ROOT/prompts.sh
. $BASH_ROOT/quiet.sh
