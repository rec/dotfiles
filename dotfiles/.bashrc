SECRETS=~/.secrets.sh

if [ -f $SECRETS ]; then
    . $SECRETS
fi

export CODE_ROOT=/code

. $CODE_ROOT/dotfiles/bash/aliases.sh
. $CODE_ROOT/dotfiles/bash/environment-variables.sh
. $CODE_ROOT/dotfiles/bash/functions.sh
. $CODE_ROOT/dotfiles/bash/interactive.sh
# . $CODE_ROOT/pppp/pppp.sh

eval "$(direnv hook bash)"
