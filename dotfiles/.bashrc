# echo 'BEGIN: .bashrc'

export CODE_ROOT=/code

. $CODE_ROOT/dotfiles/bash/environment-variables.sh
. $CODE_ROOT/dotfiles/bash/aliases.sh
. $CODE_ROOT/dotfiles/bash/functions.sh
. $CODE_ROOT/dotfiles/bash/interactive.sh
. $CODE_ROOT/pppp/pppp.sh

# . /usr/local/opt/autoenv/activate.sh

# Prevent the activate script from killing $PATH
# TODO: is this necessary now?
_OLD_VIRTUAL_PATH=

eval "$(direnv hook bash)"
# echo 'END: .bashrc'
