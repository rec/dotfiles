export CODE_ROOT=/code

. $CODE_ROOT/dotfiles/bash/environment-variables.sh
. $CODE_ROOT/dotfiles/bash/aliases.sh
. $CODE_ROOT/dotfiles/bash/functions.sh
. $CODE_ROOT/dotfiles/bash/interactive.sh
. $CODE_ROOT/pppp/pppp.sh

# Prevent the activate script from killing $PATH
_OLD_VIRTUAL_PATH=

denv
