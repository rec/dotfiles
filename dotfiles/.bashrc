export CODE_ROOT=/code

. $CODE_ROOT/dotfiles/bash/environment-variables.sh
. $CODE_ROOT/dotfiles/bash/aliases.sh
. $CODE_ROOT/dotfiles/bash/functions.sh
. $CODE_ROOT/dotfiles/bash/interactive.sh
. $CODE_ROOT/pppp/pppp.sh

# Prevent the activate script from killing $PATH
_OLD_VIRTUAL_PATH=

# denv

# Comment This Out!
export PS1='\u@\h:\w$ '

PATH="/Users/tom/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/tom/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/tom/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/tom/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/tom/perl5"; export PERL_MM_OPT;
