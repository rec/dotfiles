#!/bin/bash

source=${1:-/code}
target=${2:-~}


# Omit .bash_profile
for file in .bash_completion .bashrc .bash_profile .emacs .emacs.d .gitconfig; do
    s=~$source/dotfiles/dotfiles/$file
    t=$target/$file

    if [ -f $t ]; then
        mv $t $t.bak
    fi
    ln -s $s $t
done
