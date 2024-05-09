#!/bin/bash

root=${1:-/code}
dotfiles=$root/dotfiles/dotfiles

for file in .bash_completion .bash_profile .bashrc .emacs .emacs.d .gitconfig; do
    source=~"$dotfiles/$file"
    target=~/"$file"

    if [ -f "$target" ]; then
        echo mv "$target" "$target".bak
    fi
    echo ln -s "$source" "$target"
done
