#!/bin/bash

source=${1:-/code}
target=${2:-~}

set -euo pipefail

# Omit .bash_profile
for file in .bash_completion .bashrc .emacs .emacs.d .gitconfig; do
    s=~"$source/dotfiles/dotfiles/$file"
    t="$target/$file"

    if [ -f "$t" ]; then
        echo mv "$t" "$t".bak
    fi
    echo ln -s "$s" "$t"
done
