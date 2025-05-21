#!/bin/bash

mkdir -p ~/code
cd ~/code
git clone git@github.com:rec/dotfiles.git
dotfiles/bash/link_dotfiles_to_home.sh
