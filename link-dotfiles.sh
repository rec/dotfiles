DOTFILES=~/code/dotfiles/dotfiles/

rm -f ~/.bash_completion ~/.bash_profile ~/.bashrc ~/.emacs ~/.emacs.d ~/.gitconfig ~/.gitignore

ln -s $DOTFILES.bash_completion ~/.bash_completion
ln -s $DOTFILES.bash_profile ~/.bash_profile
ln -s $DOTFILES.bashrc ~/.bashrc
ln -s $DOTFILES.emacs ~/.emacs
ln -s $DOTFILES.emacs.d ~/.emacs.d
ln -s $DOTFILES.gitconfig ~/.gitconfig
ln -s $DOTFILES.gitignore ~/.gitignore
