#!/bin/sh

basedir=`dirname $0`/../..

# set files
FILE_LIST=(.emacs.d .vimrc .vim .zshrc)

for file in ${FILE_LIST[@]}; do
  if [ -f ~/$file -o -d ~/$file ]; then
    rm -rf ~/$file
  fi
  ln -s $HOME/dotfiles/$file ~/
done

# initialize git submodle
cd $HOME/dotfiles
git submodule update --init

# vim NeoBundleInstall
vim -S .vim/neobundleinstall.vim
