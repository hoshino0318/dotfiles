#!/bin/sh

basedir=`dirname $0`/../..

FILE_LIST=(.emacs.d .vimrc .vim .zshrc)

for file in ${FILE_LIST[@]}; do
  if [ -f ~/$file -o -d ~/$file ]; then
    rm -rf ~/$file
  fi
  ln -s $HOME/dotfiles/$file ~/
done

