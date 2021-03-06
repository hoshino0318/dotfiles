#!/bin/sh -x

basedir=`dirname $0`/../..

FILE_LIST=(.eslintrc.json .emacs.d .pryrc .rspec .screenrc .tmux.conf .vimrc .vim .zshrc)

: [Info] Set synblic link
for file in ${FILE_LIST[@]}; do
  if [ -f ~/$file -o -d ~/$file ]; then
    rm -rf ~/$file
  fi
  ln -s $HOME/dotfiles/$file ~/
done
echo;

: [Info] Initialize git submodle
cd $HOME/dotfiles
git submodule update --init
echo;

: [Info] Vim NeoBundleInstall
vim -S .vim/neobundleinstall.vim
echo;

: [Info] Vim Set golang
if [ ! -e $HOME/.vim/neobundle/go ]; then
  if [ -n "$GOROOT" ]; then
    ln -s $GOROOT/misc/vim $HOME/.vim/neobundle/go
  fi
fi
