" .vimrc

" show line number
set nocompatible
set number

" syntax highlight
if &t_Co > 1
  syntax enable
endif

" for backspace
set backspace=2

setlocal expandtab
setlocal tabstop<
setlocal softtabstop=2
setlocal shiftwidth=2

set laststatus=2
set statusline=%F%r%h%=
