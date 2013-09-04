" .vimrc

" show line number
set nocompatible
set number

" syntax highlight
if &t_Co > 1
  syntax enable
endif

" backspace
set backspace=2

" encoding
set encoding=utf-8
set fileencodings=utf-8,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,euc-jp,sjis,cp932

" indent
filetype plugin indent on
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent
set smarttab

set laststatus=2
set statusline=%F%r%h%=

" search
set ignorecase
set smartcase

" command complement
set wildmenu

" command line complement
set wildmode=list,full

" delete trailing white spaces
autocmd BufWritePre * :%s/\s\+$//e

" cursor highlight
set cursorline
augroup cch
  autocmd! cch
  autocmd WinLeave * set nocursorline
  autocmd WinEnter,BufRead * set cursorline
augroup END
hi clear CursorLine
hi CursorLine gui=underline
highlight CursorLine ctermbg=black guibg=black

" undodir
if has('persistent_undo')
  set undodir=~/.vimundo
  set undofile
endif

" ruby
autocmd BufNewFile,BufRead *.ru set filetype=ruby

" ##### NeoBundle #####
set nocompatible
filetype off

let g:neobundle_default_git_protocol='https'

set rtp+=~/dotfiles/neobundle.vim
if has('vim_starting')
  set runtimepath+=~/dotfiles/neobundle.vim
  call neobundle#rc(expand('~/.vim/neobundle/'))
endif

NeoBundle 'molokai'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Lokaltog/vim-powerline'
NeoBundle 'groenewege/vim-less'
NeoBundle 'tomtom/tcomment_vim'
NeoBundleLazy 'dag/vim2hs'
NeoBundleLazy 'derekwyatt/vim-scala'
NeoBundleLazy 'elzr/vim-json'
NeoBundleLazy 'mattn/emmet-vim'
NeoBundleLazy 'vim-coffee-script'

filetype plugin indent on
" ### END NeoBundle ###

" molokai
set t_Co=256
colorscheme molokai

" omnifunc
setlocal omnifunc=syntaxcomplete#Complete

" neocomplcache
let g:neocomplcache_enable_at_startup = 1

" vim-json
au! BufRead,BufNewFile *.json set filetype=json

" Unite key bindings
noremap <C-i> :Unite buffer<CR>
noremap <C-m> :Unite file_mru<CR>
noremap <C-k> :Unite -buffer-name=file file<CR>
