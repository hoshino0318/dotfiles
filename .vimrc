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

" encoding
set encoding=utf-8
set fileencodings=utf-8,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,euc-jp,sjis,cp932

" インデントの設定
filetype plugin indent on
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

set laststatus=2
set statusline=%F%r%h%=

" 自動インデント
set autoindent

" スマートインデント
set smarttab

" 検索文字列が小文字の場合は大文字小文字を区別なく検索する
set ignorecase

" 検索文字列に大文字が含まれている場合は区別して検索する
set smartcase

" コマンドの自動補完
set wildmenu

" 行末の空白を自動で消す
autocmd BufWritePre * :%s/\s\+$//e

" カーソル行をハイライト
set cursorline
" カレントウィンドウにのみ罫線を引く
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
NeoBundleLazy 'dag/vim2hs'
NeoBundleLazy 'derekwyatt/vim-scala'
NeoBundleLazy 'vim-coffee-script'
NeoBundleLazy 'ZenCoding.vim'

filetype plugin indent on
" ### END NeoBundle ###

" molokai
set t_Co=256
colorscheme molokai

" omnifunc
setlocal omnifunc=syntaxcomplete#Complete

" neocomplcache
let g:neocomplcache_enable_at_startup = 1 " 起動時に有効化
