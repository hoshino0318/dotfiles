# .zshrc
# Author:  Tatsuya Hoshino
# Update: 2013/09/21

local BLACK=$'%{e[1;30m%}'
local RED=$'%{e[1;31m%}'
local GREEN=$'%{e[1;32m%}'
local YELLOW=$'%{e[1;33m%}'
local BLUE=$'%{e[1;34m%}'
local PURPLE=$'%{e[1;35m%}'
local AQUA=$'%{e[1;36m%}'
local WHITE=$'%{e[1;37m%}'
local DEFAULT=$'%{e[1;m%}'

# set prompt
#
autoload colors
colors

# git branch setting
# see: http://blog.hifumi.info/mac/custom-zsh-prompt/
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '%b'
zstyle ':vcs_info:*' actionformats '%b|%a'
# In the case of CYGWIN,
# precmd don't works because it's too slow.
KERNEL_NAME=`uname -s`
case $KERNEL_NAME in
  CYGWIN*)
    ;;
  *)
    precmd () {
        psvar=()
        LANG=en_US.UTF-8 vcs_info
        [ -n "$vcs_info_msg_0_" ] && psvar[1]="$vcs_info_msg_0_"
    }
    ;;
esac

# colour prompt
# see: http://yonchu.hatenablog.com/entry/2012/10/20/044603
case ${UID} in
0)
    PROMPT="%B%{${fg[cyan]}%}%/#%{${reset_color}%}%b "
    PROMPT2="%B%{${fg[cyan]}%}%_#%{${reset_color}%}%b "
    SPROMPT="%B%{${fg[cyan]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
    ;;
*)
    PROMPT="%F{033}[${USER}@${HOST%%.*} %1~%F{208}%1(v|(%1v)|)%F{033}]%(!.#.$) %{${reset_color}%}"
    PROMPT2="%F{033}%_%%%{${reset_color}%} "
    SPROMPT="%F{162}%r is correct? [n,y,a,e]:%{${reset_color}%} "
    #[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
    #    PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
    #;;
esac

# alias
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"
alias tree="tree -C"
alias ec="emacsclient"
#alias java="/usr/java/default/bin/java"
#alias javac="/usr/java/default/bin/javac"

export PATH=$HOME/bin:$PATH

# OS X に関する設定
if test -f ~/.mac_alias; then
    source ~/.mac_alias
else
    alias ls="ls -F --color"
    alias ll="ls -alhF --color"
fi

# 補完の利用設定
autoload -U compinit
compinit

# C-s, C-qを無効にする。
setopt NO_flow_control
setopt COMPLETE_IN_WORD

# コマンドの訂正
setopt correct

# コマンドラインの引数で –prefix=/usr などの = 以降でも補完できる
setopt magic_equal_subst

# LSCOLORS (BSD 用) の色を変更
export LSCOLORS=Exfxcxdxbxegedabagacad
# LSOLORS の色変更
export LS_COLORS='di=01;34'

# ファイルリスト補完でも ls と同様に色をつける
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# sudo でも補完の対象
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

# 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt print_eight_bit

## Command history configuration
HISTFILE=${HOME}/.zsh_history
HISTSIZE=50000
SAVEHIST=50000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data
setopt append_history

# カッコの対応などを自動的に補完
setopt auto_param_keys

# 補完候補が複数ある時に、一覧表示
setopt auto_list

# 補完キー（Tab, Ctrl+I) を連打するだけで順に補完候補を自動で補完
setopt auto_menu

# cd をしたときにls を実行する
function chpwd() { ls }

# Pager
export PAGER="lv -c"

# beep 音を消す
setopt nobeep
setopt nolistbeep

## cd 時に自動で push
setopt auto_pushd
## 同じディレクトリを pushd しない
setopt pushd_ignore_dups

# インクリメンタルサーチに色を付ける
zle_highlight=(default:fg=white isearch:bold,fg=green)

# Colored man pages
export MANPAGER='less -R'
function man() {
    # mb begin blinking
    # md begin bold
    # me end mode
    # se end standout-mode
    # so begin standout-mode - info box
    # ue end underline
    # us begin underline
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[01;38;5;33m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}

# Colored grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='01;38;5;74'

# This loads RVM into a shell session.
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# less に色付け
export LESS='-R'
if [ -f /usr/bin/src-hilite-lesspipe.sh ]; then
  export LESSOPEN='| /usr/bin/src-hilite-lesspipe.sh %s'
fi

# 個別の path 設定があれば読み込む
[ -f ~/.path ] && source ~/.path

# Proxy の設定があれば読み込む
[ -f ~/.proxy ] && source ~/.proxy

# zsh completion
autoload -U compinit && compinit

# zaw
source ~/dotfiles/zaw/zaw.zsh

# peco function
# see: http://weblog.bulknews.net/post/89635306479/ghq-peco-percol
function peco-src () {
    local selected_dir=$(ghq list --full-path | peco --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-src
bindkey '^]' peco-src
