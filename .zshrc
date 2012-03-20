# Created by newuser for 4.3.1

local BLACK=$'%{e[1;30m%}'
local RED=$'%{e[1;31m%}'
local GREEN=$'%{e[1;32m%}'
local YELLOW=$'%{e[1;33m%}'
local BLUE=$'%{e[1;34m%}'
local PURPLE=$'%{e[1;35m%}'
local AQUA=$'%{e[1;36m%}'
local WHITE=$'%{e[1;37m%}'
local DEFAULT=$'%{e[1;m%}'

## Default shell configuration
#
# set prompt
#
autoload colors
colors
case ${UID} in
0)
    PROMPT="%B%{${fg[cyan]}%}%/#%{${reset_color}%}%b "
    PROMPT2="%B%{${fg[cyan]}%}%_#%{${reset_color}%}%b "
    SPROMPT="%B%{${fg[cyan]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
    ;;
*)
    # PROMPT="%{${fg[cyan]}%}%/%%%{${reset_color}%} "
    PROMPT="%{${fg[cyan]}%}[${USER}@${HOST%%.*} %1~]%(!.#.$) %{${reset_color}%}%b"
    PROMPT2="%{${fg[cyan]}%}%_%%%{${reset_color}%} "
    SPROMPT="%{${fg[cyan]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
    #[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
    #    PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
    #;;
esac

# alias
alias rm="rm -i"
alias mv="mv -i"
alias ls="ls -FG"
alias ll="ls -laFG"
[ -f ~/dotfiles/.mac ] && source ~/dotfiles/.mac
alias java="/usr/java/default/bin/java"
alias javac="/usr/java/default/bin/javac"

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

# ディレクトリを水色にする｡
export LS_COLORS='di=01;34'

# ファイルリスト補完でもlsと同様に色をつける｡
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

# cd をしたときにll を実行する
function chpwd() { ls }

# Pager
#export PAGER=lv

# Proxy の設定があれば読み込む
[ -f ~/.proxy ] && source ~/.proxy

# This loads RVM into a shell session.
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# rvm setting
#rvm gemset use ruby1.9.3-rails3.1.3

# less に色付け
export LESS='-R'
export LESSOPEN='| /usr/bin/src-hilite-lesspipe.sh %s'
