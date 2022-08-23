#
#  __                 __              
# |  |--.---.-.-----.|  |--.----.----.
# |  _  |  _  |__ --||     |   _|  __|
# |_____|___._|_____||__|__|__| |____|
#
# Emilia's bashrc <3
# Author: Emilia Dunfelt, edun@dunfelt.se


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

########################################################
# Variables
########################################################
# vim
export MYVIMRC="~/.config/vim/vimrc"
export VIMINIT="source $MYVIMRC"
export VISUAL=vim;
export EDITOR=vim;

# general
export BROWSER="firefox"

# ps1
export PS1="\[\e[36m\]👾 \[\e[m\]\W\[\e[36m\] >>\[\e[m\] "
#export PS1="\W $ "

# integrate gtk theme with qt apps
export QT_QPA_PLATFORMTHEME="gtk2"
export QT_STYLE_OVERRIDE="gtk2"

# plan9
export PLAN9="/home/e/plan9"

# other
export XDG_DATA_HOME="/home/e/.config"
export WEECHAT_HOME="~/.config/weechat"
export DOOMDIR="~/.config/doom"
export PF_COL2=0
export GNUPGHOME="~/.config/gnupg"
export WINEPREFIX="~/.cofig/wine"
export GPODDER_HOME="/home/e/media/mus/pods"
export XAPERS_ROOT="/home/e/media/doc/xapers"
export BAT_THEME="base16-256"
export YTFZF_EXTMENU="rofi -dmenu -fuzzy -width 1000"
export YTFZF_ENABLE_FZF_DEFAULt_OPTS=0

# path
export PATH=$PATH:$HOME/.local/bin:$HOME/usr/bin:$PLAN9/bin:$HOME/usr/bin:$HOME/go/bin


########################################################
# Aliases
########################################################
# common commands
alias ll='ls -alF'              # easy long listing
alias ls='ls --color=auto'      # nice ls coloring
alias grep='grep --color=auto'  # nice grep coloring
alias diff='colordiff'          # nice diff coloring
alias cp="cp -i"                # confirm before overwriting something
alias df='df -h'                # human-readable sizes
alias free='free -m'            # show sizes in MB
alias v="vim"                   # vim, of course :)
alias h='history'               # I'm lazy
alias j='jobs -l'               # faster, better jobs
alias ..='cd ..'                # up a dir
alias ...='cd ../../..'         # up two dirs
alias b='buku --suggest'        # search bookmarks
alias bfzf='firefox $(buku -p -f 40 | fzf | cut -f1)'
alias r='ranger'                # file manager
alias twr='task ready'           # what's next?

export jdfun
alias cjd='jdfun'

# screencast boox
alias cast="scrcpy --tcpip=192.168.1.57:5555"


########################################################
# Functions
########################################################
# convenient extraction
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Johnny.Decimal
jdfun() {
    cd ~/archive/*/*/${1}*
}


########################################################
# Misc
########################################################
# Nice ls folder colors
#LS_COLORS=$LS_COLORS:'tw=01;33:ow=01;33:'; export LS_COLORS

# FZF
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"\
" --color=bg+:$color01,bg:$color00,spinner:$color0C,hl:$color0D"\
" --color=fg:$color04,header:$color0D,info:$color0A,pointer:$color0C"\
" --color=marker:$color0C,fg+:$color06,prompt:$color0A,hl+:$color0D"

export PATH=$PATH:/home/e/.spicetify

# enable bash completion in interactive shells
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Base16 Shell
BASE16_SHELL_PATH="$HOME/.config/base16-shell"
[ -n "$PS1" ] && \
  [ -s "$BASE16_SHELL_PATH/profile_helper.sh" ] && \
    source "$BASE16_SHELL_PATH/profile_helper.sh"

. "$HOME/.cargo/env"
