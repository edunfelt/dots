#
#  __                 __              
# |  |--.---.-.-----.|  |--.----.----.
# |  _  |  _  |__ --||     |   _|  __|
# |_____|___._|_____||__|__|__| |____|
#
# Emilia's bashrc <3
# Author: Emilia Dunfelt, edun@dunfelt.se
#

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

# path
export PATH=$PATH:$HOME/.local/bin:$HOME/usr/bin:$PLAN9/bin:$HOME/usr/bin
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

export jdfun
alias cjd='jdfun'

# print log
alias l="cat ~/LOG"

# screencast boox
alias cast="scrcpy --tcpip=192.168.1.57:5555"

alias claws-mail="claws-mail --alternate-config-dir $HOME/.config/claws-mail"
alias pidgin="pidgin -c $HOME/.config/purple"

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
    cd ~/Documents/*/*/${1}*
}

########################################################
# Misc
########################################################
# Nice ls folder colors
#LS_COLORS=$LS_COLORS:'tw=01;33:ow=01;33:'; export LS_COLORS

# FFF
# Directory color [0-9]
export FFF_COL1=1
# Status background color [0-9]
export FFF_COL2=4
# Selection color [0-9] (copied/moved files)
export FFF_COL3=4
# Cursor color [0-9]
export FFF_COL4=6
# Status foreground color [0-9]
export FFF_COL5=7
# Favorites (Bookmarks) (keys 1-9) (dir or file)
export FFF_FAV1=~/current
export FFF_FAV2=~/archive
export FFF_FAV3=~/tmp
export FFF_FAV5=~/media
export FFF_FAV6=~/.config/
export FFF_FAV7=~/media/tablet

export PATH=$PATH:/home/e/.spicetify
