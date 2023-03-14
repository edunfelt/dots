#
#  __                 __              
# |  |--.---.-.-----.|  |--.----.----.
# |  _  |  _  |__ --||     |   _|  __|
# |_____|___._|_____||__|__|__| |____|
#


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

########################################################
# Variables
########################################################
# vim
export VISUAL=vim;
export EDITOR=vim;
# export EDITOR=emacsclient;

# general
export BROWSER="firefox"

# ps1
export PS1="\[\e[36m\]🦋 \[\e[m\]\W\[\e[36m\] >>\[\e[m\] "
#export PS1="\W $ "

# integrate gtk theme with qt apps
export QT_QPA_PLATFORMTHEME="qt5ct"
#export QT_STYLE_OVERRIDE="qt5ct"

# other
export PLAN9="/usr/local/plan9"
export XDG_DATA_HOME="~/.config"
export WEECHAT_HOME="~/.config/weechat"
export GNUPGHOME="~/.config/gnupg"
export WINEARCH=win32
export PF_COL2=0
export BAT_THEME="base16-256"
export YTFZF_EXTMENU="rofi -dmenu -fuzzy -width 1000"
export YTFZF_ENABLE_FZF_DEFAULt_OPTS=0
export FZF_BIBTEX_CACHEDIR='~/Downloads/fzf-bibtex'
export FZF_BIBTEX_SOURCES='~/google-drive/current/bibliography.bib'

# path
export PATH=$PATH:$HOME/.local/bin:$HOME/go/bin:$PLAN9/bin:$HOME/.cargo/bin


########################################################
# Aliases
########################################################
# common commands
alias ls='lsd'
alias ll='ls -l'              # easy long listing
alias lt='ls --tree'            # tree listing
alias grep='grep --color=auto'  # nice grep coloring
alias diff='colordiff'          # nice diff coloring
alias cp="cp -i"                # confirm before overwriting something
alias df='df -h'                # human-readable sizes
alias free='free -m'            # show sizes in MB
alias v="vim"                   # vim, of course :)
alias e="emacsclient"
alias h='history'               # I'm lazy
alias j='jobs -l'               # faster, better jobs
alias ..='cd ..'                # up a dir
alias ...='cd ../../..'         # up two dirs
alias b='buku --suggest'        # search bookmarks
alias bfzf='firefox $(buku -p -f 40 | fzf | cut -f1)'
alias r='ranger'                # file manager
alias twr='task ready'           # what's next?
alias entropy='cat /proc/sys/kernel/random/entropy_avail'

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

# enable bash completion in interactive shells
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Base16 Shell
# BASE16_SHELL_PATH="$HOME/.config/base16-shell"
# [ -n "$PS1" ] && \
#   [ -s "$BASE16_SHELL_PATH/profile_helper.sh" ] && \
#     source "$BASE16_SHELL_PATH/profile_helper.sh"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

source /home/emilia/.config/broot/launcher/bash/br
