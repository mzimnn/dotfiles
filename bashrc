# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# environment variables
export EDITOR='vim'

# aliases
alias sudo='sudo '
alias ls='LC_COLLATE=C ls --color=auto --group-directories-first'
alias ll='ls -l'
alias diff='diff --color=auto'
alias mkdir='mkdir -p -v'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias pacman='pacman --color=auto'
alias cower='cower --color=auto'
alias rm='rm -I'

# set bash prompt (PS1)
DEFAULT='\[\033[0m\]'
RED='\[\033[1;31m\]'
GREEN='\[\033[1;32m\]'
YELLOW='\[\033[1;33m\]'
CYAN='\[\033[1;36m\]'

function get_user_color() {
	case `whoami` in
		root)	echo "$RED" ;;
		*)	echo "$CYAN" ;;
	esac
}

source /usr/share/git/completion/git-prompt.sh
branch='$(__git_ps1 " (%s)")'

# override default
PS1="$(get_user_color)\u:$GREEN\w$YELLOW$branch$(get_user_color) \\$ $DEFAULT"
