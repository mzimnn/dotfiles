# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias definitions
if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

# environment variables
export EDITOR='vim'

# set bash prompt (PS1)
DEFAULT='\033[0m'
RED='\033[1;31m'
GREEN='\033[1;32m'
YELLOW='\033[1;33m'
CYAN='\033[1;36m'

function get_user_color() {
    case "$(whoami)" in
        root) echo "\[$RED\]" ;;
        *)    echo "\[$GREEN\]" ;;
    esac
}

function get_exit_status_color() {
    if [ "$?" -eq 0 ]; then
        printf "$GREEN"
    else
        printf "$RED"
    fi
}

if [ -f /usr/share/git/completion/git-prompt.sh ]; then
    source /usr/share/git/completion/git-prompt.sh
    branch='$(__git_ps1 " (%s)")'
fi

PS1="$(get_user_color)\u@\h\[$DEFAULT\]:\[$YELLOW\]\w\[$CYAN\]$branch\$(get_exit_status_color) \$ \[$DEFAULT\]"
