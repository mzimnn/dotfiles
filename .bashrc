# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias definitions
if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

# shell options
set -o vi
shopt -s cdspell

# environment variables
export EDITOR='vim'
HISTCONTROL=ignoredups

# set bash prompt (PS1)
DEFAULT='\033[0m'
RED='\033[1;31m'
GREEN='\033[1;32m'
YELLOW='\033[1;33m'
CYAN='\033[1;36m'

get_user_color() {
    case "$(whoami)" in
        root) echo "\[$RED\]" ;;
        *)    echo "\[$GREEN\]" ;;
    esac
}

get_exit_status_color() {
    if [ "$?" -eq 0 ]; then
        printf "$GREEN"
    else
        printf "$RED"
    fi
}

char_to_int() {
    printf '%d' "'$1"
}

word_to_int() {
    local word="$1"
    local sum=0

    for (( i=0; i < "${#word}"; ++i )); do
        sum=$((sum + $(char_to_int "${word:$i:1}")))
    done

    echo "$sum"
}

word_to_color() {
    local MAX_COLOR_CODE=232 # excluded
    local color_value="$(($(word_to_int "$1") % $MAX_COLOR_CODE))";
    echo "\[\033[38;5;${color_value}m\]"
}

if [ -f /usr/share/git/completion/git-prompt.sh ]; then
    source /usr/share/git/completion/git-prompt.sh
    branch='$(__git_ps1 " (%s)")'
fi

PS1="$(get_user_color)\u\[$DEFAULT\]@$(word_to_color $(hostname))\h\[$DEFAULT\]:\[$YELLOW\]\w\[$CYAN\]$branch\[\$(get_exit_status_color)\]\n\\$ \[$DEFAULT\]"
