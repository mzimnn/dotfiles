# If not running interactively, don't do anything
[[ $- != *i* ]] && return

is_installed () {
    command -v "$1" >/dev/null
}

if is_installed tmux
then
    # if not inside a tmux session, start a new session
    [ -z "${TMUX}" ] && exec tmux
fi

files=(
    /usr/share/bash-completion/completions/git
    /usr/share/git/completion/git-prompt.sh
    /usr/share/z/z.sh
    ~/.config/bash/aliases
    ~/.config/bash/local
    ~/.config/bash/prompt
)

for file in "${files[@]}"
do
    [ -f "$file" ] && source "$file"
done

unset file
unset files
unset is_installed

# shell options
set -o vi
shopt -s autocd
shopt -s cdspell
shopt -s globstar
shopt -s histappend

# environment variables
export EDITOR='vim'
HISTCONTROL=ignoreboth

# Set LS_COLORS using `dircolors`
eval $(dircolors -b)
