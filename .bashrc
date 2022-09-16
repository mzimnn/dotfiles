# If not running interactively, don't do anything
[[ $- != *i* ]] && return

is_installed () {
    command -v "$1" >/dev/null
}

# Debugging in VSCode is not possible if tmux is started
if is_installed tmux && [ "$TERM_PROGRAM" != 'vscode' ]
then
    # If not inside a tmux session, start a new session
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

# Shell options
shopt -s autocd
shopt -s cdspell
shopt -s globstar
shopt -s histappend

# Environment variables
export EDITOR='vim'
export MANPAGER='less -R --color=d+g --color=u+b'
HISTCONTROL=ignoreboth
HISTSIZE=10000

# Set LS_COLORS using `dircolors`
if is_installed dircolors
then
    eval $(dircolors -b)
fi

unset is_installed
