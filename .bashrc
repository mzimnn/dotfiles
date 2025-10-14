# If not running interactively, don't do anything
[[ $- != *i* ]] && return

is_installed () {
    command -v "$1" >/dev/null
}

source_if_exists () {
    [ -f "$1" ] && source "$1"
}

if is_installed tmux &&
   # Debugging in VSCode is not possible if tmux is started
   [ "$TERM_PROGRAM" != 'vscode' ] &&
   # Do not use tmux inside Emacs or Intellij IDEA
   [ -z "$INSIDE_EMACS" ] &&
   [ "$TERMINAL_EMULATOR" != 'JetBrains-JediTerm' ]
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
    source_if_exists "$file"
done

unset file
unset files

# Dump terminals (e.g. Emacs shell-mode) do not support these key bindings
if [ "$TERM" != "dumb" ]
then
    FZF_ALT_C_COMMAND= source_if_exists /usr/share/fzf/key-bindings.bash
fi

# Shell options
shopt -s autocd
shopt -s cdspell
shopt -s globstar
shopt -s histappend

# Environment variables
export EDITOR='vim'
export MANPAGER='less -R --color=d+g --color=u+b'
GIT_COMPLETION_IGNORE_CASE=1
HISTCONTROL=ignoreboth
HISTIGNORE=exit
HISTSIZE=10000
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

if is_installed diff-so-fancy
then
    export GIT_PAGER='diff-so-fancy | less --tabs=5 -FRX'
    export GIT_CONFIG_COUNT=3
    export GIT_CONFIG_KEY_0='interactive.diffFilter'
    export GIT_CONFIG_VALUE_0='diff-so-fancy --patch'
    export GIT_CONFIG_KEY_1='color.diff-highlight.oldHighlight'
    export GIT_CONFIG_KEY_2='color.diff-highlight.newHighlight'

    # Check if terminal supports less than 256 colors or a light background is
    # used
    if [ "$(tput colors)" -lt 256 ] || [ "$BACKGROUND" = 'light' ]
    then
        export GIT_CONFIG_VALUE_1='white bold red'
        export GIT_CONFIG_VALUE_2='white bold green'
    else
        export GIT_CONFIG_VALUE_1='red bold 52'
        export GIT_CONFIG_VALUE_2='green bold 22'
    fi
fi

# Set LS_COLORS using `dircolors`
is_installed dircolors && eval $(dircolors -b)

unset is_installed
