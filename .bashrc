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
   # Do not use tmux inside Emacs
   [ -z "$INSIDE_EMACS" ]
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
    # Make builtin "bind" a no-op so that invocations of "bind" are ignored in
    # the following script which is sourced. I don't like these key bindings,
    # therefore I bind it myself afterwards.
    bind () { :; }

    source_if_exists /usr/share/fzf/key-bindings.bash

    # Use default implementation of builtin "bind" again so it can be used
    # normally.
    unset bind

    if is_installed __fzf_history__
    then
        # Overwrite history search (CTRL-R) with fzf variant
        bind -m emacs-standard -x '"\C-r": __fzf_history__'
        bind -m vi-command -x '"\C-r": __fzf_history__'
        bind -m vi-insert -x '"\C-r": __fzf_history__'
    fi
fi

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
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

if is_installed diff-so-fancy
then
    export GIT_PAGER='diff-so-fancy | less --tabs=5 -FRX'
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0='interactive.diffFilter'
    export GIT_CONFIG_VALUE_0='diff-so-fancy --patch'
fi

# Set LS_COLORS using `dircolors`
is_installed dircolors && eval $(dircolors -b)

unset is_installed
