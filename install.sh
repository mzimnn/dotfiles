#!/bin/bash

files=(
    '.bash_profile'
    '.bashrc'
    '.config/bash/aliases'
    '.config/bash/prompt'
    '.config/emacs/init.el'
    '.config/emacs/lisp/init-eww.el'
    '.config/emacs/lisp/init-magit.el'
    '.config/emacs/lisp/init-org.el'
    '.config/emacs/lisp/init-theme.el'
    '.config/git/config'
    '.config/sway/config'
    '.config/tmux/tmux.conf'
    '.inputrc'
    '.local/bin/git-check-repos'
    '.vimrc'
)

for file in "${files[@]}"; do
    mkdir -p "$HOME/$(dirname "$file")"
    ln -s -i "$(realpath "$(dirname "$0")")/$file" "$HOME/$file"
done
