#!/bin/bash

files=(
    '.bash_profile'
    '.bashrc'
    '.config/bash/aliases'
    '.config/bash/prompt'
    '.config/emacs/init.el'
    '.config/emacs/lisp/init-eww.el'
    '.config/emacs/lisp/init-magit.el'
    '.config/emacs/lisp/init-misc.el'
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
    source="$(realpath "$(dirname "$0")")/$file"
    target="$HOME/$file"

    mkdir -p "$(dirname "$target")"

    if [ "$(readlink "$target")" = "$source" ]
    then
        # file is already symlinked
        continue
    fi

    ln -s -i "$source" "$target"
done
