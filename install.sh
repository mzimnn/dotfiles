#!/bin/bash

files=(
    '.bash_profile'
    '.bashrc'
    '.config/bash/aliases'
    '.config/bash/prompt'
    '.config/emacs/init.el'
    '.config/sway/config'
    '.inputrc'
    '.local/bin/git-check-repos'
    '.gitconfig'
    '.vimrc'
)

for file in "${files[@]}"; do
    mkdir -p "$HOME/$(dirname "$file")"
    ln -s -i "$(realpath $(dirname "$0"))/$file" "$HOME/$file"
done
