#!/bin/bash

files=(
    '.bash_profile'
    '.bashrc'
    '.config/bash/aliases'
    '.config/bash/prompt'
    '.config/sway/config'
    '.emacs.d/init.el'
    '.local/bin/git-check-repos'
    '.gitconfig'
    '.vimrc'
)

for file in "${files[@]}"; do
    mkdir -p "$HOME/$(dirname "$file")"
    ln -s -i "$(realpath $(dirname "$0"))/$file" "$HOME/$file"
done
