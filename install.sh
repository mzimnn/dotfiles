#!/bin/bash

files=(
    '.bash_aliases'
    '.bash_profile'
    '.bash_prompt'
    '.bashrc'
    '.emacs.d/init.el'
    '.gitconfig'
    '.vimrc'
)

for file in "${files[@]}"; do
    mkdir -p "$(dirname "$file")"
    ln -s -i "$(realpath $(dirname "$0"))/$file" "$HOME/$file"
done
