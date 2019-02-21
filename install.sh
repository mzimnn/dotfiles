#!/bin/bash

files=(
    '.bash_aliases'
    '.bash_profile'
    '.bash_prompt'
    '.bashrc'
    '.gitconfig'
    '.vimrc'
)

for file in "${files[@]}"; do
    ln --symbolic --interactive "$(realpath $(dirname "$0"))/"$file"" "$HOME/"$file""
done
