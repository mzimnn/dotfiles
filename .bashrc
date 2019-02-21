# If not running interactively, don't do anything
[[ $- != *i* ]] && return

files=(
    /usr/share/git/completion/git-prompt.sh
    ~/.bash_aliases
    ~/.bash_prompt
)

for file in "${files[@]}"
do
    [ -f "$file" ] && source "$file"
done

unset file
unset files

# shell options
set -o vi
shopt -s autocd
shopt -s cdspell
shopt -s globstar
shopt -s histappend

# environment variables
export EDITOR='vim'
HISTCONTROL=ignoredups
