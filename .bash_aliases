alias ..='cd ..'
alias ...='cd ../..'
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias la='ls -lA'
alias ll='ls -l'
alias ls='LC_COLLATE=C ls --color=auto --group-directories-first'
alias mkdir='mkdir -p -v'
alias pacman='pacman --color=auto'
alias rm='rm -I'
alias sudo='sudo '
alias watch='watch '

if [ -d ~/Development ]; then
    alias d='cd ~/Development/'
fi
