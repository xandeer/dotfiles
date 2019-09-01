unalias rm
unalias cp
unalias mv

alias docker='sudo docker'
alias vi='vim'
alias vimrc='vim ~/.config/vim/vimrc.local'
alias ag='alias | grep'
alias tn='terminal-notifier -message'

alias m='make'
alias n='npm'
alias s='scd'
alias y='yarn'

alias cat='bat'
alias ping='prettyping --nolegend'
alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"
alias preview="fzf --preview 'bat --color \"always\" {}'"
alias -g F='| fzf'
alias -g G='| rg'

unalias fd
unalias ff