if [ $(uname -s) = 'Linux' ]; then
    alias ors='sudo nixos-rebuild switch'
elif [ $(uname -s) = 'Darwin' ]; then
    alias ors='darwin-rebuild switch'
    alias p11='export https_proxy=http://localhost:8010'
fi

unalias cp
unalias mv
unalias rm
unalias duf

alias hf='hledger -f ~/projects/personal/notes/hledger-financial.org'
alias hh='hledger -f ~/projects/personal/notes/hledger-habit.org'

alias grep='rg'
alias ag='alias | rg'
alias tn='terminal-notifier -message'

alias l='exa -lFh'
alias la='exa -lFha'
alias ldot='exa -ld .*'
alias ls='exa'

alias find='fd'

alias m='make'
alias n='npm'
alias y='yarn'
alias s='scd'
alias rm='trash'
alias rmrf='trash'
alias rmd='trash ~/temp/donut/{*.apk,*.zip,*.txt}'
alias rms='trash ~/temp/screenshot/*.png'

alias cat='bat'
alias ping='prettyping --nolegend'
alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"
alias preview="fzf --preview 'bat --color \"always\" {}'"
alias -g F='| fzf'
alias -g G='| rg'
alias freq='zsh_stats'

# mr
alias mr='mr -d ~'
alias mu='mr -d ~ update'
alias mc='mr -d ~ commit'

# git
alias gam='git add --all;git commit -m'
alias gcd='git checkout dev'
alias gm='git commit -m'
alias gr-='git reset HEAD~'

# yarn
alias yag="yarn global add"
alias ya="yarn add"
alias yad="yarn add -D"
alias yr="yarn run"
alias ys="yarn run start"
alias yb="yarn run build"
alias yu="yarn upgrade"

# npm
alias ni="npm i"
alias nig="npm i -g "
alias nis="npm i -S "
alias nid="npm i -D "
alias ns="npm start"
alias nt="npm test"
alias nto="npm test --"
alias nr="npm run"

alias mx='chmod +x'

# tmux
alias tma='tmux attach'
alias tmn='tmux new -s $(basename $(pwd))'
alias tmr='tmux new -s ranger'
alias tmu='tmux list-sessions'
