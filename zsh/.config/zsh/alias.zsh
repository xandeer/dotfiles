if [ $(uname -s) = 'Linux' ]; then
    alias ors='sudo nixos-rebuild switch'
elif [ $(uname -s) = 'Darwin' ]; then
    alias ors='darwin-rebuild switch'
fi

alias ag='alias | grep'
alias tn='terminal-notifier -message'

alias m='make'
alias n='npm'
alias y='yarn'
alias s='sudo'
alias rmrf='rm -rf'

alias cat='bat'
alias ping='prettyping --nolegend'
alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"
alias preview="fzf --preview 'bat --color \"always\" {}'"
alias -g F='| fzf'
alias -g G='| rg'

# mr
alias mu='cd ~;mr -j 9 update;cd -'
alias mn='commit_and_push notes'
alias mp='commit_and_push xandeer.github.io'

function commit_and_push() {
    cd ~/projects/personal/$1 \
        && git add --all \
        && git commit -m "$(date +'manual: [%F %a %T]')" \
        && git push
    cd -
}

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