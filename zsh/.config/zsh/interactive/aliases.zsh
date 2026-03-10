[[ -n ${ZSH_INTERACTIVE_ALIASES_LOADED:-} ]] && return
typeset -g ZSH_INTERACTIVE_ALIASES_LOADED=1

if [[ $OSTYPE == linux* ]] && zsh_has_command nixos-rebuild; then
  alias ors='sudo nixos-rebuild switch'
elif [[ $OSTYPE == darwin* ]] && zsh_has_command darwin-rebuild; then
  alias ors='darwin-rebuild switch'
  alias p11='export https_proxy=http://localhost:8010'
fi

(( $+aliases[cp] )) && unalias cp
(( $+aliases[mv] )) && unalias mv
(( $+aliases[rm] )) && unalias rm
(( $+aliases[duf] )) && unalias duf

alias ss='skillshare'
alias q='exit'

alias hf='hledger -f ~/projects/personal/notes/hledger-financial.org'
alias hh='hledger -f ~/projects/personal/notes/hledger-habit.org'

if zsh_has_command rg; then
  alias grep='rg'
  alias ag='alias | rg'
  alias -g G='| rg'
fi

if zsh_has_command terminal-notifier; then
  alias tn='terminal-notifier -message'
fi

if zsh_has_command fd; then
  alias find='fd'
fi

alias m='make'
alias n='npm'
alias y='yarn'

if zsh_has_command scd; then
  alias s='scd'
fi

if zsh_has_command trash; then
  alias rm='trash'
  alias rmrf='trash'
  alias rmd='trash ~/temp/donut/*.apk;trash ~/temp/donut/*.zip'
  alias rms='trash ~/temp/screenshot/*.png'
fi

if zsh_has_command bat; then
  alias cat='bat'
fi

if zsh_has_command prettyping; then
  alias ping='prettyping --nolegend'
fi

if zsh_has_command ncdu; then
  alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"
fi

if zsh_has_command fzf && zsh_has_command bat; then
  alias preview="fzf --preview 'bat --color \"always\" {}'"
fi

if zsh_has_command fzf; then
  alias -g F='| fzf'
fi

alias freq='zsh_stats'

alias mr='mr -d ~'
alias mu='mr -d ~ update'
alias mc='mr -d ~ commit'

alias gam='git add --all;git commit -m'
alias gcd='git checkout dev'
alias gm='git commit -m'
alias gr-='git reset HEAD~'

alias yag='yarn global add'
alias ya='yarn add'
alias yad='yarn add -D'
alias yr='yarn run'
alias ys='yarn run start'
alias yb='yarn run build'
alias yu='yarn upgrade'

alias ni='npm i'
alias nig='npm i -g '
alias nis='npm i -S '
alias nid='npm i -D '
alias ns='npm start'
alias nt='npm test'
alias nto='npm test --'
alias nr='npm run'

alias mx='chmod +x'

alias tma='tmux attach'
alias tmn='tmux new -s $(basename $(pwd))'
alias tmr='tmux new -s ranger'
alias tmu='tmux list-sessions'

if zsh_has_command brew-file; then
  alias bf='brew-file --no_appstore'
  alias bi='brew-file --no_appstore install'
  alias bic='brew-file --no_appstore --cask install'
fi

alias bu='brew uninstall'
alias buc='brew uninstall --cask'
