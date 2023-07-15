def nuopen [arg, --raw (-r)] { if $raw { open -r $arg } else { open $arg } }
alias open = ^open

alias ors = darwin-rebuild switch --flake ~/.config/nix-darwin

alias e = emacsclient

alias q = exit

# dir
alias md = mkdir
# zoxide
alias s = z

# ls
alias l = ls
alias ll = ls -l
alias la = ls -la
alias ldot = ls .*

# git
alias gc = git checkout
alias gl = git log
alias gst = git status

# make
alias m = make

# yarn
alias y = yarn
alias ya = yarn add

# npm
alias n = npm
alias ni = npm -i

# brew
alias bf = brew-file --no_appstore
alias bi = brew-file --no_appstore install
alias bic = brew-file --no_appstore --cask install
alias bu = brew uninstall
alias buc = brew uninstall --cask
