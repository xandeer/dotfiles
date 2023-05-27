def nuopen [arg, --raw (-r)] { if $raw { open -r $arg } else { open $arg } }
alias open = ^open

alias q = exit

alias l = ls
alias ll = ls -l
alias la = ls -la
alias ldot = ls .*

alias md = mkdir

alias gc = git checkout
alias gl = git log

alias m = make

alias y = yarn
alias ya = yarn add

alias n = npm
alias ni = npm -i
