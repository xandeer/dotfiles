#!/usr/bin/env sh

export DOTS_DIR=$HOME/projects/personal/dotfiles

# Make vim the default editor
export EDITOR="emacsclient"
export GIT_EDITOR=vim

# nvm
export NVM_DIR=$HOME/.nvm
[ -s "$NVM_DIR/nvm.sh" ] && \. $NVM_DIR/nvm.sh  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. $NVM_DIR/bash_completion  # This loads nvm bash_completion

# fuzzy
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(code {})+abort'"