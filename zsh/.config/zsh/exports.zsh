#!/usr/bin/env sh

export DOTS_DIR=$HOME/projects/personal/dotfiles

# Make vim the default editor
export EDITOR="emacsclient"
export GIT_EDITOR=vim

# fuzzy
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(code {})+abort'"

source ~/.config/auth.env
