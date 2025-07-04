#!/usr/bin/env sh

function set_proxy() {
  local is_termux=$(command -v termux-setup-storage)
  if test -z "$is_termux"; then
    export px=http://127.0.0.1:8010
    export https_proxy=$px
    export http_proxy=$px
    export all_proxy=$px
  fi
}

# set_proxy
