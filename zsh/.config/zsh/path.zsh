#!/usr/bin/env bash

update_path() {
  if [[ "$OSTYPE" = darwin* ]]; then
    # add android path
    export SDK_DIR=~/Library/Android/sdk
  fi

  if [[ "$OSTYPE" = linux* ]]; then
    export SDK_DIR=~/Android/Sdk
  fi

  export JAVA_HOME=/Applications/Android\ Studio.app/Contents/jre/Contents/Home

  export PATH=$PATH:$SDK_DIR/platform-tools
  export PATH=$PATH:$SDK_DIR/tools

  # add doom path
  export PATH=$PATH:$HOME/projects/others/doom/bin

  [ -d "$HOME/bin" ] && export PATH="$HOME/bin:$PATH"
}

if [[ "$XANDEER_PROFILE" != loaded ]] ; then
  export XANDEER_PROFILE=loaded
  update_path
fi
