#!/usr/bin/env sh

function update-path() {
  # update path for gnu coreutils, make & find on darwin
  export PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
  export MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH
  export PATH=/usr/local/opt/make/libexec/gnubin:$PATH
  export MANPATH=/usr/local/opt/make/libexec/gnuman:$MANPATH
  export PATH=/usr/local/opt/findutils/libexec/gnubin:$PATH
  export MANPATH=/usr/local/opt/findutils/libexec/gnuman:$MANPATH

  if [[ "$OSTYPE" = darwin* ]]; then
    # add android path
    export SDK_DIR=~/Library/Android/sdk

    # add rust path
    export PATH=$HOME/.cargo/bin:$PATH
  fi

  if [[ "$OSTYPE" = linux* ]]; then
    export SDK_DIR=~/Android/Sdk
  fi

  export JAVA_HOME=/Applications/Android\ Studio.app/Contents/jre/Contents/Home

  export PATH=$PATH:$SDK_DIR/platform-tools
  export PATH=$PATH:$SDK_DIR/tools

  export RUST_SRC_PATH=$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src

  # add doom path
  export PATH=$PATH:$HOME/.emacs.d/bin


  # The next line updates PATH for the Google Cloud SDK.
  export GOOGLE_CLOUD_SDK=$HOME/.local/share/google-cloud-sdk
  export GOOGLE_PATH_INC=$GOOGLE_CLOUD_SDK/path.zsh.inc
  export GOOGLE_COMPLETION_INC=$GOOGLE_CLOUD_SDK/completion.zsh.inc
  if [ -f $GOOGLE_PATH_INC ]; then
    source $GOOGLE_PATH_INC
  fi

  # The next line enables shell command completion for gcloud.
  if [ -f $GOOGLE_COMPLETION_INC ]; then
    source $GOOGLE_COMPLETION_INC
  fi
}

if [[ "$XANDEER_PROFILE" != loaded ]] ; then
  export XANDEER_PROFILE=loaded
  update-path
fi
