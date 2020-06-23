#!/usr/bin/env sh

if [[ "$XANDEER_PROFILE" != loaded ]] ; then
    export XANDEER_PROFILE=loaded

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

    export PATH=$PATH:$SDK_DIR/platform-tools
    export PATH=$PATH:$SDK_DIR/tools

    export RUST_SRC_PATH=$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src

    # add doom path
    export PATH=$PATH:$HOME/.emacs.d/bin
fi
