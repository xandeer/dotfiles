echo "Loading zsh plugins..."

export ZPLUG_HOME=$HOME/projects/others/zplug
source "$ZPLUG_HOME/init.zsh"
source_if_exists "$ZSH_CONFIG_DIR/plugins/bundles.zsh"

zplugsync() {
  emulate -L zsh

  if ! zplug check; then
    zplug install
  fi

  zplug load
}

zplug load
echo "Zsh plugins loaded."
