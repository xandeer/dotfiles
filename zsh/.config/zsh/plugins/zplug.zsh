echo "Loading zsh plugins..."

export ZPLUG_HOME=$HOME/projects/others/zplug
source "$ZPLUG_HOME/init.zsh"
source_if_exists "$ZSH_CONFIG_DIR/plugins/bundles.zsh"

# Install plugins if there are plugins that have not been installed
if ! zplug check; then
    printf "Install plugins? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load
echo "Zsh plugins loaded."
