# zplug
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# Load theme
zplug "mafredri/zsh-async", from:github, use:async.zsh
# zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme, from:github, at:next, as:theme
zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme

zplug "hchbaw/zce.zsh", from:github, use:zce.zsh

# Miscellaneous commands
# zplug "k4rthik/git-cal", as:command
# zplug "peco/peco", as:command, from:gh-r, use:"*${(L)$(uname -s)}*amd64*"

# Supports oh-my-zsh plugins and the like
zplug "lib/completion",                   from:oh-my-zsh
# zplug "lib/theme-and-appearance",         from:oh-my-zsh
zplug "plugins/zsh_reload",               from:oh-my-zsh

if [[ $OSTYPE = (darwin)* ]]; then
    zplug "plugins/osx",                  from:oh-my-zsh
    # zplug "plugins/brew",                 from:oh-my-zsh, if:"(( $+commands[brew] ))"
fi

zplug "plugins/adb",                      from:oh-my-zsh, if:"(( $+commands[adb] ))"
zplug "plugins/cargo",                    from:oh-my-zsh, if:"(( $+commands[cargo] ))"
zplug "plugins/colored-man-pages",        from:oh-my-zsh
zplug "plugins/command-not-found",        from:oh-my-zsh, if:"(( $+commands[cnf-lookup] ))"
# zplug "plugins/emacs",                    from:oh-my-zsh, if:"(( $+commands[emacsclient] ))"
zplug "plugins/git",                      from:oh-my-zsh, if:"(( $+commands[git] ))"
# zplug "plugins/git-flow-avh",             from:oh-my-zsh, if:"(( $+commands[git] ))"
zplug "plugins/golang",                   from:oh-my-zsh, if:"(( $+commands[go] ))"
# zplug "plugins/gradle",                   from:oh-my-zsh, if:"(( $+commands[gradle] ))"
zplug "plugins/pass",                     from:oh-my-zsh, if:"(( $+commands[pass] ))"
zplug "plugins/rust",                     from:oh-my-zsh, if:"(( $+commands[rust] ))"
zplug "plugins/rvm",                      from:oh-my-zsh, if:"(( $+commands[rvm] ))"
zplug "plugins/sudo",                     from:oh-my-zsh, if:"(( $+commands[sudo] ))"
zplug "plugins/scd",                      from:oh-my-zsh
export FPATH=$FPATH:$ZPLUG_REPOS/robbyrussell/oh-my-zsh/plugins/scd
zplug "plugins/systemd",                  from:oh-my-zsh, if:"(( $+commands[systemctl] ))"

# Warn you when you run a command that you've set an alias for without
# using the alias.
zplug "djui/alias-tips"

zplug "hlissner/zsh-autopair", defer:2
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
# zsh-syntax-highlighting must be loaded after executing compinit command
# and sourcing other plugins
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-history-substring-search", defer:3

zplug "Aloxaf/fzf-tab", from:github, if:"(( $+commands[fzf] ))"

# Adds aliases to open your current repo & branch on github.
# zplug "peterhurford/git-it-on.zsh"

# A collection of scripts that might be useful to sysadmins.
# zplug "skx/sysadmin-util"
