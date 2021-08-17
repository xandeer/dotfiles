#!/bin/zsh -f
#

echo "Loading zsh plugins..."

export ZPLUG_HOME=$HOME/projects/others/zplug
source $ZPLUG_HOME/init.zsh

# zplug
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# Load theme
zplug "mafredri/zsh-async", from:github, use:async.zsh
# zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme, from:github, at:next, as:theme
zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme

zplug "hchbaw/zce.zsh", from:github, use:zce.zsh

# Miscellaneous commands
zplug "k4rthik/git-cal", as:command
zplug "peco/peco", as:command, from:gh-r, use:"*${(L)$(uname -s)}*amd64*"

# Supports oh-my-zsh plugins and the like
zplug "lib/completion",                   from:oh-my-zsh
zplug "lib/directories",                  from:oh-my-zsh
zplug "lib/git",                          from:oh-my-zsh
zplug "lib/history",                      from:oh-my-zsh
zplug "lib/theme-and-appearance",         from:oh-my-zsh
zplug "plugins/common-aliases",           from:oh-my-zsh
zplug "plugins/zsh_reload",               from:oh-my-zsh

if [[ $OSTYPE = (darwin)* ]]; then
    zplug "lib/clipboard",                from:oh-my-zsh
    zplug "plugins/osx",                  from:oh-my-zsh
    zplug "plugins/brew",                 from:oh-my-zsh, if:"(( $+commands[brew] ))"
fi

zplug "plugins/adb",                      from:oh-my-zsh, if:"(( $+commands[adb] ))"
zplug "plugins/cargo",                    from:oh-my-zsh, if:"(( $+commands[cargo] ))"
zplug "plugins/colored-man-pages",        from:oh-my-zsh
zplug "plugins/colorize",                 from:oh-my-zsh
zplug "plugins/command-not-found",        from:oh-my-zsh, if:"(( $+commands[cnf-lookup] ))"
zplug "plugins/git",                      from:oh-my-zsh, if:"(( $+commands[git] ))"
zplug "plugins/git-extras",               from:oh-my-zsh, if:"(( $+commands[git] ))"
zplug "plugins/git-flow-avh",             from:oh-my-zsh, if:"(( $+commands[git] ))"
zplug "plugins/gitignore",                from:oh-my-zsh, if:"(( $+commands[git] ))"
zplug "plugins/golang",                   from:oh-my-zsh, if:"(( $+commands[go] ))"
zplug "plugins/gradle",                   from:oh-my-zsh, if:"(( $+commands[gradle] ))"
zplug "plugins/nvm",                      from:oh-my-zsh, if:"(( $+commands[nvm] ))"
zplug "plugins/pass",                     from:oh-my-zsh, if:"(( $+commands[pass] ))"
zplug "plugins/rust",                     from:oh-my-zsh, if:"(( $+commands[rust] ))"
zplug "plugins/rvm",                      from:oh-my-zsh, if:"(( $+commands[rvm] ))"
zplug "plugins/sudo",                     from:oh-my-zsh, if:"(( $+commands[sudo] ))"
zplug "plugins/scd",                      from:oh-my-zsh
export FPATH=$FPATH:$ZPLUG_REPOS/robbyrussell/oh-my-zsh/plugins/scd
zplug "plugins/systemd",                  from:oh-my-zsh, if:"(( $+commands[systemctl] ))"
zplug "plugins/yarn",                     from:oh-my-zsh


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

# Adds aliases to open your current repo & branch on github.
zplug "peterhurford/git-it-on.zsh"

# A collection of scripts that might be useful to sysadmins.
zplug "skx/sysadmin-util"

[ -d "$HOME/bin" ] && export PATH="$HOME/bin:$PATH"

zplug "~/.config/zsh", from:local, defer:3

# Install plugins if there are plugins that have not been installed
if ! zplug check; then
    printf "Install plugins? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

if zplug check "mollifier/anyframe"; then
    # expressly specify to use peco
    #zstyle ":anyframe:selector:" use peco
    # expressly specify to use percol
    #zstyle ":anyframe:selector:" use percol
    # expressly specify to use fzf-tmux
    zstyle ":anyframe:selector:" use fzf-tmux
    # expressly specify to use fzf
    zstyle ":anyframe:selector:" use fzf

    # specify path and options for peco, percol, or fzf
    #zstyle ":anyframe:selector:peco:" command 'peco --no-ignore-case'
    #zstyle ":anyframe:selector:percol:" command 'percol --case-sensitive'
    zstyle ":anyframe:selector:fzf-tmux:" command 'fzf-tmux --extended'
    #zstyle ":anyframe:selector:fzf:" command 'fzf --extended'
    #zstyle ":anyframe:selector:fzf:" command 'fzf'

    #bindkey '^@' anyframe-widget-cd-ghq-repository
    #bindkey '^r' anyframe-widget-put-history
fi

if zplug check "zsh-users/zsh-history-substring-search"; then
    zmodload zsh/terminfo
    bindkey "$terminfo[kcuu1]" history-substring-search-up
    bindkey "$terminfo[kcud1]" history-substring-search-down
    bindkey "^[[1;5A" history-substring-search-up
    bindkey "^[[1;5B" history-substring-search-down
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
    bindkey -M emacs '^P' history-substring-search-up
    bindkey -M emacs '^N' history-substring-search-down
    bindkey -M vicmd '^p' history-substring-search-up
    bindkey -M vicmd '^n' history-substring-search-down
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down
fi

if zplug check "zsh-users/zsh-syntax-highlighting"; then
    typeset -gA ZSH_HIGHLIGHT_STYLES ZSH_HIGHLIGHT_PATTERNS

    ZSH_HIGHLIGHT_STYLES[default]='none'
    ZSH_HIGHLIGHT_STYLES[cursor]='fg=yellow'
    ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red'
    ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=yellow'
    ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan'
    ZSH_HIGHLIGHT_STYLES[builtin]='fg=cyan'
    ZSH_HIGHLIGHT_STYLES[function]='fg=cyan'
    ZSH_HIGHLIGHT_STYLES[command]='fg=cyan'
    ZSH_HIGHLIGHT_STYLES[precommand]='fg=green'
    ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=yellow'
    ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=green'
    ZSH_HIGHLIGHT_STYLES[path]='fg=white,underline'
    ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=grey,underline'
    ZSH_HIGHLIGHT_STYLES[path_prefix]='fg=white'
    ZSH_HIGHLIGHT_STYLES[path_approx]='fg=white'
    ZSH_HIGHLIGHT_STYLES[globbing]='none'
    ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=green'
    ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=blue'
    ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=blue'
    ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='none'
    ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=magenta'
    ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=magenta'
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=cyan'
    ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=cyan'
    ZSH_HIGHLIGHT_STYLES[redirection]='fg=magenta'
    ZSH_HIGHLIGHT_STYLES[bracket-level-1]='fg=cyan,bold'
    ZSH_HIGHLIGHT_STYLES[bracket-level-2]='fg=green,bold'
    ZSH_HIGHLIGHT_STYLES[bracket-level-3]='fg=magenta,bold'
    ZSH_HIGHLIGHT_STYLES[bracket-level-4]='fg=yellow,bold'
    ZSH_HIGHLIGHT_STYLES[assign]='none'

    ZSH_HIGHLIGHT_PATTERNS=('rm -rf *' 'fg=white,bold,bg=red')

    ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor line)

    ### Fix slowness of pastes with zsh-syntax-highlighting.zsh
    pasteinit() {
        OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
        zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
    }

    pastefinish() {
        zle -N self-insert $OLD_SELF_INSERT
    }
    zstyle :bracketed-paste-magic paste-init pasteinit
    zstyle :bracketed-paste-magic paste-finish pastefinish
    ### Fix slowness of pastes
fi

if zplug check "zsh-users/zsh-autosuggestions"; then
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=075'
    bindkey '^ ' autosuggest-accept
    bindkey '\es' autosuggest-execute
fi

if zplug check "denysdovhan/spaceship-prompt"; then
    SPACESHIP_PROMPT_ORDER=(
    time        # Time stampts section (Disabled)
    dir           # Current directory section
    git           # Git section (git_branch + git_status)
    exec_time     # Execution time
    line_sep      # Line break
    jobs          # Background jobs indicator
    char
    )

    SPACESHIP_TIME_SHOW=true
    SPACESHIP_EXIT_CODE_SHOW=true
    SPACESHIP_EXEC_TIME_SHOW=true
fi

if zplug check "hchbaw/zce.zsh"; then
  bindkey "\eg" zce
fi

# Then, source plugins and add commands to $PATH
zplug load
echo "Zsh plugins loaded."

# [ -z "$TMUX"  ] && { tmux attach || exec tmux new-session && exit;}
if [ -e /Users/kevin/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/kevin/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export NIX_PATH="darwin-config=$HOME/.nixpkgs/darwin-configuration.nix:/nix/var/nix/profiles/per-user/root/channels:$HOME/.nix-defexpr/channels"
