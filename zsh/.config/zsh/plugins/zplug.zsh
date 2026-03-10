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

zplug load
echo "Zsh plugins loaded."
