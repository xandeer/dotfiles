{config, ...}: {
  # nix-darwin do not set PATH for nushell! so we need to do it manually
  # this is a workaround to add nix's PATH to nushell
  programs.nushell.enable = true;
  programs.oh-my-posh.enable = true;

  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.nushell.extraConfig = ''
    let-env PATH = ([
      "${config.home.homeDirectory}/bin"
      "/usr/local/bin"
      "${config.home.homeDirectory}/.nix-profile/bin"
      "/etc/profiles/per-user/${config.home.username}/bin"
      "/run/current-system/sw/bin"
      "/nix/var/nix/profiles/default/bin"
      "/opt/homebrew/bin"

      ($env.PATH | split row (char esep))
                                  ] | flatten)

    def nuopen [arg, --raw (-r)] { if $raw { open -r $arg } else { open $arg } }

    ### Theme
    let-env POWERLINE_COMMAND = "oh-my-posh"
    let-env POSH_THEME = ""
    let-env PROMPT_INDICATOR = ""
    let-env POSH_PID = (random uuid)
    # By default displays the right prompt on the first line
    # making it annoying when you have a multiline prompt
    # making the behavior different compared to other shells
    let-env PROMPT_COMMAND_RIGHT = ""
    let-env POSH_SHELL_VERSION = (version | get version)

    # PROMPTS
    let-env PROMPT_MULTILINE_INDICATOR = (^"oh-my-posh" print secondary $"--config=($env.POSH_THEME)" --shell=nu $"--shell-version=($env.POSH_SHELL_VERSION)")

    let-env PROMPT_COMMAND = { ||
        # We have to do this because the initial value of `$env.CMD_DURATION_MS` is always `0823`,
        # which is an official setting.
        # See https://github.com/nushell/nushell/discussions/6402#discussioncomment-3466687.
        let cmd_duration = if $env.CMD_DURATION_MS == "0823" { 0 } else { $env.CMD_DURATION_MS }

        # hack to set the cursor line to 1 when the user clears the screen
        # this obviously isn't bulletproof, but it's a start
        let clear = (history | last 1 | get 0.command) == "clear"

        let width = ((term size).columns | into string)
        ^"oh-my-posh" print primary $"--config=($env.POSH_THEME)" --shell=nu $"--shell-version=($env.POSH_SHELL_VERSION)" $"--execution-time=($cmd_duration)" $"--error=($env.LAST_EXIT_CODE)" $"--terminal-width=($width)" $"--cleared=($clear)"
    }
  '';

  programs.nushell.configFile = {
    text = ''
let light_theme = {
    # color for nushell primitives
    separator: dark_gray
    leading_trailing_space_bg: { attr: n } # no fg, no bg, attr none effectively turns this off
    header: green_bold
    empty: blue
    # Closures can be used to choose colors for specific values.
    # The value (in this case, a bool) is piped into the closure.
    bool: {|| if $in { "dark_cyan" } else { "dark_gray" } }
    int: dark_gray
    filesize: {|e|
      if $e == 0b {
        "dark_gray"
      } else if $e < 1mb {
        "cyan_bold"
      } else { "blue_bold" }
    }
    duration: dark_gray
  date: {|| (date now) - $in |
    if $in < 1hr {
      "purple"
    } else if $in < 6hr {
      "red"
    } else if $in < 1day {
      "yellow"
    } else if $in < 3day {
      "green"
    } else if $in < 1wk {
      "light_green"
    } else if $in < 6wk {
      "cyan"
    } else if $in < 52wk {
      "blue"
    } else { "dark_gray" }
  }
    range: dark_gray
    float: dark_gray
    string: dark_gray
    nothing: dark_gray
    binary: dark_gray
    cellpath: dark_gray
    row_index: green_bold
    record: white
    list: white
    block: white
    hints: dark_gray

    shape_and: purple_bold
    shape_binary: purple_bold
    shape_block: blue_bold
    shape_bool: light_cyan
    shape_closure: green_bold
    shape_custom: green
    shape_datetime: cyan_bold
    shape_directory: cyan
    shape_external: cyan
    shape_externalarg: green_bold
    shape_filepath: cyan
    shape_flag: blue_bold
    shape_float: purple_bold
    # shapes are used to change the cli syntax highlighting
    shape_garbage: { fg: white bg: red attr: b}
    shape_globpattern: cyan_bold
    shape_int: purple_bold
    shape_internalcall: cyan_bold
    shape_list: cyan_bold
    shape_literal: blue
    shape_match_pattern: green
    shape_matching_brackets: { attr: u }
    shape_nothing: light_cyan
    shape_operator: yellow
    shape_or: purple_bold
    shape_pipe: purple_bold
    shape_range: yellow_bold
    shape_record: cyan_bold
    shape_redirection: purple_bold
    shape_signature: green_bold
    shape_string: green
    shape_string_interpolation: cyan_bold
    shape_table: blue_bold
    shape_variable: purple
    shape_vardecl: purple
}

let-env config = {
  show_banner: false
  color_config: $light_theme
}
'';
  };

  programs.nushell.shellAliases = {
    open = "^open";
    ors = "darwin-rebuild switch --flake ${config.home.homeDirectory}/projects/personal/dotfiles/nix/";

    e = "emacsclient";

    q = "exit";

    # dir
    md = "mkdir";
    # zoxide
    s = "z";

    # ls
    l = "ls";
    ll = "ls -l";
    la = "ls -la";
    ldot = "ls .*";

    # git
    gc = "git checkout";
    gl = "git log";
    gst = "git status";

    # make
    m = "make";

    # yarn
    y = "yarn";
    ya = "yarn add";

    # npm
    n = "npm";
    ni = "npm -i";

    # brew
    # bf = "brew-file --no_appstore";
    # bi = "brew-file --no_appstore install";
    # bic = "brew-file --no_appstore --cask install";
    bi = "brew install";
    bu = "brew uninstall";
    buc = "brew uninstall --cask";
  };
}
