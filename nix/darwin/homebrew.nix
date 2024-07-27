{ ... }: {
  # Homebrew Mirror in China
  environment.variables = {
    HOMEBREW_API_DOMAIN = "https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles/api";
    HOMEBREW_BOTTLE_DOMAIN = "https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles";
    HOMEBREW_BREW_GIT_REMOTE = "https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/brew.git";
    HOMEBREW_CORE_GIT_REMOTE = "https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-core.git";
    HOMEBREW_PIP_INDEX_URL = "https://pypi.tuna.tsinghua.edu.cn/simple";
  };

  homebrew = {
    # enable = true;
    enable = false;

    onActivation = {
      autoUpdate = false;
      # 'zap': uninstalls all formulae(and related files) not listed here.
      cleanup = "zap";
    };

    # Applications to install from Mac App Store using mas.
    # You need to install all these Apps manually first so that your apple account have records for them.
    # otherwise Apple Store will refuse to install them.
    # For details, see https://github.com/mas-cli/mas
    masApps = {
      # Xcode = 497799835;
      # Wechat = 836500024;
      # NeteaseCloudMusic = 944848654;
      # QQ = 451108668;
      # WeCom = 1189898970;  # Wechat for Work
      # TecentMetting = 1484048379;
      # QQMusic = 595615424;
    };

    taps = [
      "homebrew/core"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/services"
      "homebrew/cask-versions"
      "d12frosted/emacs-plus"
      "yqrashawn/goku"
    ];

    # `brew install`
    brews = [
      "wget"  # download tool
      "curl"  # no not install curl via nixpkgs, it's not working well on macOS!
      # "aria2"  # download tool
      "httpie"  # http client
      "goku" # karabiner by edn watcher
      "sdcv"
      "trash"
      "wakatime-cli"
      "imagemagick"
      # "pinentry"
      "enchant"
      "ffmpeg"

      "gnupg"

      {
        name = "node@16";
        link = true;
        conflicts_with = [ "node" ];
      }

      # reinstall after terminal and shell prepared to inherit PATH environment.
      # brew reinstall emacs-plus@30 --with-xwidgets --with-imagemagick
      {
        name = "emacs-plus@30";
        # link = true;
        args = [ "with-xwidgets" "with-imagemagick" ];
      }
    ];

    # `brew install --cask`
    # TODO Feel free to add your favorite apps here.
    casks = [
      # "firefox"
      # "google-chrome"
      "visual-studio-code"

      # IM & audio & remote desktop & meeting
      # "telegram"
      # "discord"

      "anki"
      "clashx-pro"    # proxy tool
      # "iina"      # video player
      # "openinterminal-lite"  # open current folder in terminal
      "syncthing"  # file sync
      "raycast"   # (HotKey: alt/option + space)search, caculate and run scripts(with many plugins)
      # "iglance"   # beautiful system monitor
      # "eudic"     # 欧路词典
      "alt-tab"
      "kitty"
      "karabiner-elements"
      "showyedge"
      "squirrel"

      # "emacs-plus@30 --with-xwidgets --with-imagemagick"

      # Development
      # "insomnia"  # REST client
      # "wireshark"  # network analyzer
    ];
  };
}
