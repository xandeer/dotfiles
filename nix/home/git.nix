{ config, lib, pkgs, ... }: {
  # `programs.git` will generate the config file: ~/.config/git/config
  # to make git use this config file, `~/.gitconfig` should not exist!
  #
  #    https://git-scm.com/docs/git-config#Documentation/git-config.txt---global
  home.activation.removeExistingGitconfig = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    rm -f ~/.gitconfig
  '';

  programs.git = {
    enable = true;
    lfs.enable = true;

    userName = "Kevin Du";
    userEmail = "kkxandeer@gmail.com";

    includes = [
      {
        path = "~/projects/personal/.gitconfig";
        condition = "gitdir:~/projects/personal/";
      }
      {
        path = "~/projects/xmind/.gitconfig";
        condition = "gitdir:~/projects/xmind/";
      }
    ];

    ignores = [
      # Mac
      ".DS_Store"

      # Clojure
      ".lein-repl-history"
      ".lein-failures"
      "completer.hist"
      ".nrepl-port"
      "repl-port"

      # Node
      "node_modules"

      # Logs
      "*.log"

      # jetbrain
      ".idea/inspectionProfiles/"
      ".idea/androidTestResultsUserPreferences.xml"
      ".idea/kotlinc.xml"
      ".idea/migrations.xml"
      ".idea/appInsightsSettings.xml"
      ".idea/deploymentTargetSelector.xml"
      ".idea/other.xml"
    ];

    extraConfig = {
      init.defaultBranch = "main";
      push.autoSetupRemote = true;
      pull.rebase = true;
      apply.whitespace = "fix";
      github.user = "xandeer";
      core = {
        ignorecase = false;
        # Treat spaces before tabs and all kinds of trailing whitespace as an error
	      # [default] trailing-space: looks for spaces at the end of a line
	      # [default] space-before-tab: looks for spaces before tabs at the beginning of a line
	      whitespace = "space-before-tab,-indent-with-non-tab,trailing-space";
      };
    };

    # signing = {
    #   key = "xxx";
    #   signByDefault = true;
    # };

    delta = {
      enable = true;
      options = {
        features = "side-by-side";
      };
    };

    aliases = {
      # common aliases
      br = "branch";
      co = "checkout";
      st = "status";
      ls = "log --pretty=format:\"%C(yellow)%h%Cred%d\\\\ %Creset%s%Cblue\\\\ [%cn]\" --decorate";
      ll = "log --pretty=format:\"%C(yellow)%h%Cred%d\\\\ %Creset%s%Cblue\\\\ [%cn]\" --decorate --numstat";
      cm = "commit -m";
      ca = "commit -am";
      dc = "diff --cached";
      amend = "commit --amend -m";

      # aliases for submodule
      update = "submodule update --init --recursive";
      foreach = "submodule foreach";
    };
  };
}
