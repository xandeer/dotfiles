{ config, ... }: {
  programs.kitty = {
    enable = true;
    keybindings = {
      # tab
      "alt+1" = "goto_tab 1";
      "alt+2" = "goto_tab 2";
      "alt+3" = "goto_tab 3";
      "alt+4" = "goto_tab 4";

      # reload kitty.conf
      "cmd+<" = "load_config_file";

      # open xandeer.conf
      "cmd+," = "launch emacsclient -n ~/projects/personal/dotfiles/nix/home/kitty.nix";
    };
    settings = {
      font_family = "FiraCode Nerd Font Mono";
      exe_search_path = "+/etc/profiles/per-user/${config.home.username}/bin";
      shell = "nu";
      editor = "emacsclient";
      copy_on_select = "clipboard";
    };
    theme = "Atom One Light";
  };
}