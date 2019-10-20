{
  services.xserver = {
    enable = true;
    layout = "us";
    videoDrivers = ["intel"];
    xkbOptions="caps:ctrl_modifier";
    libinput = {
      enable = true;
      naturalScrolling = true;
      tapping = true;
    };

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };
    windowManager = {
      default = "i3";
      i3.enable = true;
    };
    displayManager.lightdm = {
      enable = true;
      autoLogin.user = "kevin";
      autoLogin.enable = true;
    };
  };
}
