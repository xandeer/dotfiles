{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.services.xkeysnail;
  configFile = if cfg.configFile != null then cfg.configFile
               else "/etc/xkeysnail.py";
in {
  options.services.xkeysnail = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the xkeysnail service.";
    };
    configFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "The config file to use for xkeysnail.";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.xkeysnail = {
      description = "Xkeysnail Service";
      serviceConfig = {
        Type = "simple";
        ExecStart = "runuser -l kevin -c ${pkgs.xandeer.xkeysnail}/bin/xkeysnail --quiet --watch ${configFile}";
        Restart = "always";
        Environment="DISPLAY=:0";
      };
    };
  };
}
