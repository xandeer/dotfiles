{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.services.v2ray;
  configFile = if cfg.configFile != null then cfg.configFile
               else "${pkgs.xandeer.v2ray}/etc/config.json";
in {
  options.services.v2ray = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the v2ray service.";
    };
    configFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "The config file to use for v2ray.";
    };
  };

  config = mkIf cfg.enable {
    launchd.daemons.v2ray = {
      command = "${pkgs.xandeer.v2ray}/bin/v2ray -config ${configFile}";
      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
      };
    };
    system.activationScripts.preActivation.text = "mkdir -p /var/log/v2ray";
  };
}
