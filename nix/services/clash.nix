{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.services.clash;
  configDir = if cfg.configDir != null then cfg.configDir
               else "${pkgs.xandeer.clash}/etc";
in {
  options.services.clash = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the clash service.";
    };
    configDir = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "The config folder to use for clash.";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.clash = {
      description = "clash Service";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.xandeer.clash}/bin/clash -d ${configDir}";
        Restart = "always";
        PIDFile = "/run/clash.pid";
      };
    };
  };
}
