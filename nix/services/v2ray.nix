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
    systemd.services.v2ray = {
      description = "V2ray Service";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      preStart = "mkdir -p /var/log/v2ray";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.xandeer.v2ray}/bin/v2ray -config ${configFile}";
        Restart = "always";
        PIDFile = "/run/v2ray.pid";
      };
    };
  };
}
