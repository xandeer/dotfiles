{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.services.v2ray;
in {
  options.services.v2ray = {
    enable = mkEnableOption "v2ray service";
    configFile = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.v2ray = {
      description = "V2ray Service";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      preStart = "mkdir -p /var/log/v2ray";
      serviceConfig =
        let
          configFile = if cfg.configFile != null then cfg.configFile else "${pkgs.v2ray}/etc/config.json";
        in {
          Type = "simple";
          ExecStart = "${pkgs.v2ray}/bin/v2ray -config ${configFile}";
          Restart = "always";
          PIDFile = "/run/v2ray.pid";
        };
    };
  };
}
