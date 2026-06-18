{
  pkgs,
  lib,
  ...
}: let
  port = toString 8008;
  updateScript = pkgs.writeShellScriptBin "update-hoogle" ''
    set -ex
    ${lib.getExe pkgs.haskellPackages.hoogle} generate -v --database /var/lib/private/hoogle/new-db.hoo
    mv /var/lib/private/hoogle/new-db.hoo /var/lib/private/hoogle/db.hoo
    set +ex
  '';
in {
  systemd.services.hoogle = {
    enable = true;
    description = "hoogle";
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      DynamicUser = true;
      WorkingDirectory = "%S/hoogle";
      StateDirectory = "hoogle";
      StateDirectoryMode = "0777";
      ConfigurationDirectory = "hoogle";
      RestartSec = 15;
      CapabilityBoundingSet = "";
      # Security
      NoNewPrivileges = true;
      # Sandboxing
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      PrivateUsers = true;
      ProtectHostname = true;
      ProtectClock = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = ["AF_UNIX AF_INET AF_INET6"];
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      PrivateMounts = true;
      # System Call Filtering
      SystemCallArchitectures = "native";
      SystemCallFilter = "~@clock @privileged @cpu-emulation @debug @keyring @module @mount @obsolete @raw-io @reboot @setuid @swap";

      ExecStart = ''
        ${pkgs.modern-hoogle}/bin/modern-hoogle \
          --asset-path ${pkgs.modern-hoogle-assets} \
          --db-path /var/lib/private/hoogle/db.hoo \
          --cutoff 30 \
          --trigger-delay 20 \
          --port ${port} \
          --host https://hoogle.mangoiv.com \
          +RTS -N2 -RTS
      '';
      Restart = "always";
    };
  };

  systemd.services.hoogle-updater = {
    enable = true;
    description = "hoogle";
    after = ["network.target"];
    startAt = "*-*-* 5:00:00";
    serviceConfig = {
      DynamicUser = true;
      WorkingDirectory = "%S/hoogle";
      StateDirectory = "hoogle";
      StateDirectoryMode = "0777";
      ConfigurationDirectory = "hoogle";
      RestartSec = 15;
      CapabilityBoundingSet = "";
      # Security
      NoNewPrivileges = true;
      # Sandboxing
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      PrivateUsers = true;
      ProtectHostname = true;
      ProtectClock = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = ["AF_UNIX AF_INET AF_INET6"];
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      PrivateMounts = true;
      # System Call Filtering
      SystemCallArchitectures = "native";
      SystemCallFilter = "~@clock @privileged @cpu-emulation @debug @keyring @module @mount @obsolete @raw-io @reboot @setuid @swap";

      Type = "oneshot";

      ExecStart = "${lib.getExe pkgs.bash} ${updateScript}/bin/update-hoogle";
    };
  };

  services.nginx.virtualHosts."hoogle.mangoiv.com" = {
    enableACME = true;
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://localhost:${port}";
      "/robots.txt".root = pkgs.writeTextDir "robots.txt" ''
        User-agent: *
        Disallow: /
      '';
    };
  };
}

