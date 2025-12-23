{
  systemConfig,
  pkgs,
  lib,
  ...
}:

let
  cfg = systemConfig.profiles.yubikey;
in
{
  config = lib.mkIf cfg.enable {
    home.file = {
      ".ssh/id_rsa.pub".text = builtins.readFile ./ssh.pub;
    };

    home.packages = with pkgs; [
      yubioath-flutter
    ];

    programs.git = {
      settings.user.email = "jmpunkt@outlook.com";
      signing = {
        key = "26CDFB2E4DB6B136";
        signByDefault = true;
      };
    };

    programs.gpg = {
      enable = true;
      publicKeys = [
        {
          source = ./yubico.pub;
          trust = 5; # highest possible trust, its my key :^)
        }
      ];
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };

    programs.ssh = {
      matchBlocks = {
        "*" = {
          identityFile = "~/.ssh/id_rsa.pub";
        };
      };
    };
  };
}
