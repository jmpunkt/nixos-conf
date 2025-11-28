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
        key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
        signByDefault = true;
      };
    };

    programs.gpg.enable = true;

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
