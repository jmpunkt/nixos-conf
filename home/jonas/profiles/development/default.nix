{
  systemConfig,
  pkgs,
  lib,
  ...
}:
let
  cfg = systemConfig.profiles.development;
in
{
  imports = [
    ./emacs
  ];

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      GTAGSCONF = "${pkgs.global}/share/gtags/gtags.conf";
      GTAGSLABEL = "pygments";
      MAKEOBJDIRPREFIX = "/home/jonas/.cache/gtags";
    };
    systemd.user.tmpfiles.rules = [
      "D /home/jonas/.cache/gtags 0755 jonas jonas 7d -"
    ];

    services.lorri = {
      enable = true;
      nixPackage = systemConfig.nix.package;
    };
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    programs.mergiraf.enable = true;
    programs.difftastic.enable = true;
    programs.git = {
      enable = true;
      settings = {
        user.name = "Jonas Meurer";
        core = {
          whitespace = "trailing-space,space-before-tab";
        };
        rerere.enabled = "true";
        merge.conflictstyle = "diff3";
        pull.ff = "only";
      };
      lfs.enable = true;
    };
  };
}
