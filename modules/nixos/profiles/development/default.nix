{
  config,
  lib,
  ...
}:

let
  cfg = config.profiles.development;
in
{
  imports = [
    ./android.nix
  ];

  options.profiles.development = {
    enable = lib.mkEnableOption "Development tools";
  };

  config = lib.mkIf cfg.enable { };
}
