{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.programs.blender;
in
{
  options.programs.blender = {
    enable = mkEnableOption "Blender";
    package =
      mkOption
        {
          type = types.package;
          default = pkgs.blender;
          description =
            ''
            Blender package containing a blender executable which
            wrapped with the provided plugins.
            '';
        };
    pythonPackages =
      mkOption
        {
          type = types.package;
          default = pkgs.python3.withPackages ( ps: with ps; [ numpy ] );
          description =
            ''
            Python packages required by Blender or third-party plugins.
            '';
          example =
            ''
            python3.withPackages (ps: with ps; [ numpy certifi ])
            '';
        };
    finalPackage =
      mkOption
        {
          type = types.package;
          visible = false;
          readOnly = true;
          description = "Resulting customized Blender package.";
        };
  };
  config =
    mkIf
      cfg.enable
      {
        environment.systemPackages = [ cfg.finalPackage ];
        programs.blender.finalPackage =
          pkgs.callPackage
            ../../pkgs/applications/blender
            {
              blender = cfg.package;
              pythonEnv = cfg.pythonPackages;
            };
      };
}
