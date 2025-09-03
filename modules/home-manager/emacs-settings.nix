{
  lib,
  pkgs,
  config,
  ...
}:
let

  inherit (lib)
    mkOption
    literalExpression
    ;

  cfg = config.programs.emacs;

  formatElispSet = {
    type =
      with lib.types;
      let
        valueType =
          (oneOf [
            bool
            int
            float
            str
            path
            (attrsOf valueType)
            (listOf valueType)
          ])
          // {
            description = "Elisp value";
          };
      in
      attrsOf valueType;

    generate =
      value:
      let
        valueToElisp =
          value:
          if (builtins.typeOf value) == "list" then
            "'(${builtins.concatStringsSep "\n" (builtins.map valueToElisp value)})"
          else if (builtins.isString value) || (builtins.isPath value) then
            ''"${value}"''
          else if (builtins.isNull value) || ((builtins.isBool value) && value == false) then
            "nil"
          else if (builtins.isBool value) then
            "t"
          else if (builtins.isInt value) || (builtins.isFloat value) then
            "${value}"
          else if (builtins.isAttrs value) then
            "'(${
              builtins.concatStringsSep " " (
                lib.attrsets.mapAttrsToList (key: value: "(${key} . ${valueToElisp value})") value
              )
            })"
          else
            lib.assertMsg false "${builtins.typeOf value} can not translated into Elisp";
      in
      builtins.concatStringsSep "\n" (
        lib.mapAttrsToList (name: value: ''(setq-default ${name} ${valueToElisp value})'') value
      );

  };

in
{
  options = {
    programs.emacs = {
      variables = mkOption {
        type = formatElispSet.type;
        default = { };
        example = literalExpression ''
          {
            completion-at-point-functions = "hi";
          }
        '';
        description = ''
          Emacs settings as setq-default Elisp commands.
        '';
      };
      paths = mkOption {
        type = with lib.types; listOf path;
        default = { };
        example = literalExpression ''
          [ pkgs.hello ]
        '';
        description = ''
          Emacs paths to be added to `exec-path` and `PATH` environment variable.
        '';
      };
    };
  };

  config = {
    programs.emacs.extraPackages = epkgs: [
      (epkgs.manualPackages.jmpunktPkgs.nixosPaths {
        variables = formatElispSet.generate cfg.variables;
        paths = cfg.paths;
      })
    ];
  };
}
