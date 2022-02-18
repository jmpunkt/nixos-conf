{ config
, pkgs
, ...
}:
{
  imports = [ ./fish/default.nix ];
  environment.shellAliases = {
    ll = "${ pkgs.exa }/bin/exa --all --links --time-style=long-iso --long";
    ls = "${ pkgs.exa }/bin/exa --all --links";
    lr = "${ pkgs.exa }/bin/exa --tree";
  };
}
