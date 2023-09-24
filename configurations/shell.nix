{
  config,
  pkgs,
  ...
}: {
  imports = [./fish/default.nix];
  environment.shellAliases = {
    ll = "${pkgs.eza}/bin/eza --all --links --time-style=long-iso --long";
    ls = "${pkgs.eza}/bin/eza --all --links";
    lr = "${pkgs.eza}/bin/eza --tree";
  };
}
