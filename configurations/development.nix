{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    tokei
    fzf
    git
    hyperfine
    direnv
    niv
  ];

  services.lorri.enable = true;
}
