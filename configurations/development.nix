{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    gcc
    gnumake
    tokei
    fzf
    hyperfine
  ];
}
