{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [ tokei fzf hyperfine ];
}
