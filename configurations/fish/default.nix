{ config, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    promptInit = builtins.concatStringsSep "\n" [
      (builtins.readFile ./fish_functions/fish_nix.fish)
      (builtins.readFile ./fish_functions/fish_print_colors.fish)
      (builtins.readFile ./fish_functions/fish_prompt.fish)
    ];
    loginShellInit = ''
      set -x EDITOR "nvim"
      set -x BROWSER "firefox"
      set -x TERMINAL "alacritty"
    '';
  };
}
