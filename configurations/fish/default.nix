{ config, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    promptInit = builtins.concatStringsSep "\n" [
      (builtins.readFile ./fish_functions/fish_print_colors.fish)
      (builtins.readFile ./fish_functions/fish_prompt.fish)
    ];
  };
}
