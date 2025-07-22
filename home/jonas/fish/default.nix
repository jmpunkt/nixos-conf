{
  config,
  pkgs,
  ...
}:
{
  home.shell.enableFishIntegration = true;

  xdg.configFile = {
    "fish/functions/fish_prompt.fish".source = ./fish_functions/fish_prompt.fish;
  };

  programs.fish = {
    enable = true;
    functions = {
      "__fish_current_nix_pkgs" = ''
        # Returns all paths which are loaded inside $PATH by `direnv` or `nix shell`
        set result (echo $PATH | tr '[: ]' '\n' | grep '/nix/store' | sed 's#^/nix/store/[a-z0-9]\+-##'| sed 's#-[^-]\+$##' | xargs -n2 -d'\n')
        echo $result
      '';
    };
  };
}
