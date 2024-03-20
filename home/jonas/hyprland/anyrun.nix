{
  config,
  pkgs,
  inputs,
  ...
}: {
  programs.anyrun = {
    enable = true;
    config = {
      plugins = with inputs.anyrun.packages.${pkgs.system}; [
        applications
        randr
      ];
      width = {fraction = 0.3;};
      layer = "overlay";
      closeOnClick = false;
      showResultsImmediately = false;
      maxEntries = null;
    };
  };
}
