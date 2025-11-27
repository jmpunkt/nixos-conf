{ pkgs, ... }:
{
  # gtk = {
  #   enable = true;
  #   font.name = "Noto Sans";
  #   iconTheme = {
  #     package = pkgs.kdePackages.breeze-icons;
  #     name = "breeze";
  #   };
  #   theme = {
  #     package = pkgs.kdePackages.breeze-gtk;
  #     name = "Breeze";
  #   };
  # };
  qt = {
    enable = true;
    platformTheme.name = "kde";
    style.name = "Breeze";
  };
}
