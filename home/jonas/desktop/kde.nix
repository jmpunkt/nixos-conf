{pkgs, ...}: {
  gtk = {
    enable = true;
    font.name = "Noto Sans";
    font.size = 10;
    iconTheme = {
      package = pkgs.kdePackages.breeze-icons;
      name = "breeze";
    };
    theme = {
      package = pkgs.kdePackages.breeze-gtk;
      name = "Breeze";
    };
  };
  qt = {
    enable = true;
    platformTheme.name = "kde6";
    style.name = "Breeze";
  };
}
