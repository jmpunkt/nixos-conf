{
  imports = [
    ./germany.nix
  ];

  profiles.desktop.wwm.keyboardLayouts = [
    {
      default = true;
      layout = "de";
      options = "ctrl:nocaps";
    }
    {
      layout = "us";
      variant = "intl";
      options = "ctrl:nocaps";
    }
  ];
}
