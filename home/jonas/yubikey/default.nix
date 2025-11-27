{ pkgs, ... }:
{
  home.file = {
    ".ssh/id_rsa.pub".text = builtins.readFile ./ssh.pub;
  };

  home.packages = with pkgs; [
    yubioath-flutter
  ];

  programs.git = {
    settings.user.email = "jmpunkt@outlook.com";
    signing = {
      key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
      signByDefault = true;
    };
  };

  programs.gpg.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    # NOTE: Use Gnome3 pinentry, works on Wayland and X11, in contrast to pinentry-gtk2
    pinentry.package = pkgs.pinentry-gnome3;
  };

  # NOTE: Required for pinentry-gnome3 to work
  dbus.packages = with pkgs; [ gcr ];

  programs.ssh = {
    matchBlocks = {
      "*" = {
        identityFile = "~/.ssh/id_rsa.pub";
      };
    };
  };
}
