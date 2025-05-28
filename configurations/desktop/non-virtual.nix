{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./minimal.nix];

  nix.settings = {
    substituters = [
      "https://emacs-ci.cachix.org"
    ];
    trusted-public-keys = [
      "emacs-ci.cachix.org-1:B5FVOrxhXXrOL0S+tQ7USrhjMT5iOPH+QN9q0NItom4="
    ];
  };

  hardware.graphics.enable = true;

  # scanner
  hardware = {
    sane.enable = true;
    sane.extraBackends = with pkgs; [hplipWithPlugin];
    sane.disabledDefaultBackends = ["escl"];
  };

  # sound
  security.rtkit.enable = true;
  services = {
    pipewire = {
      enable = true;
      pulse = {enable = true;};
      alsa = {
        enable = true;
        support32Bit = true;
      };
    };
  };

  # printing
  services = {
    printing.enable = true;
    printing.drivers = with pkgs; [hplip];
    avahi = {
      enable = true;
      nssmdns4 = true;
      nssmdns6 = true;
    };
  };

  # dns
  networking.nameservers = [
    # Quad9
    "9.9.9.9"
    "149.112.112.112"
    "2620:fe::fe"
    "2620:fe::9"
  ];
  services.resolved = {
    enable = true;
    dnssec = "true";
    dnsovertls = "opportunistic";
    llmnr = "false";
    fallbackDns = [
      # Cloudflare
      "1.1.1.1"
      "1.0.0.1"
      "2606:4700:4700::1111"
      "2606:4700:4700::1001"
    ];
  };
  networking.networkmanager = {
    settings.connection = {
      "ipv6.addr-gen-mode" = "stable-privacy";
      "ipv6.ip6-privacy" = "2";
    };
  };
}
