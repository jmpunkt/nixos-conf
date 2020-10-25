{ pkgs, nodejs, stdenv, chromium, writeText, makeWrapper, which }:

let
  super = import ./composition.nix {
    inherit pkgs nodejs;
    inherit (stdenv.hostPlatform) system;
  };
in super // {
  # override goes here

  # https://github.com/NixOS/nixpkgs/issues/60057#issuecomment-505781308
  mermaid-cli = super."@mermaid-js/mermaid-cli".overrideAttrs (oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ])
      ++ [ makeWrapper which ];

    PUPPETEER_SKIP_CHROMIUM_DOWNLOAD = 1;

    nixpkgsChromePuppeteerConfig = writeText "puppeteerConfig.json" ''
      { "executablePath": "${chromium}/bin/chromium" }
    '';

    postInstall = (oldAttrs.postInstall or "") + ''
      wrapProgram $out/bin/mmdc --add-flags "-p $nixpkgsChromePuppeteerConfig"
    '';
  });
}
