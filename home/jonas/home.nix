{
  config,
  pkgs,
  lib,
  xdgData,
  ...
}: let
  ge =
    pkgs.stdenv.mkDerivation
    rec {
      name = "proton-ge";
      version = "7.0rc3-GE-1";
      src =
        pkgs.fetchzip
        {
          url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/Proton-${version}.tar.gz";
          sha256 = "j+Cqq2+VKhsPk8yOHKWVotTcvucDkxDSd6K+pV1ceds=";
        };
      nativeBuildInputs = [];
      installPhase = ''
        mkdir -p $out
        mv * $out/
      '';
    };
in {
  imports = import ../../modules/all-home-manager.nix;
  home.language = {
    base = "en_US.utf8";
    address = "de_DE.utf8";
    monetary = "de_DE.utf8";
    paper = "de_DE.utf8";
    time = "de_DE.utf8";
  };
  # home.activation =
  #   {
  #     proton-ge = (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #       target_dir="/home/jonas/.steam/root/compatibilitytools.d"
  #       if ! [ -d "$target_dir" ]; then
  #         mkdir -p $target_dir
  #       fi
  #       target="$target_dir/Proton-${ge.version}"
  #       if ! [ -d "$target" ]; then
  #         cp -R ${ge} "$target"
  #         chmod -R u+w "$target" # not strictly necessary...
  #       fi
  #     '');
  #   };
  home.packages = with pkgs; [
    binutils-unwrapped
    dropbox-cli
    pdfpc
    cryptsetup
    gnupg
    feh
    thunderbird
    audacious
    discord
    tdesktop
    tokei
    git
    gitAndTools.delta
    hyperfine
    hunspell
    hunspellDicts.en-us-large
    jmpunkt.hunspellDicts.de-de
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    jmpunkt.chatterino2-nigthly
    jmpunkt.emacs
    jmpunkt.latex
  ];
  xdg.configFile = {
    "emacs/init.el".text = builtins.readFile ./emacs/init.el;
    # since emacs 27.1
    "emacs/early-init.el".text = builtins.readFile ./emacs/early-init.el;
  };
  home.file = {
    ".ssh/id_rsa.pub".text = builtins.readFile ./ssh/yubikey.pub;
    ".ssh/config".text = builtins.readFile ./ssh/config;
    ".steam/root/compatibilitytools.d/Proton-${ge.version}" = {source = ge;};
  };
  programs = {
    firefox = {
      enable = false;
      profiles = {
        home = {
          id = 0;
          settings = {
            "browser.toolbars.bookmarks.visibility" = "never";
            "gfx.webrender.all" = true;
            "media.ffmpeg.vaapi.enabled" = true;
            "media.ffvpx.enabled" = false;
            "network.dns.disablePrefetch" = true;
            "dom.security.https_only_mode" = true;
            "dom.security.https_only_mode_ever_enabled" = true;
            "dom.security.https_only_mode_send_http_background_request" = false;
            "dom.security.https_first" = true;
            "dom.vibrator.max_vibrate_ms" = 0;
            "dom.enable_performance" = false;
            "dom.enable_performance_navigation_timing" = false;
            "dom.enable_resource_timing" = false;
            "app.normandy.enabled" = false;
            "app.shield.optoutstudies.enabled" = false;
            "browser.display.use_document_fonts" = 0;
            "extensions.pocket.enabled" = false;
            "extensions.htmlaboutaddons.recommendations.enabled" = false;
            "toolkit.coverage.opt-out" = true;
            "toolkit.coverage.endpoint.base" = "";
            "toolkit.telemetry.archive.enabled" = false;
            "toolkit.telemetry.coverage.opt-out" = true;
            "toolkit.telemetry.firstShutdownPing.enabled" = false;
            "toolkit.telemetry.hybridContent.enabled" = false;
            "toolkit.telemetry.bhrPing.enabled" = false;
            "toolkit.telemetry.newProfilePing.enabled" = false;
            "toolkit.telemetry.shutdownPingSender.enabled" = false;
            "toolkit.telemetry.updatePing.enabled" = false;
            "toolkit.telemetry.unified" = false;
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;
            "browser.ping-centre.telemetry" = false;
            "privacy.donottrackheader.enabled" = true;
            "privacy.sanitize.sanitizeOnShutdown" = true;
            "privacy.userContext.enabled" = true;
            "privacy.userContext.ui.enabled" = true;
            "security.identityblock.show_extended_validation" = true;
            "security.insecure_connection_icon.enabled" = true;
            "security.insecure_connection_icon.pbmode.enabled" = true;
            "security.insecure_connection_text.enabled" = true;
            "security.insecure_connection_text.pbmode.enabled" = true;
            "security.mixed_content.upgrade_display_content" = true;
            "security.cert_pinning.enforcement_level" = 2;
            "security.certerrors.mitm.auto_enable_enterprise_roots" = false;
            "security.family_safety.mode" = 0;
            "security.ssl.require_safe_negotiation" = true;
            "security.ssl.treat_unsafe_negotiation_as_broken" = true;
            "media.navigator.enabled" = false;
            "media.navigator.video.enabled" = false;
            "media.peerconnection.enabled" = false;
            "media.video_stats.enabled" = false;
            "media.webspeech.synth.enabled" = false;
            "media.autoplay.default" = 5;
            "media.autoplay.blocking_policy" = 2;
            "geo.enabled" = false;
            "webgl.disabled" = true;
            "webgl.enable-webgl2" = false;
            "pdfjs.enableScripting" = false;
            "plugin.default.state" = 0;
            "ui.use_standins_for_native_colors" = true;
            "webgl.enable-debug-renderer-info" = false;
            "app.update.auto" = false;
          };
        };
      };
    };
    direnv.enable = true;
    direnv.nix-direnv = {
      enable = true;
    };
    git = {
      enable = true;
      userName = "Jonas Meurer";
      userEmail = "jmpunkt@outlook.com";
      signing = {
        key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
        signByDefault = true;
      };
      extraConfig = {
        core = {
          editor = "emacs";
          pager = "delta";
        };
        interactive.diffFilter = "delta --color-only";
        delta = {
          features = "line-numbers";
          whitespace-error-style = "22 reverse";
          decorations = {
            commit-decoration-style = "bold yellow box ul";
            file-style = "bold yellow ul";
            file-decoration-style = "none";
          };
        };
      };
    };
  };
  services.dropbox.enable = true;
}
