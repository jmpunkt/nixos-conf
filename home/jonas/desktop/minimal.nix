{pkgs, ...}: {
  home.packages = with pkgs; [
    # cli
    hyperfine
    nix-tree
    sqlite
    tokei

    # spelling
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    enchant
    hunspell
    hunspellDicts.en-us-large
    jmpunkt.hunspellDicts.de-de
  ];
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-bin.unwrapped;
    languagePacks = ["de" "en-US"];
    policies = let
      lock-false = {
        Value = false;
        Status = "locked";
      };
      lock-true = {
        Value = true;
        Status = "locked";
      };
    in {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };
      DisablePocket = true;
      DisableFirefoxAccounts = true;
      DisableAccounts = true;
      DontCheckDefaultBrowser = true;
      DisplayBookmarksToolbar = "never";
      SearchBar = "unified";
      HttpsOnlyMode = "enabled";
      PostQuantumKeyAgreementEnabled = true;
      TranslateEnabled = false;
      # NOTE: Not working with non ESR versions
      SearchEngines = {
        Default = "DuckDuckGo";
        DefaultPrivate = "DuckDuckGo";
        Remove = [
          "Google"
          "Bing"
          "Amazon.de"
          "eBay"
          "Ecosia"
          "LEO Eng-Deu"
          "Wikipedia (de)"
        ];
        PreventInstalls = true;
      };
      SanitizeOnShutdown = {
        Downloads = true;
        Cache = true;
      };

      # Minimal extensions, everything else should be installed manually, depending on the device.
      ExtensionSettings = {
        # uBlock Origin:
        "uBlock0@raymondhill.net" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
          installation_mode = "force_installed";
        };
        # Privacy Badger:
        "jid1-MnnxcxisBPnSXQ@jetpack" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/privacy-badger17/latest.xpi";
          installation_mode = "force_installed";
        };

        # Disable default search engine add-ons, except for DuckDuckGo
        "amazon@search.mozilla.org" = {
          "installation_mode" = "blocked";
        };
        "bing@search.mozilla.org" = {
          "installation_mode" = "blocked";
        };
        "ebay@search.mozilla.org" = {
          "installation_mode" = "blocked";
        };
        "ecosia@search.mozilla.org" = {
          "installation_mode" = "blocked";
        };
        "google@search.mozilla.org" = {
          "installation_mode" = "blocked";
        };
        "leo_ende_de@search.mozilla.org" = {
          "installation_mode" = "blocked";
        };
        "wikipedia@search.mozilla.org" = {
          "installation_mode" = "blocked";
        };
      };

      Preferences = {
        "browser.contentblocking.category" = {
          Value = "strict";
          Status = "locked";
        };
        "extensions.pocket.enabled" = lock-false;
        "browser.topsites.contile.enabled" = lock-false;
        "browser.formfill.enable" = lock-false;
        "browser.search.suggest.enabled" = lock-false;
        "browser.search.suggest.enabled.private" = lock-false;
        "browser.urlbar.suggest.searches" = lock-false;
        "browser.urlbar.showSearchSuggestionsFirst" = lock-false;
        "browser.newtabpage.activity-stream.feeds.section.topstories" = lock-false;
        "browser.newtabpage.activity-stream.feeds.snippets" = lock-false;
        "browser.newtabpage.activity-stream.section.highlights.includePocket" = lock-false;
        "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = lock-false;
        "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = lock-false;
        "browser.newtabpage.activity-stream.section.highlights.includeVisited" = lock-false;
        "browser.newtabpage.activity-stream.showSponsored" = lock-false;
        "browser.newtabpage.activity-stream.system.showSponsored" = lock-false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = lock-false;
      };
    };
  };
}
