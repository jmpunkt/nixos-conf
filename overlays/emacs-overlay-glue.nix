# NOTE: Only use repository data (Emacs source, packages) from
#       emacs-overlay and configure Emacs ourself.
{
  emacs-overlay,
  emacs-mirror,
}: self: super: let
  inherit (self) emacs emacsPackagesFor;
  inherit
    (super)
    pkgs
    stdenv
    lib
    ;
  # reconnect pkgs to the built emacs
  forwardPkgs = drv:
    drv.overrideAttrs (old: {
      passthru =
        old.passthru
        // {
          pkgs = emacsPackagesFor drv;
        };
    });
in {
  emacs-overlay =
    (super.emacs-overlay or {})
    // {
      repos = let
        inherit
          (super)
          lib
          fetchFromGitHub
          fetchFromSavannah
          ;
        fetchFromJson = jsonFile: let
          repoMeta = lib.importJSON jsonFile;
        in {
          src = (
            if repoMeta.type == "savannah"
            then fetchFromSavannah
            else if repoMeta.type == "github"
            then fetchFromGitHub
            else throw "Unknown repository type ${repoMeta.type}!"
          ) (builtins.removeAttrs repoMeta ["type" "version"]);
          manifest = repoMeta;
        };
      in {
        emacs = {
          lsp = fetchFromJson "${emacs-overlay}/repos/emacs/emacs-lsp.json";
          master = fetchFromJson "${emacs-overlay}/repos/emacs/emacs-master.json";
          unstable = fetchFromJson "${emacs-overlay}/repos/emacs/emacs-unstable.json";
          github = {
            src = emacs-mirror;
            manifest = {
              version = let
                version = lib.jmpunkt.unixTimestampToDateTime emacs-mirror.lastModified;
              in
                # Ensure we have a version number or not some random text.
                assert (builtins.stringLength version) <= 14; version;
              rev = emacs-mirror.rev;
            };
          };
        };
      };

      # Override an existing Emacs derivation with features (defined
      # below).
      #
      # Only overrides the Elisp packages provided by emacs-overlay. To
      # override the source of Emacs use `mkEmacsFromRepo`.
      mkEmacsWithFeatures = features:
        builtins.foldl'
        (drv: fn: fn drv)
        emacs
        (features ++ [forwardPkgs]);

      # Override an Emacs derivation with a custom source defined in
      # emacs-overlay (see repos/default.nix for defined repositories).
      #
      # Example mkEmacsFromRepo { name = "test"; repo = emacs-overlay.repos.emacs.master; }
      mkEmacsFromRepo = {
        name,
        features ? [],
        repository,
      }: let
        setSource = drv:
          (drv.override {srcRepo = true;}).overrideAttrs (old: {
            name = "${name}-${repository.manifest.version}";
            inherit (repository.manifest) version;
            inherit (repository) src;
            postPatch =
              old.postPatch
              + ''
                substituteInPlace lisp/loadup.el \
                --replace '(emacs-repository-get-version)' '"${repository.manifest.rev}"' \
                --replace '(emacs-repository-get-branch)' '"master"'
              '';
          });
      in
        builtins.foldl'
        (drv: fn: fn drv)
        emacs
        ([setSource]
          ++ features
          ++ [forwardPkgs]);

      enableIgc = drv:
        drv.overrideAttrs (
          old: {
            configureFlags = old.configureFlags ++ [(lib.withFeature true "mps")];
            buildInputs = old.buildInputs ++ [pkgs.mps];
          }
        );

      # Enable link-time optimization
      enableLTO = drv:
        drv.overrideAttrs (
          old: {
            configureFlags =
              old.configureFlags
              ++ lib.singleton "--enable-link-time-optimization";
          }
        );

      # Enable debuggin with GDB
      enableDebug = drv:
        drv.overrideAttrs (
          old: {
            dontStrip = true;
            CFLAGS = "-O0 -ggdb " + (old.CFLAGS or "");
            configureFlags =
              old.configureFlags
              ++ (lib.singleton "--enable-check-lisp-object-type");
          }
        );
    };
}
