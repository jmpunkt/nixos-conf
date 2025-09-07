{ emacs-overlay }:
(
  (emacs-overlay.mkEmacsFromRepo {
    name = "emacs-jmpunkt";
    features = with emacs-overlay; [
      enableLTO
      enableWideInt
      enableIgc
    ];
    repository = emacs-overlay.repos.emacs.igc;
  }).override
  {
    withXinput2 = true;
    withWebP = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withPgtk = true;
    withXwidgets = false;
    withNativeCompilation = true;
  }
).overrideAttrs
  (
    old:
    let
      patchesWithoutFix = builtins.filter (
        e: !(builtins.isAttrs e) || (e.name != "?id=53a5dada413662389a17c551a00d215e51f5049f")
      ) old.patches;
    in
    {
      patches = patchesWithoutFix ++ [
        # keep an eye on (https://github.com/tyler-dodge/emacs/commit/e56c55a742c0f0d152afb4958b863cdf7207b4c3), not working for Linux since it
        # is a MacOS workaround, patch below works fine and has
        # taken from https://github.com/geza-herman/emacs/commit/784a9fd3d511b7f6794f713a8d0b1370ab1b2401
        ./improved-reading.patch
      ];
      env = (old.env or { }) // {
        CFLAGS = (
          toString [
            "-flto"
            "-march=x86-64-v3"
            "-fomit-frame-pointer"
            "-funroll-loops"
            "-frename-registers"
            "-fno-signed-zeros"
            "-fno-semantic-interposition"
            "-pipe"
          ]
        );
      };
    }
  )
