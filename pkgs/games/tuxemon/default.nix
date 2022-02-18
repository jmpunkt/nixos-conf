{ lib
, fetchFromGitHub
, python3Packages
}:
python3Packages.buildPythonApplication
  rec {
    pname = "tuxemon";
    version = "0.4.25";
    src =
      fetchFromGitHub
        {
          owner = "Tuxemon";
          repo = "Tuxemon";
          rev = "2c741edc626073b65dbbca55dc30560928a2f44e";
          sha256 = "0haml8q8njnprnrsyn7fibdswayq7nvq7y4sy1b7198c1bqbiwip";
        };
    # Tuxemon does not support to build the translation during the build phase.
    # Translations are build during the runtime -> not possible in Nix.
    broken = true;
    checkPhase =
      ''
      python -m unittest py3_tests/tuxemon/core/*.py
      '';
    propagatedBuildInputs =
      with python3Packages;
      [
        six
        natsort
        click
        requests
        Babel
        cbor
        pillow
        pygame
        lxml
        jmpunktPkgs.pytmx
        jmpunktPkgs.pyscroll
        jmpunktPkgs.netira
      ];
    meta =
      with lib;
      {
        homepage = "https://github.com/Tuxemon/Tuxemon";
        description = "Open source monster-fighting RPG.";
        license = licenses.gpl3;
        maintainers = with maintainers; [ ];
      };
  }
