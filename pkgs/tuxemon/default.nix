{ lib, stdenv, fetchFromGitHub, python3Packages }:

with python3Packages;

let
  pytmx = buildPythonPackage rec {
    pname = "PyTMX";
    version = "3.21.7";

    propagatedBuildInputs = [
      six
      pygame
      pyglet
      pysdl2
    ];

    src = fetchPypi {
      inherit pname version;
      sha256 = "19wgjdhkzzi2kpnba6xpr29b24dqy3nhdvcfardfm738mzxc3sgn";
    };
  };
  pyscroll = buildPythonPackage rec {
    pname = "pyscroll";
    version = "2.19.2";

    propagatedBuildInputs = [ six pygame ];

    src = fetchPypi {
      inherit pname version;
      sha256 = "175d39rymnx3v3mcfjjkr1wv76nsl1s00z04nzsspklyk0ih2783";
    };
  };
in
buildPythonPackage rec {
  pname = "tuxemon";
  version = "0.4.2";

  src = fetchFromGitHub {
    owner = "Tuxemon";
    repo = "Tuxemon";
    rev = "73a751f908ccdceaee72710707f9dacb0e571d99";
    sha256 = "1q801k04805l68sh547dz3kqwsipqn0n16qsaqa390w5p6sbq819";
  };

  # Tuxemon does not support to build the translation during the build phase.
  # Transltations are build during the runtime -> not possible in Nix.
  broken = true;

  checkPhase = ''true'';

  propagatedBuildInputs = [
    requests
    Babel
    cbor
    pillow
    lxml
    pytmx
    pyscroll
    netira
  ];

  meta = with lib; {
    homepage = https://github.com/Tuxemon/Tuxemon;
    description = "Open source monster-fighting RPG.";
    license = licenses.gpl3;
    maintainers = with maintainers; [ ];
  };
}
