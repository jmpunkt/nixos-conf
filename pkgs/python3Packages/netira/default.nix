{ lib
, python3Packages
, fetchFromGitHub
}:
with python3Packages;
buildPythonPackage
  rec {
    pname = "netira";
    version = "1.0.2";
    src =
      fetchFromGitHub
        {
          owner = "ShadowBlip";
          repo = "Neteria";
          rev = "1a8c976eb2beeca0a5a272a34ac58b2c114495a4";
          sha256 = "1c2fa0d4k2n3b88ac8awajqnfbar2y77zhsxa3wg0hix8lgkmgz3";
        };
    propagatedBuildInputs = [ rsa ];
    meta =
      with lib;
      {
        homepage = "https://github.com/ShadowBlip/Neteria";
        description = "Open source game networking framework for Python. ";
        license = licenses.gpl2;
        maintainers = with maintainers; [ ];
      };
  }
