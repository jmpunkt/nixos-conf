{ python3Packages }:

python3Packages.buildPythonPackage rec {
  pname = "pyscroll";
  version = "2.19.2";

  propagatedBuildInputs = with python3Packages; [ six pygame ];

  src = python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "175d39rymnx3v3mcfjjkr1wv76nsl1s00z04nzsspklyk0ih2783";
  };
}
