{ python3Packages }:

python3Packages.buildPythonPackage rec {
  pname = "PyTMX";
  version = "3.21.7";

  propagatedBuildInputs = with python3Packages; [ six pygame pyglet pysdl2 ];

  src = python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "19wgjdhkzzi2kpnba6xpr29b24dqy3nhdvcfardfm738mzxc3sgn";
  };
}
