{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  pkgconfig,
  flex,
}:
stdenv.mkDerivation
{
  pname = "nyan-project";
  version = "2020-05-17";
  src =
    fetchFromGitHub
    {
      owner = "SFTtech";
      repo = "nyan";
      rev = "2d6fba801a34e67677ce212c8af7c0f991d2bbcd";
      sha256 = "1jg7n0g5abzr4glq2xkskik2hakdzafylpb62z27mf4qbg01lqp4";
    };
  nativeBuildInputs = [cmake pkgconfig];
  buildInputs = [flex];
  meta = with lib; {
    description = "Modding API with a typesafe hierarchical key-value database with inheritance and dynamic patching";
    homepage = "https://github.com/SFTtech/nyan/";
    license = licenses.lgpl3Plus;
    platforms = with platforms; unix;
    maintainers = with maintainers; [];
  };
}
