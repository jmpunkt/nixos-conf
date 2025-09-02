{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
  magit,
}:
let
  rev = "bb99a478966902cfa6fd158457218c19f889dd6a";
  ename = "jj-mode";
  pname = "jj-mode.el";
  owner = "bolivier";
in
melpaBuild {
  inherit pname ename;

  version = "20250831.0";

  commit = rev;

  packageRequires = [
    magit
  ];

  src = fetchFromGitHub {
    inherit rev owner;
    repo = pname;
    sha256 = "sha256-dQzVygQQAQONMVrMwfSJbjQUX86flRgpWGhd8Ow6tVk=";
  };

  recipe = writeText "recipe" ''
    (${ename} :repo "${owner}/${pname}" :fetcher github)
  '';

  meta = with lib; {
    description = "Jujutsu version control mode for Emacs inspired by Magit";
    license = licenses.mit;
  };
}
