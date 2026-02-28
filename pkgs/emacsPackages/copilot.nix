{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  dash,
  editorconfig,
  s,
  f,
  jsonrpc,
}:
let
  rev = "3b086696fb1bb97518b7b30ce1b184ebb8c085b1";
  pname = "copilot";
  owner = "copilot-emacs";
in
melpaBuild {
  inherit pname;
  version = "20260226.0";

  commit = rev;
  packageRequires = [
    dash
    editorconfig
    s
    f
    jsonrpc
  ];

  src = fetchFromGitHub {
    inherit rev owner;
    repo = "${pname}.el";
    sha256 = "sha256-i2s92BUcuiUlolbjL0qBvNxEs/3cRTFHm+wvSfferbc=";
  };

  recipe = writeText "recipe" ''
    (copilot :repo "${owner}/${pname}" :fetcher github)
  '';

  meta = {
    description = "An unofficial Copilot plugin for Emacs.";
  };
}
