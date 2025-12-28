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
  rev = "6a2ad80489b8a0d021df95293eb7ac370aea140b";
  pname = "copilot";
  owner = "copilot-emacs";
in
melpaBuild {
  inherit pname;
  version = "20250404.0";

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
    sha256 = "sha256-aiVRefuH4jpAtTgMMDa7jI0Yy08J4+fV59f8nd7vT7g=";
  };

  recipe = writeText "recipe" ''
    (copilot :repo "${owner}/${pname}" :fetcher github)
  '';

  meta = {
    description = "An unofficial Copilot plugin for Emacs.";
  };
}
