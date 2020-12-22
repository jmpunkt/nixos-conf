{ lib, vscode-utils, nodejs, fetchFromGitHub }:

let
  name = "vscode-eslint";
  publisher = "dbaeumer";
  # https://github.com/emacs-lsp/lsp-mode/issues/1932#issuecomment-714633460
  version = "2.1.8";
in
vscode-utils.buildVscodeExtension {
  name = "${publisher}-${name}-${version}";

  vscodeExtUniqueId = "${publisher}.${name}";

  src = fetchFromGitHub {
    owner = "microsoft";
    repo = "vscode-eslint";
    rev = "release/${version}";
    sha256 = "ceNiSXlyeglL9ML73iSTttOEy2rYyc3/lPGOhnL1ZoU=";
  };

  meta = {
    license = lib.licenses.mit;
  };
}
