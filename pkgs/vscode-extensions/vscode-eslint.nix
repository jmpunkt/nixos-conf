{ lib, vscode-utils, nodejs, fetchFromGitHub }:

vscode-utils.buildVscodeMarketplaceExtension {
  mktplcRef = {
    name = "vscode-eslint";
    publisher = "dbaeumer";
    version = "2.1.8";
    sha256 = "parXZhF9qyRAlmeGItCbvPfyyQQ9WmlBKKFYQ8KIFH0=";
  };

  meta = {
    license = lib.licenses.mit;
  };
}
