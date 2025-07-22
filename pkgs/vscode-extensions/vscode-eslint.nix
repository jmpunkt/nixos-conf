{
  lib,
  vscode-utils,
  nodejs,
  fetchFromGitHub,
}:
vscode-utils.buildVscodeMarketplaceExtension {
  mktplcRef = {
    name = "vscode-eslint";
    publisher = "dbaeumer";
    version = "2.1.14";
    sha256 = "bVGmp871yu1Llr3uJ+CCosDsrxJtD4b1+CR+omMUfIQ=";
  };
  meta = {
    license = lib.licenses.mit;
  };
}
