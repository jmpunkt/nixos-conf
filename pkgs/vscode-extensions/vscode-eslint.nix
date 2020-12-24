{ lib, vscode-utils, nodejs, fetchFromGitHub }:

vscode-utils.buildVscodeMarketplaceExtension {
  mktplcRef = {
    name = "vscode-eslint";
    publisher = "dbaeumer";
    version = "2.1.8";
    sha256 = "SNAcFeD0ZIr6FTUC2CPdaVqRA0TcYcfK24tT7wUL3KM=";
  };

  meta = {
    license = lib.licenses.mit;
  };
}
