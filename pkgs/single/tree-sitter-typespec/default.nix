{ fetchFromGitHub, tree-sitter, lib }:
tree-sitter.buildGrammar {
  language = "tree-sitter-typespec";
  version = "814c98283fd92a248ba9d49ebfe61bc672a35875";
  src = fetchFromGitHub {
    owner = "happenslol";
    repo = "tree-sitter-typespec";
    rev = "814c98283fd92a248ba9d49ebfe61bc672a35875";
    sha256 = "sha256-3/zNoawx1DsKmG0KFvJD+o80IMBsJd2VV2ng+fSrV1c=";
  };
}
