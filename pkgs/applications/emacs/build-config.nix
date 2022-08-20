{
  lib,
  callPackage,
  emacs,
  fetchFromGitHub,
  emacsPackagesFor,
}: {
  name,
  initFile,
  emacsBuilder ? (emacsPackagesFor emacs),
  extraVariables ? {},
  extraPaths ? [],
  skipNixCheck ? false,
  lookupOrder ? ["melpaPackages" "melpaPackages" "melpaPackages"],
  packageIdentifiers ? ["use-package" "require"],
}: let
  inherit (builtins) stringLength filter substring split readFile typeOf add length head tail;
  initBuilder = callPackage ./build-init.nix {};
  pathBuilder = callPackage ./build-nixos-paths.nix {};
  hasNoNixOsPaths =
    isEmptyAttrSet extraVariables
    && isEmptyAttrSet extraPaths;

  file = readFile initFile;
  isEmptyAttrSet = set:
    length (builtins.attrNames set) == 0;
  # Create a list of every second element in the original list.
  evenOnly = list:
    if length list >= 2
    then let
      new = tail list;
    in
      [(head new)]
      ++ (evenOnly (tail new))
    else [];
  findDeclarations = file: ident: let
    prefix = "(${ident}";
    elements = split "\\(${ident} '?([a-zA-Z-]+)" file;
    x = length elements;
  in
    map
    head (evenOnly elements);
  allPackages =
    lib.unique
    (builtins.fold'
      (a: b: a ++ b)
      []
      (map (findDeclarations file) packageIdentifiers));
  resolvePackageSingle = lookup: pkg: let
    item = lib.findFirst (builtins.hasAttr pkg) null lookup;
  in
    if builtins.isNull item
    then throw "emacs package `${x}` was not found in any of ${builtins.toJSON lookupOrder}"
    else item.${pkg};
  resolvedPackagesFn = epkgs: let
    lookup = map (x: epkgs.${x}) lookupOrder;
    modulePaths = lib.optional (!hasNoNixOsPaths) [
      (pathBuilder
        {
          variables = extraVariables;
          paths = extraPaths;
        })
    ];
  in
    (map
      (resolvePackageSingle lookup)
      (filter (x: x != "nixos-paths") allPackages))
    ++ modulePaths;
  initModule = initBuilder {
    inherit name initFile emacsBuilder;
    emacsPackageFn = resolvedPackagesFn;
  };
in
  assert hasNoNixOsPaths
  || (builtins.any (x: x == "nixos-paths"));
    emacsBuilder.emacsWithPackages (
      epkgs: (resolvedPackagesFn epkgs) ++ [initModule]
    )
