self: super:

{
  nodePackages = super.nodePackages // ( super.callPackage ./plugins.nix {} );
}
