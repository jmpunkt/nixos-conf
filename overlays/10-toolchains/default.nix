self: super:

{
  rustToolchain = super.callPackage ./rust.nix {};
  pythonToolchain = super.callPackage ./python.nix {};
}
