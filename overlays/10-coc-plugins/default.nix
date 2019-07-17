super: self:
{
  coc-plugins = super.callPackage ./plugins.nix {};
}
