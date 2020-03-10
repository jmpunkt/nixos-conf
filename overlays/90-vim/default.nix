self: super:

{
  vimPlugins = super.vimPlugins // (super.callPackage ./vim-plugins.nix { });
}
