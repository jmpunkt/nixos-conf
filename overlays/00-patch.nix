self: super:
# contains patches for machine configuration, normally the overlay is
# not shared with the machine configuration.
{
  # pipewire =
  #   let
  #     lib = super.lib;
  #   in
  #     super.pipewire.overrideAttrs (
  #       old: rec {
  #         version = "0.3.29";
  #         src = super.fetchFromGitLab {
  #           domain = "gitlab.freedesktop.org";
  #           owner = "pipewire";
  #           repo = "pipewire";
  #           rev = version;
  #           sha256 = "sha256-8yHvtfDh0srSwg8euyP7HC5/Burcn/O5Yg53qCzrUpo=";
  #         };
  #         patches = lib.take 4 old.patches;
  #       }
  #     );
}
