self: super:
# contains patches for machine configuration, normally the overlay is
# not shared with the machine configuration.
{
  # NOTICE: disabled until discord breaks again
  discord =
    super.discord.overrideAttrs
    (
      old: rec {
        version = "0.0.18";
        src =
          super.fetchurl
          {
            url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
            sha256 = "BBc4n6Q3xuBE13JS3gz/6EcwdOWW57NLp2saOlwOgMI=";
          };
      }
    );
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
