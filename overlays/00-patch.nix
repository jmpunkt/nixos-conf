self: super:

# contains patches for machine configuration, normally the overlay is
# not shared with the machine configuration.
{
  # NOTICE: disabled until discord breaks again
  discord = super.discord.overrideAttrs (old: rec {
    version = "0.0.15";
    src = super.fetchurl {
      url =
        "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      sha256 = "re3pVOnGltluJUdZtTlSeiSrHULw1UjFxDCdGj/Dwl4=";
    };
  });
}
