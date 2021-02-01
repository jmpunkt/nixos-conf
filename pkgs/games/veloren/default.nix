{ lib, mozillaNightlyRustPlatform, fetchurl, pkg-config, git, git-lfs
, python3, alsaLib, atk, cairo, glib, gtk3, pango, server ? false, chat ? false,
}:

mozillaNightlyRustPlatform.buildRustPackage rec {
  pname = "veloren";
  version = "0.5.0";

  src = fetchurl {
    url =
      "https://gitlab.com/veloren/veloren/-/archive/v0.5.0/veloren-${version}.tar.gz";
    sha256 = "0jz4l3gj77l9b0p69ry9mw04b33vwxpbv8b417axxiffrh2xjp7k";
  };

  nativeBuildInputs = [ git git-lfs pkg-config python3 ];

  buildInputs = [ alsaLib atk cairo glib gtk3 pango ];

  cargoBuildFlags = (lib.optional (!server) "--bin veloren-voxygen")
    ++ (lib.optional (chat) "--bin veloren-chat-cli")
    ++ (lib.optional server "--bin veloren-server-cli");

  postInstall = ''
    cp -R assets $out/bin/assets
  '';

  cargoSha256 = "0yzmsr2iqdz6k4bagkfp0fs0k571cr1w03kxzxwqr2laqic42aw5";

  meta = with lib; {
    description =
      "Veloren is a multiplayer voxel RPG written in Rust. It is inspired by games such as Cube World, Legend of Zelda: Breath of the Wild, Dwarf Fortress and Minecraft.";
    homepage = "https://gitlab.com/veloren/veloren";
    license = [ licenses.gpl3 ];
    maintainers = [ ];
  };
}
