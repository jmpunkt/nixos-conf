{ lib
, rustPlatform
, fetchFromGitLab
}:
rustPlatform.buildRustPackage
  rec {
    pname = "conduit";
    version = "0.3.0";
    src =
      fetchFromGitLab
        {
          inherit version;
          owner = "famedly";
          repo = "conduit";
          rev = "tags/v${ version }";
          sha256 = "1qn1fj60i8nn2ahgj2zp5ixd79bb0wl1ld36x3igws2f3c0f5pfi";
        };
    cargoSha256 = "sha256-s1762Ss31sA5qALHzwkvwbfRXo00cCtqzQyoz3/zf6I=";
    meta =
      with lib;
      {
        description = "A Matrix homeserver written in Rust";
        homepage = "https://gitlab.com/famedly/conduit/";
        license = with licenses; [ asl20 ];
        maintainers = with maintainers; [ ];
      };
  }
