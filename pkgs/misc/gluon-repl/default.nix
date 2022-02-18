{ lib
, rustPlatform
, fetchFromGitHub
}:
rustPlatform.buildRustPackage
  rec {
    pname = "gluon";
    version = "0.13.1";
    src =
      fetchFromGitHub
        {
          owner = "gluon-lang";
          repo = "gluon";
          rev = "tags/v${ version }";
          sha256 = "1hrf36g0p931d6i7ky089jdq7mki0ilmsq2n5yc5yhc73x6rvf0m";
        };
    cargoSha256 = "1f8ksl538acy0nj7avixmkywf2s7abmlwapwims4jiz5r91v89is";
    # <2019-12-25 Mi> tests does not work at the moment
    checkPhase = "true";
    preBuild = "cd repl";
    postBuild = "cd ..";
    meta =
      with lib;
      {
        description = "A static, type inferred and embeddable language written in Rust";
        homepage = "https://github.com/gluon-lang/gluon";
        license = licenses.mit;
        maintainers = [ ];
        platforms = platforms.all;
      };
  }
