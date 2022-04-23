{
  stdenv,
  lib,
  fetchFromGitHub,
  jdk,
  jre,
  maven,
  makeWrapper,
}:
#  <interactiveMode>true</interactiveMode>
#  <offline>false</offline>
let
  fetchDepsFor = module: ''
    while mvn package -DskipTests -Dmaven.test.skip=true -pl ${module} -am -Dmaven.repo.local=$out/.m2 -Dmaven.wagon.rto=5000; [ $? = 1 ]; do
      echo "timeout, restart maven to continue downloading"
    done
  '';
  installModule = module: ''
    buildJar=$(find ./${module}/target/ -name "${module}*.jar" | head -n 1 | tr -d '\n')

    cp "$buildJar" $out/share/java/${module}.jar

    makeWrapper ${jre}/bin/java $out/bin/${module} \
          --add-flags "-jar $out/share/java/${module}.jar"
  '';
  # 'maven.repo.local' must be writable so copy it out of nix store
  packageModule = module: skipTests: ''
    mvn package ${lib.optionalString skipTests "-DskipTests -Dmaven.test.skip=true"} -pl ${module} -am --offline -Dmaven.repo.local=$NIX_MAVEN_REPO
  '';
  fetchDeps = {
    name,
    version,
    src,
    modules,
    sha256,
  }:
    stdenv.mkDerivation
    {
      name = "${name}-${version}-deps";
      inherit src;
      buildInputs = [jdk maven];
      buildPhase = builtins.concatStringsSep "\n" (map fetchDepsFor modules);
      # keep only *.{pom,jar,sha1,nbm} and delete all ephemeral files with lastModified timestamps inside
      installPhase = ''find $out/.m2 -type f -regex '.+\(\.lastUpdated\|resolver-status\.properties\|_remote\.repositories\)' -delete'';
      outputHashAlgo = "sha256";
      outputHashMode = "recursive";
      outputHash = sha256;
    };
  buildMavenModules = {
    name,
    version,
    src,
    modules,
    depsSha256,
    meta,
    skipTests ? false,
  }:
    stdenv.mkDerivation
    {
      inherit name version src meta skipTests;
      nativeBuildInputs = [makeWrapper jdk maven];
      buildPhase = let
        deps =
          fetchDeps
          {
            inherit name version src modules;
            sha256 = depsSha256;
          };
      in
        ''
          export NIX_MAVEN_REPO=$(cp -dpR ${deps}/.m2 ./ && chmod +w -R .m2 && pwd)/.m2
        ''
        + builtins.concatStringsSep "\n" (map (mod: packageModule mod skipTests) modules);
      installPhase =
        ''
          mkdir -p $out/bin
          mkdir -p $out/share/java
        ''
        + builtins.concatStringsSep "\n" (map installModule modules);
    };
in
  buildMavenModules
  rec {
    name = "veraPDF-apps";
    version = "1.18.5";
    src =
      fetchFromGitHub
      {
        owner = "verapdf";
        repo = name;
        rev = "v${version}";
        sha256 = "7opEzyx0wIrRDA68Ma1RPzyotf/l3rAt7G7sQbMhRKQ=";
      };
    skipTests = true;
    modules = ["pdfbox-apps" "greenfield-apps"];
    # NOTICE: SHA changed without changing the version of verapdf
    depsSha256 = "jDG3/8Q0qWzovW0c3+iS3hz1BkuAvs2wKA/Tz1rJq2g=";
    meta = with lib; {
      description = "Greenfield PDF/A validation, feature reporting and metadata repair developed for veraPDF";
      homepage = "https://github.com/veraPDF/veraPDF-validation";
      license = with licenses; [gpl3Plus mpl20];
    };
  }
