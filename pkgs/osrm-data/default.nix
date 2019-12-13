{
  stdenv,
  fetchurl,
  osrm-backend,
  country ? "germany",
  profile ? "${osrm-backend}/share/osrm/profiles/car.lua",
}:

stdenv.mkDerivation rec {
  pname = "osrm-data-${country}";
  version = "191121";

  baseName = "${country}-${version}";
  localSrc = builtins.baseNameOf src;
  srcName = "${baseName}.osm.pbf";

  # src = fetchurl {
  #   url = "https://download.geofabrik.de/europe/${baseName}.osm.pbf";
  #   sha256 = "240a987667e08b6755fbb3b21d715a5573bac1f300085b5fb1960561880a2952";
  # };

  src = ./germany-191121.osm.pbf;

  phases = [ "buildPhase" "installPhase" ];

  buildPhase = ''
    # Required because OSRM puts the output files in the same folder as the
    # source. Chaning the path ensures that the program puts it into the build
    # directory
    ln -s $src ${localSrc}
    ${osrm-backend}/bin/osrm-extract ${localSrc} -p ${profile}
    rm ${localSrc}
    ${osrm-backend}/bin/osrm-partition ./${baseName}.osrm
    ${osrm-backend}/bin/osrm-customize ./${baseName}.osrm
  '';

  installPhase = ''
    install -d $out/share/data
    install -D *osrm* $out/share/data
  '';
}

