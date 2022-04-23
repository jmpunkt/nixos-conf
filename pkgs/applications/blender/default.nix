{
  stdenv,
  makeWrapper,
  blender,
  pythonEnv,
}:
stdenv.mkDerivation
{
  name = "blender-wrapped";
  inherit (blender) version;
  phases = ["installPhase"];
  nativeBuildInputs = [makeWrapper];
  installPhase = ''
    makeWrapper ${blender}/bin/blender $out/bin/blender \
      --prefix PYTHONPATH : ${pythonEnv}/${pythonEnv.sitePackages} \
      --add-flags '--python-use-system-env'


    ln -s ${blender}/share $out/share
  '';
}
