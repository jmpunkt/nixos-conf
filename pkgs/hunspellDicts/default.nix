{ stdenv, lib, fetchurl, fetchFromGitHub, unzip, coreutils, bash, which, zip, ispell, perl, hunspell }:
let
  mkDict =
    { name, readmeFile, dictFileName, ... }@args:
    stdenv.mkDerivation ({
      inherit name;
      installPhase = ''
        # hunspell dicts
        install -dm755 "$out/share/hunspell"
        install -m644 ${dictFileName}.dic "$out/share/hunspell/"
        install -m644 ${dictFileName}.aff "$out/share/hunspell/"
        # myspell dicts symlinks
        install -dm755 "$out/share/myspell/dicts"
        ln -sv "$out/share/hunspell/${dictFileName}.dic" "$out/share/myspell/dicts/"
        ln -sv "$out/share/hunspell/${dictFileName}.aff" "$out/share/myspell/dicts/"
        # docs
        install -dm755 "$out/share/doc"
        install -m644 ${readmeFile} $out/share/doc/${name}.txt
        runHook postInstall
      '';
    } // args);
in
{
  de-de = mkDict rec {
    name = "hunspell-dict-de-de-frami-${version}";
    version = "2017-01-12";

    src = fetchurl {
      url = "https://extensions.libreoffice.org/assets/downloads/z/dict-de-de-frami-${version}.oxt";
      sha256 = "r1FQFeMGxjQ3O1OCgIo5aRIA3jQ5gR0vFQLpuRwjtGo=";
    };

    dictFileName = "de_DE_frami";
    readmeFile = "${dictFileName}_README.txt";
    nativeBuildInputs = [ unzip ];
    unpackCmd = ''
      unzip $src ${dictFileName}/{${dictFileName}.dic,${dictFileName}.aff,${readmeFile}}
    '';

    meta = with lib; {
      description = "Hunspell dictionary for German (Germany) from LibreOffice";
      homepage = "https://extensions.libreoffice.org/extensions/german-de-de-frami-dictionaries";
      license = licenses.lgpl3Only;
      platforms = platforms.all;
    };
  };
}
