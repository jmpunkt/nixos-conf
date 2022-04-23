{stdenv}:
stdenv.mkDerivation
rec {
  name = "rkhunter";
  version = "1.4.6";
  src =
    fetchurlfetchFromGitHub
    {
      url = "https://sourceforge.net/projects/rkhunter/files/rkhunter/1.4.6/rkhunter-1.4.6.tar.gz/download";
      sha256 = "115h6q8157cq1nqxs0zyl8y7awcqqq5snwrrgyphvrqzknp5mk09";
    };
}
