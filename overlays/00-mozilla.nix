self: super:

{
  mozOverlay = super.fetchFromGitHub {
    owner  = "mozilla";
    repo   = "nixpkgs-mozilla";
    rev    = "d46240e8755d91bc36c0c38621af72bf5c489e13";
    sha256 = "0icws1cbdscic8s8lx292chvh3fkkbjp571j89lmmha7vl2n71jg";
  };
}

