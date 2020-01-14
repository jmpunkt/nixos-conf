{ config, ... }:


let
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };
in
{
  home.packages = with unstable.pkgs; [
    (texlive.combine {
      inherit (texlive)
        scheme-tetex
        adjustbox
        ucs
        collectbox
        collection-fontsrecommended
        pagecolor
        koma-script
        csquotes
        mdframed
        needspace
        sourcesanspro
        ly1
        mweights
        sourcecodepro
        titling
        lm
        listings
        float
        xcolor
        setspace
        etoolbox
        caption
        l3packages
        l3kernel
        hyperref
        capt-of
        wrapfig
        tabu
        footmisc
        beamertheme-metropolis
        xkeyval
        minted
        fvextra
        ifplatform
        xstring
        framed
        upquote
        dvipng
        svg
        trimspaces
        relsize
        pdfpages
        refman
        cleveref
        semantic
        todonotes
        tuda-ci anyfontsize urcls roboto xcharter pdfx xmpincl
        latexmk;
    })
  ];
}
