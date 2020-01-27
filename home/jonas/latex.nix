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
        etoolbox
        caption
        l3packages
        l3kernel
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
        tufte-latex changepage fancyhdr geometry hyperref natbib sauerj paralist placeins ragged2e setspace textcase titlesec xcolor xifthen microtype mathpazo soul bera hardwrap realscripts was fontaxes
        latexmk;
    })
  ];
}
