{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
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
        latexmk;
    })
  ];
}
