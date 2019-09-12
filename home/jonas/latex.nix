{ pkgs, config, ... }:
let
  my_latex = with pkgs;
    texlive.combine {
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
        wrapfig
        xkeyval;

    };
in
{
  home.packages = [ my_latex ];
}
