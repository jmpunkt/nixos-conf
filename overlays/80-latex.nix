self: super:

let
  # Create pygments Texlive package which
  pygments-tex.pkgs = [
    (super.python37Packages.pygments.overrideAttrs (oldAttrs: {
      pname = "${oldAttrs.pname}-tex";
      tlType = "bin";
    })

    )
  ];
  texlive = super.texlive;
in {
  jmpunkt = (super.jmpunkt or { }) // {
    latex = (texlive.combine {
      # add custom package
      inherit pygments-tex;
      # add packages provided by NixOS
      inherit (texlive)
        scheme-tetex ec adjustbox ucs collectbox collection-fontsrecommended
        pagecolor koma-script csquotes mdframed needspace sourcesanspro ly1
        mweights sourcecodepro titling lm listings float etoolbox caption
        l3packages l3kernel capt-of wrapfig tabu footmisc beamertheme-metropolis
        xkeyval minted fvextra ifplatform xstring framed upquote dvipng svg
        trimspaces relsize pdfpages refman cleveref semantic todonotes tuda-ci
        anyfontsize urcls roboto xcharter pdfx xmpincl tufte-latex changepage
        fancyhdr geometry hyperref natbib sauerj paralist placeins ragged2e
        setspace textcase titlesec xcolor xifthen microtype mathpazo soul bera
        hardwrap realscripts was fontaxes fancyvrb latexmk catchfile environ;
    });
  };
}
