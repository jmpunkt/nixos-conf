{
  writeText,
  emacsPackagesFor,
}: emacs: let
  tomlHighlights =
    writeText "toml-highlights.scm"
    ''
      (bare_key) @property
      (quoted_key) @string

      (boolean) @keyword
      (comment) @comment
      (string) @string
      (integer) @number
      (float) @number
      (offset_date_time) @string.special
      (local_date_time) @string.special
      (local_date) @string.special
      (local_time) @string.special

      "." @punctuation.delimiter
      "," @punctuation.delimiter

      "=" @operator

      "[" @punctuation.bracket
      "]" @punctuation.bracket
      "[[" @punctuation.bracket
      "]]" @punctuation.bracket
      "{" @punctuation.bracket
      "}" @punctuation.bracket
    '';
in ((emacsPackagesFor emacs).overrideScope' (eself: esuper: {
  tree-sitter-langs = esuper.tree-sitter-langs.overrideAttrs (old: rec {
    postInstall =
      (old.postInstall or "")
      + ''
        mkdir $out/share/emacs/site-lisp/elpa/${old.pname}-${old.version}/queries/toml
        ln -s ${tomlHighlights} $out/share/emacs/site-lisp/elpa/${old.pname}-${old.version}/queries/toml/highlights.scm
      '';
  });
}))
