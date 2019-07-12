self: super:

{
  rustToolchain = super.latest.rustChannels.stable.rust.override {
    extensions = [
      "rust-src"
      "rls-preview"
      "clippy-preview"
      "rustfmt-preview"
    ];
  };

  pythonToolchain = (super.python37.withPackages(ps: with ps; [
    python-language-server
    pyls-black
    pyls-isort
    pyls-mypy
    numpy
    scipy
    pandas
    mccabe
  ]));
}
