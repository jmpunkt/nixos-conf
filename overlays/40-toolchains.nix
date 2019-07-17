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
    mccabe
    mypy
    pylama
    black
    isort
    pycodestyle
    pyflakes
    yapf
    jedi

    numpy
    scipy
    pandas
  ]));
}
