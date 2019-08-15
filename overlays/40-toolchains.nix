self: super:

{
  rustToolchain = super.latest.rustChannels.stable.rust.override {
    targets = [
      "x86_64-unknown-linux-gnu"
      "x86_64-unknown-linux-musl"
      "wasm32-unknown-unknown"
    ];
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
    pyls-mypy
    jedi
    epc

    mccabe
    mypy
    pylama
    black
    isort
    pycodestyle
    pyflakes
    yapf

    numpy
    scipy
    pandas
  ]));
}
