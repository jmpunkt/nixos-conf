{ python37, ... }:

python37.withPackages(ps: with ps; [
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
])
