{ stdenv, fetchzip, fetchgit, vimUtils, ... }:

{
  coc-nvim = vimUtils.buildVimPluginFrom2Nix rec {
    pname = "coc-nvim";
    version = "0.0.71";

    index_js = fetchzip {
      url = "https://github.com/neoclide/coc.nvim/releases/download/v${version}/coc.tar.gz";
      sha256 = "1bhkyrmrpriizg3f76x4vp94f2bfwcf7a6cp3jvv7vj4zaqhsjzz";
    };

    src = fetchgit {
      url = "https://github.com/neoclide/coc.nvim";
      rev = "fa429d0bbcc0eca6ff85fc9a4d55e43406beec19";
      sha256 = "1alr0arvsxarkyyfvm22bizpndvwmc8cz33i19alzd05mgym8mzj";
    };

    postInstall = ''
        mkdir -p $out/share/vim-plugins/coc-nvim/build
        cp ${index_js}/index.js $out/share/vim-plugins/coc-nvim/build/
    '';
  };

  lexima = vimUtils.buildVimPlugin {
    name = "lexima-git-2019-02-26";
    buildPhase = "true";
    src = fetchgit {
      url = "https://github.com/cohama/lexima.vim";
      rev = "54e647e4dd320e69a0972507e961651e327e0423";
      sha256 = "0sr15pxzw8ldfsw3h381rvnh43m47rcqpjw8al2akjhmxqlnzjrq";
    };
  };

  vim-searchindex = vimUtils.buildVimPlugin {
    name = "vim-searchindex-git-2018-12-05";
    src = fetchgit {
      url = "https://github.com/google/vim-searchindex";
      rev = "576a7018081b38f621de050d4d60d59515865a75";
      sha256 = "0xikwb1i6d7ailygcshjpp2h5pi24fmgs5sfmdgq5cvasvlj1xfv";
    };
  };

  quick-scope = vimUtils.buildVimPlugin {
    name = "quick-scope-2.5.0";
    src = fetchgit {
      url = "https://github.com/unblevable/quick-scope";
      rev = "10029708ee50d300d4b5e3475610210d4b29c74d";
      sha256 = "1nlrj5n0lzqy267rvza3ky5yf8plad5fpb1r8dqgq5s3k4l448mg";
    };
  };

  defx-nvim = vimUtils.buildVimPluginFrom2Nix {
    name = "defx-nvim-git-2019-03-02";
    src = fetchgit {
      url = "https://github.com/shougo/defx.nvim";
      rev = "5e13e29cd69f67a3f4474822b890ef9c479119f8";
      sha256 = "17gfj5kagr24kr85mna37pir69wd4542935l872581xhy3kiga1w";
    };
  };

  defx-git = vimUtils.buildVimPluginFrom2Nix {
    name = "defx-git-git-2019-03-02";
    src = fetchgit {
      url = "https://github.com/kristijanhusak/defx-git";
      rev = "bb1ec337838870b1b966826ad24c109073d2a9ac";
      sha256 = "1xg17bvqqad1723ps6h4pwfnkxffkk3y6nz0qx9d00ryxcc312x1";
    };
  };
}
