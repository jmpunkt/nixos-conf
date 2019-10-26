{ stdenv, fetchzip, fetchgit, vimUtils, fetchFromGitHub, nodePackages, ... }:

{
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
}
