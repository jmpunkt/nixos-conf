{ vimUtils, fetchFromGitHub, ... }:

{
  vim-searchindex = vimUtils.buildVimPlugin {
    name = "vim-searchindex-git-2018-12-05";
    src = fetchFromGitHub {
      owner = "google";
      repo = "vim-searchindex";
      rev = "576a7018081b38f621de050d4d60d59515865a75";
      sha256 = "0xikwb1i6d7ailygcshjpp2h5pi24fmgs5sfmdgq5cvasvlj1xfv";
    };
  };
}
