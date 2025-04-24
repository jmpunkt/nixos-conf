{...}: {
  home.file = {
    ".ssh/id_rsa.pub".text = builtins.readFile ./ssh.pub;
  };

  programs.git = {
    enable = true;
    userEmail = "jmpunkt@outlook.com";
    signing = {
      key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
      signByDefault = true;
    };
  };

  programs.ssh = {
    matchBlocks = {
      "*" = {
        identityFile = "~/.ssh/id_rsa.pub";
      };
    };
  };
}
