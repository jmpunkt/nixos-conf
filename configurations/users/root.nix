{
  lib,
  config,
  pkgs,
  ...
}:
{
  users.users.root = {
    openssh.authorizedKeys.keys = [
      (builtins.readFile ./../../home/jonas/profiles/yubikey/ssh.pub)
    ];
  };
}
