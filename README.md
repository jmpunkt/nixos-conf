
# Bootstrap

## Clone repository

```bash
cd /etx/nixos
git clone https://github.com/jmpunkt/nixos-conf
```

## Setup NixOS

Create the file _/etc/nixos/configuration.nix_ with the following content and
then build the system.

```nix
{ config, pkgs, ... }:

{
  imports = [
    ./nixos-conf/machines/<MACHINE>/configuration.nix
  ];
}
```

Build the system
```bash
nixos-rebuild switch
```

## Setup Home-Manager

```bash
ln -s /etc/nixos/nixos-conf/home/$USER $HOME/.config/nixpkgs
home-manger switch
```
