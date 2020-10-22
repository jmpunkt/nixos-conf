
# Usage

## NixOS

Link the home-manager profile.
```bash
ln -s <PATH_TO_GIT_REPO> /etc/nixos/nixos-conf
```

Building the system using the provided flake.

```bash
nixos-rebuild switch --flake <PATH_TO_GIT_REPO>#
```

## Home-Manager

Link the home-manager profile.
```bash
ln -s <PATH_TO_GIT_REPO>/$USER $HOME/.config/nixpkgs
```

Update programs and configurations.
```bash
home-manger switch
```
