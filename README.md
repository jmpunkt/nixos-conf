# Usage

## NixOS + Home-Manager

### Local

Building the system using the provided flake.
```bash
nixos-rebuild switch --flake <PATH_TO_GIT_REPO>#
```

### Remote

```bash
nixos-rebuild --flake <PATH_TO_GIT_REPO>#mymachine \
  --target-host mymachine-hostname --build-host localhost \
  switch
```


## ISO

Build the ISO from the Git Repo.
```bash
nix build <PATH_TO_GIT_REPO>#iso
```

Build the ISO from the pinned path.
```bash
nix build self#iso
```

## Shell

Nix shell without overlay (pure Nixpkgs).
```bash
nix shell nixpkgs#<pkg>
```

Nix shell with overlay, including `jmpunkt` prefix.
```bash
nix shell self#<pkg>
```
