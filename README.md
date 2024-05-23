# Usage

## System

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

### Manual

```bash
sh build.sh
doas nix-env -p /nix/var/nix/profiles/system --set ./result
doas result/bin/switch-to-configuration boot
```

### After BIOS Update

After an BIOS update, the Linux Bootmanager might be deleted. In order
to restore it, follow the instructions.

Boot into the UEFI version of the LiveCD, otherwise it will not work.

```bash
sudo mount /dev/.. /mnt
sudo mount /dev/.. /mnt/boot/efi
sudo nixos-enter
bootctl install
exit
sudo umount /mnt/boot/efi
sudo umount /mnt
reboot
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

## Keyboard

```bash
sh qmk/flash.sh
```

Enter password for doas. Then enter flash mode on keyboard.
