#! /usr/bin/env bash

set -e
hostname=$(hostname)

current=$(cat /nix/var/nix/profiles/system/nixos-version)

echo "Building configuration for $hostname"

nix --experimental-features 'nix-command flakes' build ".#$hostname"

built=$(cat ./result/nixos-version)

echo "diffing system derivations for $hostname ($current -> $built)"
nix --experimental-features 'nix-command flakes' run nixpkgs#nvd diff /nix/var/nix/profiles/system ./result/
