#! /usr/bin/env bash

set -e
hostname=$(hostname)

echo "Building configuration for $hostname"

nix --experimental-features 'nix-command flakes' build ".#$hostname"

nix --experimental-features 'nix-command flakes' run self#nvd diff /nix/var/nix/profiles/system ./result/
