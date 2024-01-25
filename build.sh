#! /usr/bin/env bash

set -e
hostname=$(hostname)

echo "Building configuration for $hostname"

nix build ".#$hostname"

nix run self#nvd diff /nix/var/nix/profiles/system ./result/
