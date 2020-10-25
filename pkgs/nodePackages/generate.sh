#!/usr/bin/env bash
node2nix -i packages.json -o packages.nix -c composition.nix
