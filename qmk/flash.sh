#!/usr/bin/env bash

set -e

echo "Building keyboard"
drv=$(nix build .#keyboard --print-out-paths --no-link)
usb_device="/dev/disk/by-label/RPI-RP2"

flash_device() {
    set -e
    src="$1"
    dst="$2"

    while [ ! -e "$dst" ]; do
        echo "Waiting for USB ($dst) device to connect..."
        sleep 1
    done

    echo "USB device detected. Flashing ($src) to $dst ..."
    dd if="$src" of="$dst"
}

echo "Building done"
doas bash -c "$(declare -f flash_device); flash_device $drv/image.uf2 $usb_device"
