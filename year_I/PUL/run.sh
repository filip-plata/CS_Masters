#!/usr/bin/env bash

qemu-system-x86_64 -enable-kvm -drive file=pul2018_cow.qcow2,if=virtio -usb -net nic,model=virtio -net user -m 1G -fsdev local,id=pul2018-share,path=pul2018-share/,security_model=none -device virtio-9p-pci,fsdev=pul2018-share,mount_tag=pul2018-share -display sdl
