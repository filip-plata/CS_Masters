DIR=$(dirname "$0")
QEMU="${DIR}/qemu/x86_64-softmmu/qemu-system-x86_64"
QEMU_SYS="qemu-system-x86_64"

${QEMU} -device harddoom2 \
	-device virtio-scsi-pci,id=scsi0 -drive file=zso2019.qcow2,if=none,id=drive0 -device scsi-hd,bus=scsi0.0,drive=drive0 \
	-enable-kvm -smp 2 -net nic,model=virtio -net user \
	-m 2G -fsdev  \
	local,id=hshare,path=hshare/,security_model=none \
	-device virtio-9p-pci,fsdev=hshare,mount_tag=hshare \
	-chardev stdio,id=cons,signal=off -device virtio-serial-pci -device virtconsole,chardev=cons \
        -usb -device usb-mouse
