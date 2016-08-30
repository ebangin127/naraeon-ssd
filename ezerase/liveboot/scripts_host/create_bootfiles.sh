cd ~/live_boot/
mkdir -p ~/live_boot/image/{live,isolinux}
sudo cp grubconfig/grub.cfg image/boot/grub.cfg
sudo cp chroot/boot/vmlinuz-*-amd64 image/live/vmlinuz1 && \
sudo cp chroot/boot/initrd.img-*-amd64 image/live/initrd1
sudo cp /usr/lib/ISOLINUX/isolinux.bin image/isolinux/ && \
sudo cp /usr/lib/ISOLINUX/isohdpfx.bin image/isolinux/ && \
sudo cp ~/isolinux.cfg image/isolinux && \
sudo cp /usr/lib/syslinux/modules/bios/*.c32 image/isolinux

