cd ~/naraeon-ssd/ezerase/liveboot/
mkdir -p ~/naraeon-ssd/ezerase/liveboot/image/{live,isolinux}
sudo cp grubconfig/grub.cfg image/boot/grub.cfg
sudo cp chroot_nvidia/boot/vmlinuz-*-amd64 image/live/vmlinuz1 && \
sudo cp chroot_nvidia/boot/initrd.img-*-amd64 image/live/initrd1
sudo cp /usr/lib/ISOLINUX/isolinux.bin image/isolinux/ && \
sudo cp /usr/lib/ISOLINUX/isohdpfx.bin image/isolinux/ && \
sudo cp ~/naraeon-ssd/ezerase/liveboot/config/isolinux.cfg image/isolinux && \
sudo cp /usr/lib/syslinux/modules/bios/*.c32 image/isolinux

