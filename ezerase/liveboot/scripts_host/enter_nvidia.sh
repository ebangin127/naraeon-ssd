cd ~/naraeon-ssd/ezerase/liveboot
sudo mount -o bind /dev chroot_nvidia/dev && sudo cp /etc/resolv.conf chroot_nvidia/etc/resolv.conf
sudo chroot chroot_nvidia
