cd ~/naraeon-ssd/ezerase/liveboot
sudo cp -r /mnt/windows/bg.jpg chroot/root/bg.jpg
sudo cp -r /mnt/windows/bg.jpg chroot_nvidia/root/bg.jpg
sudo mkdir chroot/etc/ezerase
sudo mkdir chroot_nvidia/etc/ezerase
sudo cp -rf /mnt/windows/source/*.py chroot/etc/ezerase/
sudo cp -rf /mnt/windows/source/view/ko/*.py chroot/etc/ezerase/
sudo cp -rf /mnt/windows/source/*.py chroot_nvidia/etc/ezerase/
sudo cp -rf /mnt/windows/source/view/ko/*.py chroot_nvidia/etc/ezerase/

