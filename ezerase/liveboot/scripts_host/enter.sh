cd ~/naraeon-ssd/ezerase/liveboot
sudo mount -o bind /dev chroot/dev && sudo cp /etc/resolv.conf chroot/etc/resolv.conf
sudo chroot chroot

