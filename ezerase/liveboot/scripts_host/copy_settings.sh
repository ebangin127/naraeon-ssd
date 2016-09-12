cd ~/naraeon-ssd/ezerase/liveboot/
sudo cp /mnt/windows/dependencies/* ~/naraeon-ssd/ezerase/liveboot/config/
sudo cp /mnt/windows/dependencies/.bashrc ~/naraeon-ssd/ezerase/liveboot/config/
sudo cp /mnt/windows/dependencies/.blackboxrc ~/naraeon-ssd/ezerase/liveboot/config/
sudo cp /mnt/windows/dependencies/.bsetbgrc ~/naraeon-ssd/ezerase/liveboot/config/
sudo chown ebangin127 ~/naraeon-ssd/ezerase/liveboot/config/*
cd ~/naraeon-ssd/ezerase/liveboot/config
sudo cp ./slim.conf ../chroot/etc/
sudo cp ./blackbox-menu ../chroot/etc/X11/blackbox/blackbox-menu
sudo cp ./Gray ../chroot/usr/share/blackbox/styles/Gray
sudo cp ./.bashrc ../chroot/root/
sudo cp ./.blackboxrc ../chroot/root/
sudo cp ./.bsetbgrc ../chroot/root/
sudo cp ./slim.conf ../chroot_nvidia/etc/
sudo cp ./blackbox-menu ../chroot_nvidia/etc/X11/blackbox/blackbox-menu
sudo cp ./Gray ../chroot_nvidia/usr/share/blackbox/styles/Gray
sudo cp ./.bashrc ../chroot_nvidia/root/
sudo cp ./.blackboxrc ../chroot_nvidia/root/
sudo cp ./.bsetbgrc ../chroot_nvidia/root/
cd ..