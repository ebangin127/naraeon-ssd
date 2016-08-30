cd ~/naraeon-ssd/ezerase/liveboot/
sudo cp /mnt/windows/dependencies/* ~/naraeon-ssd/ezerase/liveboot/config/
sudo cp /mnt/windows/dependencies/.bashrc ~/naraeon-ssd/ezerase/liveboot/config/
sudo cp /mnt/windows/dependencies/.blackboxrc ~/naraeon-ssd/ezerase/liveboot/config/
sudo cp /mnt/windows/dependencies/.bsetbgrc ~/naraeon-ssd/ezerase/liveboot/config/
cd ~/naraeon-ssd/ezerase/liveboot/config
cp ./slim.conf ../chroot/etc/
cp ./blackbox-menu ../chroot/etc/X11/blackbox/blackbox-menu
cp ./Gray ../chroot/usr/share/blackbox/styles/Gray
cp ./.bashrc ../chroot/root/
cp ./.blackboxrc ../chroot/root/
cp ./.bsetbgrc ../chroot/root/
cd ..

