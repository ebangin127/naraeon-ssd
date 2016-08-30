cd ~/live_boot/
sudo cp /mnt/windows/dependencies/* ~/live_boot/config/
sudo cp /mnt/windows/dependencies/.bashrc ~/live_boot/config/
sudo cp /mnt/windows/dependencies/.blackboxrc ~/live_boot/config/
sudo cp /mnt/windows/dependencies/.bsetbgrc ~/live_boot/config/
cd ~/live_boot/config
sudo cp ./slim.conf ../chroot/etc/
sudo cp ./blackbox-menu ../chroot/etc/X11/blackbox/blackbox-menu
sudo cp ./Gray ../chroot/usr/share/blackbox/styles/Gray
sudo cp ./.bashrc ../chroot/root/
sudo cp ./.blackboxrc ../chroot/root/
sudo cp ./.bsetbgrc ../chroot/root/
cd ..

