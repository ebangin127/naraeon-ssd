cd ~/naraeon-ssd/
git pull
sudo cp ~/naraeon-ssd/ezerase/liveboot/scripts_chroot/*.sh ~/naraeon-ssd/ezerase/liveboot/chroot_init/scripts/
sudo rm -rf ~/naraeon-ssd/ezerase/liveboot/chroot/
sudo cp -r ~/naraeon-ssd/ezerase/liveboot/chroot_init/ ~/naraeon-ssd/ezerase/liveboot/chroot/