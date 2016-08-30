cd ~/naraeon-ssd/ezerase/liveboot
sudo rm -rf chroot
sudo apt-get update
sudo apt-get install --yes debootstrap debian-archive-keyring syslinux isolinux xorriso
sudo apt-get dist-upgrade
sudo debootstrap --arch=amd64 --variant=minbase jessie chroot http://ftp.kr.debian.org/debian/
sudo mkdir ~/naraeon-ssd/ezerase/liveboot/chroot/scripts
sudo cp ~/naraeon-ssd/ezerase/liveboot/scripts_chroot/*.sh ~/naraeon-ssd/ezerase/liveboot/chroot/scripts/