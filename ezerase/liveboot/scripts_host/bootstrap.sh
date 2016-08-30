cd ~/naraeon-ssd/ezerase/liveboot
rm -rf chroot
sudo apt-get install --yes debootstrap debian-archive-keyring syslinux isolinux xorriso
sudo apt-get dist-upgrade
debootstrap --arch=amd64 --variant=minbase stretch chroot http://ftp.kr.debian.org/debian/
mkdir ~/naraeon-ssd/ezerase/liveboot/chroot/scripts
sudo cp ~/naraeon-ssd/ezerase/liveboot/scripts_chroot/*.sh ~/naraeon-ssd/ezerase/liveboot/chroot/scripts/