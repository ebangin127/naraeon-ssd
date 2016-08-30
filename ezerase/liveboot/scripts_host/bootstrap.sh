cd ~
mkdir live_boot
cd live_boot
sudo rm -rf chroot
sudo apt-get update
sudo apt-get install --yes debootstrap debian-archive-keyring syslinux isolinux xorriso
sudo apt-get dist-upgrade
sudo debootstrap --arch=amd64 --variant=minbase stretch chroot http://ftp.kr.debian.org/debian/
sudo mkdir ~/live_boot/chroot/scripts
sudo cp ~/live_boot/scripts_chroot/*.sh ~/live_boot/chroot/scripts/

