cd ~/naraeon-ssd/ezerase/liveboot
sudo rm -rf chroot chroot_nvidia
sudo apt-get update
sudo apt-get install --yes debootstrap debian-archive-keyring syslinux isolinux xorriso
sudo apt-get dist-upgrade
sudo debootstrap --arch=amd64 --variant=minbase stretch chroot http://ftp.kr.debian.org/debian/
sudo debootstrap --arch=amd64 --variant=minbase stretch chroot_nvidia http://ftp.kr.debian.org/debian/
sudo mkdir ~/naraeon-ssd/ezerase/liveboot/chroot/scripts
sudo cp ~/naraeon-ssd/ezerase/liveboot/scripts_chroot/*.sh ~/naraeon-ssd/ezerase/liveboot/chroot/scripts/
sudo mkdir ~/naraeon-ssd/ezerase/liveboot/chroot/nvme-cli/
sudo cp -r ~/naraeon-ssd/ezerase/liveboot/nvme-cli/* ~/naraeon-ssd/ezerase/liveboot/chroot/nvme-cli/
sudo mkdir ~/naraeon-ssd/ezerase/liveboot/chroot/sedutil/
sudo cp -r ~/naraeon-ssd/ezerase/liveboot/sedutil/* ~/naraeon-ssd/ezerase/liveboot/chroot/sedutil/
sudo mkdir -p ~/naraeon-ssd/ezerase/liveboot/chroot/usr/lib/pm-utils/sleep.d/
sudo cp ~/naraeon-ssd/ezerase/liveboot/config/99_resume.sh ~/naraeon-ssd/ezerase/liveboot/chroot/usr/lib/pm-utils/sleep.d/
rm -rf ~/naraeon-ssd/ezerase/liveboot/chroot_init/
mkdir ~/naraeon-ssd/ezerase/liveboot/chroot_init/
cp -r ~/naraeon-ssd/ezerase/liveboot/chroot/* ~/naraeon-ssd/ezerase/liveboot/chroot_init/

sudo mkdir ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/scripts
sudo cp ~/naraeon-ssd/ezerase/liveboot/scripts_chroot/*.sh ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/scripts/
sudo mkdir ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/nvme-cli/
sudo cp -r ~/naraeon-ssd/ezerase/liveboot/nvme-cli/* ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/nvme-cli/
sudo mkdir ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/sedutil/
sudo cp -r ~/naraeon-ssd/ezerase/liveboot/sedutil/* ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/sedutil/
sudo mkdir -p ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/usr/lib/pm-utils/sleep.d/
sudo cp ~/naraeon-ssd/ezerase/liveboot/config/99_resume.sh ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/usr/lib/pm-utils/sleep.d/
rm -rf ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia_init/
mkdir ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia_init/
cp -r ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia/* ~/naraeon-ssd/ezerase/liveboot/chroot_nvidia_init/
