export HOME=/root
export LC_ALL=C
cd /
apt-get -t stretch --no-install-recommends --yes install \
linux-image-amd64 xserver-xorg-core xserver-xorg xterm \
x11-xserver-utils xinit util-linux live-boot systemd systemd-sysv \
blackbox nano hdparm python3-tk xli slim fonts-nanum pm-utils xfce4-screenshooter
echo "nls_ascii" >> /etc/initramfs-tools/modules
update-initramfs -u