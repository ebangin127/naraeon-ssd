export HOME=/root
export LC_ALL=C
cd /
apt-get install -t sid --no-install-recommends --yes util-linux &&
apt-get -t stretch --no-install-recommends --yes install \
linux-image-amd64 live-boot systemd,systemd-sysv \
xserver-xorg-core xserver-xorg xinit python3-tk \
xterm nano hdparm xli slim blackbox fonts-nanum
