export HOME=/root
export LC_ALL=C
cd /
apt-get install --no-install-recommends --yes util-linux &&
apt-get --no-install-recommends --yes install \
linux-image-amd64 live-boot &&
apt-get --no-install-recommends --yes install \
xserver-xorg-core xserver-xorg xinit python3-tk \
xterm nano hdparm xli slim blackbox fonts-nanum
