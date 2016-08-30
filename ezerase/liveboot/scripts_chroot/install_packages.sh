export HOME=/root
export LC_ALL=C
cd /
apt-get install -t stretch --no-install-recommends --yes \
util-linux
apt-get -t jessie-backports --no-install-recommends --yes install
linux-image-amd64 live-boot systemd systemd-sysv \
xserver-xorg-core xserver-xorg xinit &&
apt-get -t jessie --no-install-recommends --yes install \
python3-tk xterm nano hdparm xli slim blackbox fonts-nanum
