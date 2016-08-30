export HOME=/root
export LC_ALL=C
cd /
apt-get -t jessie-backports --yes install \
linux-image-amd64 live-boot &&
apt-get -t jessie --yes install \
xserver-xorg-core xserver-xorg xinit xterm \
nano hdparm python3-tk xli slim blackbox fonts-nanum
apt-get -t stretch --yes install \
util-linux