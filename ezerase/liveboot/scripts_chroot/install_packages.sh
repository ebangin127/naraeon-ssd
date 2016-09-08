export HOME=/root
export LC_ALL=C
cd /
apt-get -t jessie-backports --no-install-recommends --yes install \
linux-image-amd64 live-boot xserver-xorg-core xserver-xorg xterm \
x11-xserver-utils
apt-get -t jessie --no-install-recommends --yes install \
blackbox xinit nano hdparm python3-tk xli slim fonts-nanum
apt-get -t stretch --no-install-recommends --yes install \
util-linux