export HOME=/root
export LC_ALL=C
cd /
apt-get -t jessie-backports --no-install-recommends --yes install \
linux-image-amd64 live-boot xserver-xorg-video-all xserver-xorg-core xserver-xorg blackbox xinit
apt-get -t jessie --no-install-recommends --yes install \
xterm nano hdparm python3-tk xli slim fonts-nanum
apt-get -t stretch --no-install-recommends --yes install \
util-linux