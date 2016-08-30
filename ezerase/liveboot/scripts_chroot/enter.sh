cd /
mount none -t proc /proc 
mount none -t sysfs /sys 
mount none -t devpts /dev/pts 
export HOME=/root 
export LC_ALL=C 
echo "deb http://ftp.kr.debian.org/debian/ sid main" >> /etc/apt/sources.list
echo "deb http://ftp.kr.debian.org/debian/ jessie-backports main" >> /etc/apt/sources.list
echo "deb http://ftp.kr.debian.org/debian/ jessie main" >> /etc/apt/sources.list
apt-get update && 
apt-get install dialog dbus -t jessie --yes && 
dbus-uuidgen > /var/lib/dbus/machine-id && 
echo "naraeon-live" > /etc/hostname
