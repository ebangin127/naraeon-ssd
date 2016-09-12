cd /
mount none -t proc /proc 
mount none -t sysfs /sys 
mount none -t devpts /dev/pts 
export HOME=/root 
export LC_ALL=C 
echo "deb http://ftp.kr.debian.org/debian/ stretch main contrib non-free" > /etc/apt/sources.list
apt-get update && 
apt-get install dialog dbus -t stretch --yes && 
dbus-uuidgen > /var/lib/dbus/machine-id && 
echo "naraeon-live" > /etc/hostname
chmod 755 /usr/lib/pm-utils/sleep.d/*.sh
