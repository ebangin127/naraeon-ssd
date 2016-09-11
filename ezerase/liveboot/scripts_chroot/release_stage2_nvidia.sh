cd /
mount none -t proc /proc 
mount none -t sysfs /sys 
mount none -t devpts /dev/pts 
export HOME=/root 
export LC_ALL=C 
echo "deb http://ftp.kr.debian.org/debian/ stretch main contrib non-free" > /etc/apt/sources.list
apt-get update && 
apt-get install dialog dbus nvidia-driver -t stretch --yes && 
dbus-uuidgen > /var/lib/dbus/machine-id && 
echo "naraeon-live" > /etc/hostname
apt-get clean
dpkg --remove --force-remove-reinstreq apt &&
rm -f /var/lib/dbus/machine-id &&
rm -rf /usr/share/locale/* &&
rm -rf /tmp/* &&
rm /etc/resolv.conf &&
umount -lf /dev/pts &&
umount -lf /sys &&
umount -lf /proc

