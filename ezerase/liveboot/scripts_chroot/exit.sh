cd /
rm -f /var/lib/dbus/machine-id 
apt-get clean 
rm -rf /tmp/* 
rm /etc/resolv.conf 
umount -lf /dev/pts 
umount -lf /sys 
umount -lf /proc
exit
