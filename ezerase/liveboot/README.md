# Naraeon SSD Tools - Secure Erase Image
Live Linux for Secure Erase SSDs (ATA and NVMe). Requires AMD64 supported platform. Both bios and efi booting methods are supported. Secure boot is not available.

## License
Mainly GPL v3. Packages listed below follows its license.
Background image(Not included in this repo.) follows Shutterstock Standard License.

## Based on
Debian jessie (util-linux from stretch) / nvme-cli / hdparm

## Package used
linux-image-amd64 live-boot fonts-nanum nano dbus blackbox

xserver-xorg-core xserver-xorg xinit python3-tk hdparm slim

## How to get into chroot environment
~~~~
sudo mount -o bind /dev chroot/dev && sudo cp /etc/resolv.conf chroot/etc/resolv.conf
sudo chroot chroot
mount none -t proc /proc
mount none -t sysfs /sys
mount none -t devpts /dev/pts
export HOME=/root
export LC_ALL=C
apt-get update
apt-get install dialog dbus --yes --force-yes
dbus-uuidgen > /var/lib/dbus/machine-id
echo "naraeon-live" > /etc/hostname
~~~~

## How to get out chroot environment
~~~~
rm -f /var/lib/dbus/machine-id
apt-get clean
rm -rf /tmp/*
rm /etc/resolv.conf
umount -f /dev/pts
umount -f /sys
umount -f /proc
exit
sudo umount -lf chroot/dev
~~~~

## How to build
Firstly, you should get your own bg.jpg. It should be at chroot/root/bg.jpg

And then build squashfs with these options.
~~~~
sudo mksquashfs chroot image/live/filesystem.squashfs -e boot -b 1048576 -comp xz -Xdict-size 100%
~~~~
Lastly, build iso with these options.
~~~~
cd image && xorriso -as mkisofs \
   -o iso/naraeon-live.iso \
   -isohybrid-mbr isolinux/isohdpfx.bin \
   -c isolinux/boot.cat \
   -b isolinux/isolinux.bin \
      -no-emul-boot -boot-load-size 4 -boot-info-table \
   -eltorito-alt-boot \
   -e isolinux/efiboot.img \
      -no-emul-boot \
      -isohybrid-gpt-basdat \
   . && cd ..
~~~~
