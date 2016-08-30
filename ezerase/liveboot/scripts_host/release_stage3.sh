#!/bin/bash
shopt -s extglob
cd ~/naraeon-ssd/ezerase/liveboot/
sudo umount -lf chroot_release/dev 
sudo rm -rf chroot_release/usr/share/fonts/truetype/nanum/!(NanumG*)
sudo rm -rf chroot_release/usr/share/fonts/truetype/dejavu &&
sudo rm -rf chroot_release/var/lib/apt &&
sudo rm -rf chroot_release/var/lib/dpkg &&
sudo rm -r chroot_release/usr/share/doc/*/!(copyright)
sudo rm -rf chroot_release/usr/share/man &&
cd ~/naraeon-ssd/ezerase/liveboot &&
sudo rm -rf image/live/filesystem.squashfs &&
sudo mksquashfs chroot_release image/live/filesystem.squashfs -e boot -b 1048576 -comp xz -Xdict-size 100% &&
sudo rm -rf /mnt/windows/naraeon-live.iso &&
cd ~/naraeon-ssd/ezerase/liveboot/image && rm ../iso/naraeon-live.iso 
xorriso -as mkisofs \
   -o ../iso/naraeon-live.iso \
   -isohybrid-mbr isolinux/isohdpfx.bin \
   -c isolinux/boot.cat \
   -b isolinux/isolinux.bin \
      -no-emul-boot -boot-load-size 4 -boot-info-table \
   -eltorito-alt-boot \
   -e isolinux/efiboot.img \
      -no-emul-boot \
      -isohybrid-gpt-basdat \
   . && cd .. &&
sudo cp iso/naraeon-live.iso /mnt/windows/ &&
sudo rm -rf chroot_release
