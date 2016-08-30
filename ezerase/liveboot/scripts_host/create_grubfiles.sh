cd ~/live_boot/
sudo grub-mkimage --config=grubconfig/embedded.cfg -O x86_64-efi -o './image/EFI/BOOT/bootx64.efi' part_gpt part_msdos ntfs \
ntfscomp hfsplus fat ext2 normal chain boot configfile linux multiboot iso9660 gfxmenu gfxterm loadenv efi_gop \
efi_uga loadbios fixvideo png ext2 ntfscomp loopback search minicmd cat cpuid appleldr elf usb videotest halt \
help ls reboot echo test normal sleep memdisk tar font video_fb video gettext true video_bochs video_cirrus \
multiboot2 acpi gfxterm_background gfxterm_menu
sudo rm ~/live_boot/image/isolinux/efiboot.img
sudo mkdosfs -F12 -n "NSTERASE" -C ~/live_boot/image/isolinux/efiboot.img 2048
sudo mcopy -s -i ~/live_boot/image/isolinux/efiboot.img ~/live_boot/image/EFI ::

