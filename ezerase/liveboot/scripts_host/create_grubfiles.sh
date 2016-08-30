cd ~/naraeon-ssd/ezerase/liveboot/
grub-mkimage --config=grubconfig/embedded.cfg -O x86_64-efi -o './image/EFI/BOOT/bootx64.efi' part_gpt part_msdos ntfs \
ntfscomp hfsplus fat ext2 normal chain boot configfile linux multiboot iso9660 gfxmenu gfxterm loadenv efi_gop \
efi_uga loadbios fixvideo png ext2 ntfscomp loopback search minicmd cat cpuid appleldr elf usb videotest halt \
help ls reboot echo test normal sleep memdisk tar font video_fb video gettext true video_bochs video_cirrus \
multiboot2 acpi gfxterm_background gfxterm_menu
rm ~/naraeon-ssd/ezerase/liveboot/image/isolinux/efiboot.img
mkdosfs -F12 -n "NSTERASE" -C ~/naraeon-ssd/ezerase/liveboot/image/isolinux/efiboot.img 2048
mcopy -s -i ~/naraeon-ssd/ezerase/liveboot/image/isolinux/efiboot.img ~/naraeon-ssd/ezerase/liveboot/image/EFI ::

