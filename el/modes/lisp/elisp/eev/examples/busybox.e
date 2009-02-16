# E-scripts on busybox

# Author:  Eduardo Ochs <edrx@mat.puc-rio.br>
# Version: 2005may03 15:20

# Quick index (blocks on which I'm working actively):
#   (to "bb_etc_inittab")
#   (to "bb_etc_profile")
#   (to "iso-qemu-main")

# Note: don't expect to just go to the "...-main" blocks, type F9, F9,
# F9, ... and have everything working; there are some steps - like
# downloading the ".tar.gz"s with the source for busybox and grub (use
# psne http://foo.org/bar/file.tar.gz for that) that are not
# automated, and are not going to be.

# which ones of those have an inittab?

# bash,      chroot
# |busybox,  chroot
# ||busybox, chroot, iptables, telnetd
# |||
# ||| debian's   initrd, unpack
# ||| |debian's  initrd,               qemu
# ||| ||debian's initrd, busybox-0.60, qemu
  ||| |||
# ||| ||| busybox-1.00,     initrd,          qemu
# ||| ||| |busybox-1.00,    initrd, netcat,  qemu
# ||| ||| ||busybox-1.00,   initrd, telnetd, qemu
# ||| ||| |||busybox-1.00,  initrd, telnetd, grub, iso,     qemu
# ||| ||| ||||busybox-1.00, initrd, telnetd, grub, mkisofs, qemu
# ||| ||| |||||

% install-bash
% chroot-bash
% umount-guestfs
% create-guestfs
% mount-guestfs
% install-busybox
% create-devices
% chroot-ash
% block-telnet-port
% qemu-linux-guestimg
% qemu-isoimg

% install-etcpasswdgoup

#               «.busybox-config»	(to "busybox-config")
#               «.busybox-compile»	(to "busybox-compile")
#               «.busybox-tags»		(to "busybox-tags")

#               «.bb_umounts»		(to "bb_umounts")
#               «.bb_mounts»		(to "bb_mounts")
#               «.bb_mkdirs»		(to "bb_mkdirs")
#               «.bb_functions»		(to "bb_functions")
#               «.bb_addbusybox»	(to "bb_addbusybox")
#               «.bb_etc_passwd_group»	(to "bb_etc_passwd_group")
#               «.bb_busybox.conf»	(to "bb_busybox.conf")
#               «.bb_empty_e2fs»	(to "bb_empty_e2fs")

#               «.bb_chroot_main»	(to "bb_chroot_main")

#               «.sparse-hda.img»	(to "sparse-hda.img")
#               «.bb_cp_modules»	(to "bb_cp_modules")
#               «.bb_etc_profile»	(to "bb_etc_profile")
#               «.bb_mknods»		(to "bb_mknods")
#               «.bb_etc_init.d_rcS»	(to "bb_etc_init.d_rcS")
#               «.bb_etc_inittab»	(to "bb_etc_inittab")

#               «.bbinitrd-qemu-main»	(to "bbinitrd-qemu-main")

#               «.iptables-ping»	(to "iptables-ping")
#               «.iptables-clear»	(to "iptables-clear")
#               «.iptables-set» 	(to "iptables-set")

#               «.grub-compile»		(to "grub-compile")
#               «.iso-mkdirs»		(to "iso-mkdirs")
#               «.iso-menu.lst»		(to "iso-menu.lst")
#               «.iso-mkisofs»		(to "iso-mkisofs")
#               «.iso-qemu-run»		(to "iso-qemu-run")

#               «.iso-qemu-main»	(to "iso-qemu-main")
#               «.iso-qemu-main-2»	(to "iso-qemu-main-2")

#               «.cdrecord»		(to "cdrecord")

#               «.download-iso-run-qemu»	(to "download-iso-run-qemu")

#               «.standard-initrd.img»		(to "standard-initrd.img")
#               «.unpack-initrd.img»		(to "unpack-initrd.img")
#               «.about-loadmodules»		(to "about-loadmodules")
#               «.rm-some-initrdfiles-modules»(to "rm-some-initrdfiles-modules")
#               «.cp-initrdfiles-modules»	(to "cp-initrdfiles-modules")

›
(eek "C-a C-q 233 C-o")

(defun mkto2 () (interactive)
  (mkto) (eek "<up> C-a C-SPC <down> C-w")
  (save-excursion (ee-goto-position "\233") (eek "C-a C-y")))

# (eev "cd $EEVE/; a2ps -=p2iso busybox.e; mv o.ps /tmp/; gv /tmp/o.ps")



#####
#
# compiling busybox 1.00
# 2005feb17
#
#####

# http://www.busybox.net/downloads/busybox-1.00.tar.gz
# http://angg.twu.net/BUSYBOX/config-1.00

# Debian's busybox is 0.60.5, which is very old and doesn't have many
# of the features that we need (for example, a telnetd)... so we use
# the upstream version.
#
# «busybox-config»  (to ".busybox-config")
# Because setting the options in "make menuconfig" takes a long time...
#
mkdir ~/BUSYBOX/
cd    ~/BUSYBOX/
wget http://angg.twu.net/BUSYBOX/config-1.00

#
# «busybox-compile»  (to ".busybox-compile")
rm -Rv ~/usrc/busybox-1.00/
tar -C ~/usrc/ -xvzf $S/http/www.busybox.net/downloads/busybox-1.00.tar.gz
cd     ~/usrc/busybox-1.00/

# (find-busyboxfile "Makefile" "menuconfig:")
# (find-busyboxfile "scripts/config/")
make -C scripts/config ncurses conf mconf     2>&1 | tee omsc

# (find-fline "~/BUSYBOX/")
# (find-fline "~/BUSYBOX/config-1.00")
cp -iv ~/BUSYBOX/config-1.00 .config

# make menuconfig
# cp -iv .config ~/BUSYBOX/config-1.00

cd ~/usrc/busybox-1.00/
make dep     2>&1 | tee omd
make         2>&1 | tee om

#
# «busybox-tags»  (to ".busybox-tags")
cd ~/usrc/busybox-1.00/
find * -type f -name '*.[ch]' | grep -v '^scripts/' | sort > .files.ch
etags $(<.files.ch)

#
# (code-c-d "busybox" "~/usrc/busybox-1.00/")
# (find-busyboxfile "")
# (find-busyboxfile "Makefile")
# (find-busyboxfile "Makefile" "menuconfig:")

# (find-busyboxfile "INSTALL")
# (find-busyboxfile "busybox.links")




#####
#
# busybox in a chrooted environment
# 2005apr08
#
#####

#
# «bb_umounts»  (to ".bb_umounts")
umount /tmp/bb/dev/pts
umount /tmp/bb/dev
umount /tmp/bb/proc

#
# «bb_mounts»  (to ".bb_mounts")
mkdir -p /tmp/bb/dev/
mkdir -p /tmp/bb/dev/pts/
mkdir -p /tmp/bb/proc/
mount -t devfs  bbdev     /tmp/bb/dev
mount -t proc   bbproc    /tmp/bb/proc
mount -t devpts bbddevpts /tmp/bb/dev/pts

#
# «bb_mkdirs»  (to ".bb_mkdirs")
mkdir -p /tmp/bb/dev/
mkdir -p /tmp/bb/dev/pts/
mkdir -p /tmp/bb/proc/

mkdir -p /tmp/bb/bin
mkdir -p /tmp/bb/sbin
mkdir -p /tmp/bb/usr/bin
mkdir -p /tmp/bb/usr/sbin

mkdir -p /tmp/bb/tmp
ln -s .. /tmp/bb/tmp/bb

chmod 1777      /tmp/bb/tmp/
mkdir -p        /tmp/bb/home/edrx/
chown 1000:1000 /tmp/bb/home/edrx/

#
# «bb_functions»  (to ".bb_functions")
bb_getlibs () {
  { for i in $*; do ldd $i; done; } | awk '{print $3}' | sort | uniq;
}
bb_addfiles () { cp --parents -v $* /tmp/bb/; }
bb_addlibsfor () { bb_addfiles $(bb_getlibs $*); }

#
# «bb_addbusybox»  (to ".bb_addbusybox")
cd ~/usrc/busybox-1.00/
cp -v          ./busybox /tmp/bb/bin/busybox
for i in $(cat ./busybox.links | sed s_^/__ ); do
  echo $i
  ln /tmp/bb/bin/busybox /tmp/bb/$i
done

bb_addlibsfor /tmp/bb/bin/busybox

#
# «bb_etc_passwd_group»  (to ".bb_etc_passwd_group")
mkdir /tmp/bb/etc/
cat > /tmp/bb/etc/passwd <<'%%%'
root::0:0:root:/:/bin/sh
edrx::1000:1000:edrx,,,:/home/edrx:/bin/sh
aleph::1001:1001:aleph,,,:/:/bin/sh
beth::1002:1002:beth,,,:/:/bin/sh
r::0:0:root:/:/bin/sh
e::1000:1000:edrx,,,:/home/edrx:/bin/sh
%%%
# root->root, edrx->edrx:
cat > /tmp/bb/etc/passwd <<'%%%'
root:$1$$oCLuEVgI1iAqOA8pwkzAg1:0:0:root:/:/bin/sh
edrx:$1$$nO9T/jR80jiG3aAiOsSSV/:1000:1000:edrx,,,:/home/edrx:/bin/sh
aleph::1001:1001:aleph,,,:/:/bin/sh
beth::1002:1002:beth,,,:/:/bin/sh
r::0:0:root:/:/bin/sh
e::1000:1000:edrx,,,:/home/edrx:/bin/sh
%%%
cat > /tmp/bb/etc/group <<'%%%'
root:*:0:
edrx:!:1000:
aleph:!:1001:
beth:!:1002:
%%%

#
# «bb_busybox.conf»  (to ".bb_busybox.conf")
# (find-busyboxfile "applets/applets.c" "Using fallback suid method")
# (find-busyboxfile "sysdeps/linux/Config.in" "_FEATURE_SUID_CONFIG_QUIET")
# (find-busyboxfile "sysdeps/linux/Config.in" "CONFIG_FEATURE_SUID_CONFIG")
# http://www.softforge.de/bb/suid.html
mkdir /tmp/bb/etc/
touch /tmp/bb/etc/busybox.conf

#
# «bb_empty_e2fs»  (to ".bb_empty_e2fs")
rm -Rv /tmp/bb/
mkdir  /tmp/bb/
# dd bs=1024 count=4096 if=/dev/zero of=/tmp/bb.img
  dd bs=1024 count=8192 if=/dev/zero of=/tmp/bb.img
mke2fs -F /tmp/bb.img
mount -o loop /tmp/bb.img /tmp/bb/

#
# «bb_chroot_main»  (to ".bb_chroot_main")
 (eechannel-xterm "A")
sudo ~/run-zsh
 (find-sh0 "sudo killall -9 telnetd")
 (eevnow-at "iptables-clear")
 (eevnow-at "iptables-set")
 (eevnow-at "bb_umounts")
rm -Rv /tmp/bb/
 (eevnow-at "bb_mounts")
 (eevnow-at "bb_mkdirs")
 (eevnow-at "bb_functions")
 (eevnow-at "bb_addbusybox")
 (eevnow-at "bb_etc_passwd_group")
 (eevnow-at "bb_busybox.conf")

chroot /tmp/bb /bin/ash
passwd root
root
root
passwd edrx
edrx
edrx

# login
# edrx
# edrx
# exit

telnetd &
exit

#
 (eechannel-xterm "A")
telnet 127.0.0.1
edrx
edrx

#

 (eechannel-xterm "bb-root" (ee-split "sudo chroot /tmp/bb /bin/ash"))

# (find-busyboxfile "")
# (find-busyboxfile "docs/BusyBox.txt")

# (find-es "initrd" "add_binaries_and_libs")




#####
#
# iptables (to reject outside connections to telnetd)
# 2005apr19
#
#####

#
# «iptables-ping»  (to ".iptables-ping")
# (find-udw3m "iptables/html/packet-filtering-HOWTO-7.html" "-p icmp -j DROP")
#
iptables -D INPUT -s 127.0.0.1 -p icmp -j DROP
ping -c 2 -W 1       127.0.0.1 | egrep 'from|transmitted'

iptables -A INPUT -s 127.0.0.1 -p icmp -j DROP
iptables -L INPUT
ping -c 2 -W 1       127.0.0.1 | egrep 'from|transmitted'

iptables -L INPUT --line-numbers
iptables -D INPUT 1

#
# (find-fline "/etc/services")
# (find-udw3m "iptables/html/packet-filtering-HOWTO-7.html" "-p icmp -j DROP")
# (find-sh "iptables --help")
# (find-man "iptables" "    -X")

#
# «iptables-clear»  (to ".iptables-clear")
iptables -D INPUT -p TCP --dport telnet -j INPUTFILTER
iptables -F INPUTFILTER
iptables -X INPUTFILTER

#
# «iptables-set»  (to ".iptables-set")
iptables -N INPUTFILTER
iptables -A INPUTFILTER -s 127.0.0.1    -j ACCEPT
iptables -A INPUTFILTER -j DROP
iptables -A INPUT -p TCP --dport telnet -j INPUTFILTER

#
iptables -L INPUTFILTER --line-numbers
iptables -L

#
# Some tests (run with F9):
 (eechannel-xterm "A")
 (eebgTWU)
# telnet 127.0.0.1
telnet 200.141.116.226
edrx
edrx
exit

#
# (find-status   "iptables")
# (find-vldifile "iptables.list")
# (find-udfile   "iptables/")
# (find-udw3m "iptables/html/packet-filtering-HOWTO.html")
# (find-udw3m "iptables/html/packet-filtering-HOWTO-5.html")
# (find-udw3m "iptables/html/packet-filtering-HOWTO-7.html")
# (find-udfile "iptables/examples/3iptables-ppp_up-rules.gz")

# (find-udfile "focalinux/text/avancado/index.txt.gz")
# (find-udfile "focalinux/text/avancado/index.txt.gz" "iptables -P INPUT DROP")
# (find-available "ebtables")




#####
#
# subroutines specific to making initrds and running qemu
# 2005apr20
#
#####

#
# «sparse-hda.img»  (to ".sparse-hda.img")
python <(cat <<'%%%'
image = open("/tmp/hda.img", "w")
image.truncate(1024 * 1048576L)
image.close()
%%%
)

#
# «bb_cp_modules»  (to ".bb_cp_modules")
# (find-k26confvar "CONFIG_NE2000")
# (find-k26confvar "CONFIG_NE2K_PCI")
# (find-fline "/lib/modules/2.6.8-1-386/")
# (find-fline "/lib/modules/2.6.8-1-386/kernel/drivers/net/")
# (find-fline "/lib/modules/2.6.8-1-386/modules.dep")
# (find-fline "/lib/modules/2.6.8-1-386/modules.dep" "ne2k-pci.ko")
#
cd /lib/modules/2.6.8-1-386/kernel/
cp drivers/net/ne.ko        /tmp/bb/tmp/
cp drivers/net/ne2k-pci.ko  /tmp/bb/tmp/
cp drivers/net/8390.ko      /tmp/bb/tmp/
cp lib/crc32.ko             /tmp/bb/tmp/

cp net/sunrpc/sunrpc.ko     /tmp/bb/tmp/
cp fs/lockd/lockd.ko        /tmp/bb/tmp/
cp fs/nfs/nfs.ko            /tmp/bb/tmp/
cp fs/exportfs/exportfs.ko  /tmp/bb/tmp/
cp fs/nfsd/nfsd.ko          /tmp/bb/tmp/

cp drivers/parport/parport.ko     /tmp/bb/tmp/
cp drivers/parport/parport_pc.ko  /tmp/bb/tmp/
cp drivers/char/lp.ko             /tmp/bb/tmp/
cp drivers/net/plip.ko            /tmp/bb/tmp/

cp arch/i386/kernel/apm.ko  /tmp/bb/tmp/

cp drivers/ide/ide-core.ko    /tmp/bb/tmp/
cp drivers/ide/pci/piix.ko    /tmp/bb/tmp/
cp drivers/ide/pci/generic.ko /tmp/bb/tmp/
cp drivers/ide/ide-disk.ko    /tmp/bb/tmp/
cp drivers/ide/ide-cd.ko      /tmp/bb/tmp/
cp drivers/cdrom/cdrom.ko     /tmp/bb/tmp/
cp fs/vfat/vfat.ko            /tmp/bb/tmp/
cp fs/fat/fat.ko              /tmp/bb/tmp/
cp fs/msdos/msdos.ko          /tmp/bb/tmp/

#
# «bb_etc_profile»  (to ".bb_etc_profile")
# (find-busyboxfile "util-linux/mount.c")
# (find-fline "/etc/fstab" "proc")
# (find-esfile "initrd.e" "cat > etc/profile <<'%%%'")

cat > /tmp/bb/etc/profile <<'%%%'
export HOST=172.20.0.1
export HERE=172.20.0.2
export EE=/tmp/ee.sh
alias  laf='ls -laF'
alias  w=which
mountproc   () { mount -t proc   proc   /proc; }
mountdev    () { mount -t devfs  devfs  /dev; }
mountdevpts () { mount -t devpts devpts /dev/pts; }
mounts      () { mountproc; mountdevpts; }
mounts2     () { mountproc; mountdev; mountdevpts; }
insmod_eth  () { cd /tmp/; insmod crc32.ko; insmod 8390.ko;
                 insmod ne2k-pci.ko; }
insmod_plip () { cd /tmp; insmod parport.ko;
                 insmod plip.ko; insmod parport_pc.ko; }
up_lo       () { ifconfig lo 127.0.0.1; route add 127.0.0.1 lo; }
up_eth      () { ifconfig eth0 172.20.0.2; route add 172.20.0.0 eth0; }
up_plip     () { ifconfig plip0 10.0.0.4 pointopoint 10.0.0.3;
                 route add 10.0.0.0 plip0; }
prep        () { mounts;  insmod_eth; up_eth; }
prep2       () { mounts2; insmod_eth; up_eth; }
fromhost    () { nc -l -p 1234; }
tohost      () { nc -q 0 $HOST 1234; }
ee          () { set -v; . $EE; set +v; }
#
vars_eth    () { export HOST=172.20.0.1; export HERE=172.20.0.2; }
vars_plip   () { export HOST=10.0.0.3;   export HERE=10.0.0.4; }

prep2ep     () { prep2; insmodps; up_plip; }
%%%

#
# «bb_mknods»  (to ".bb_mknods")
cd /tmp/bb/dev/
mknod null    c 1 3
mknod console c 5 1
mknod ptmx    c 5 2
#
# mknod tty     c 5 0
# mknod tty1    c 4 1
# mknod tty2    c 4 2
# mknod tty3    c 4 3
# mknod tty4    c 4 4

mknod tty  c 5   0; for i in $(seq 1 4); do mknod tty$i  c 4 $i; done
mknod vcs  c 7   0; for i in $(seq 1 4); do mknod vcs$i  c 7 $i; done
mknod vcsa c 7 128; for i in $(seq 1 4); do mknod vcsa$i c 7 $[128+$i]; done

mknod hda    b 3 0
mknod hda1   b 3 1
mknod hda2   b 3 2


#
# «bb_etc_init.d_rcS»  (to ".bb_etc_init.d_rcS")
# Note that this is optional!
# Use only when you want to do all this at startup.

mkdir -p /tmp/bb/etc/init.d/
cat    > /tmp/bb/etc/init.d/rcS <<'%%%'
#!/bin/sh
mount -t proc   proc   /proc
echo "  Before mounts:"; mount
#mount -t devfs devfs  /dev
mount -t devpts devpts /dev/pts
echo "  After mounts:"; mount
#
# cd /tmp/bb/dev/
# mknod null    c 1 3
# mknod console c 5 1
# mknod ptmx    c 5 2
#
# mknod tty     c 5 0
# mknod tty1    c 4 1
# mknod tty2    c 4 2
# mknod tty3    c 4 3
# mknod tty4    c 4 4
#
# (find-eevex "busybox.e" "bb_etc_profile")
. /etc/profile
up_lo
insmod_plip && up_plip
insmod_eth  && up_eth
#
telnetd
%%%
chmod 755 /tmp/bb/etc/init.d/rcS

#
# Notes about the "job control turned off" problem:
#
# (find-k26file "init/main.c" "/sbin/init")
# (find-busyboxfile "busybox.links" "/sbin/init")
# (find-busyboxfile "docs/BusyBox.txt" "without an inittab")
# (find-busyboxfile "init/init.c" "No inittab file -- set up some default")
# (find-busyboxfile "docs/BusyBox.txt" "controlling tty")
# (find-busyboxfile "init/init.c" "static void set_term(int fd)")
# (find-busyboxfile "init/init.c" "setsid();")
# (find-busyboxfile "shell/ash.c" "can't access tty; job control turned off")

# Entries like "...:/bin/sh" in inittab create "non-login" shells that
# don't read /etc/profile, entries like "...:-/bin/sh" create login
# shells that do...
#
# (find-busyboxfile "shell/ash.c" "if (argv[0] && argv[0][0] == '-')")
# (find-busyboxfile "shell/ash.c" "read_profile(\"/etc/profile\");")
# (find-busyboxfile "init/init.c" "-/bin/sh")

#
# «bb_etc_inittab»  (to ".bb_etc_inittab")
# This is optional too - this is the default inittab with just one
# change: moving the first shell from the default tty, to tty1, i.e.,
# "::askfirst:/bin/sh" -> "tty1::askfirst:/bin/sh".

cat > /tmp/bb/etc/inittab <<'%%%'
::sysinit:/etc/init.d/rcS
::ctrlaltdel:/sbin/reboot
::shutdown:/sbin/swapoff -a
::shutdown:/bin/umount -a -r
::restart:/sbin/init
tty1::askfirst:-/bin/sh
tty2::askfirst:-/bin/sh
tty3::askfirst:-/bin/sh
tty4::askfirst:-/bin/sh
%%%

#




#####
#
# making an initrd
# 2005apr19
#
#####

#
# «bbinitrd-qemu-main»  (to ".bbinitrd-qemu-main")
 (eechannel-xterm "A")
sudo ~/run-zsh
 (message (find-sh0 "sudo killall -9 telnetd"))
 (eevnow-at "iptables-clear")
 (eevnow-at "iptables-set")
 (eevnow-at "bb_umounts")
rm -Rv /tmp/bb/
dd bs=1024 count=3072 if=/dev/zero of=/tmp/bb.img
mke2fs -F /tmp/bb.img
mkdir /tmp/bb/
mount -o loop /tmp/bb.img /tmp/bb/
 (eevnow-at "bb_mounts")
 (eevnow-at "bb_mkdirs")
 (eevnow-at "bb_functions")
 (eevnow-at "bb_addbusybox")
 (eevnow-at "bb_etc_passwd_group")
 (eevnow-at "bb_busybox.conf")

 (eevnow-at "bb_umounts")
 (eevnow-at "bb_mknods")
 (eevnow-at "bb_etc_profile")
 (eevnow-at "bb_cp_modules")

 (eevnow-at "sparse-hda.img")

cd /tmp/
umount /tmp/bb/

cp -v /boot/vmlinuz-2.6.8-1-386    /tmp/vmlinuz

 (eechannel-xterm "A")
# Try the initrd with qemu.
# (find-man "1 qemu")
cd /tmp/
qemu -hda    /tmp/hda.img  \
     -kernel /tmp/vmlinuz  \
     -monitor stdio -m 64  \
     -pci                  \
     -initrd /tmp/bb.img

#



# (find-k26file "")

# (find-sh "ifconfig; route")
# (to "bb_etc_profile")
#   init
#   prep2
#   telnetd &

 (eechannel-xterm "B")
telnet 172.20.0.2
root
root

date | netcat -q 0 172.20.0.2 1234



# (find-man "1 qemu" "-nographic")
# (find-man "1 qemu" "-monitor dev")
qemu -m 64 -isa -n /tmp/my-qemu-ifup \


# What to search if plip doesn't work (or is too slow) with the
# default settings, i.e., no IRQ:
#
# (to "bb_etc_profile")
# (find-k26file "drivers/parport/Kconfig")
# (find-k26docfile "parport.txt")




#####
#
# compiling grub 0.96
# 2005may01
#
#####

# ftp://alpha.gnu.org/gnu/grub/grub-0.96.tar.gz
# (code-c-d "grub" "~/usrc/grub-0.96/" "~/usrc/grub-0.96/docs/grub")
# '(add-to-list 'Info-additional-directory-list "~/usrc/grub-0.96/")
# (find-grubfile "")
# (find-grubnode "Top")
# (find-grubnode "Making a GRUB bootable CD-ROM")
# See also:
# http://www.gnu.org/software/grub/grub-2.en.html

#
# «grub-compile»  (to ".grub-compile")
rm -Rv ~/usrc/grub-0.96/
tar -C ~/usrc/ -xvzf $S/ftp/alpha.gnu.org/gnu/grub/grub-0.96.tar.gz
cd     ~/usrc/grub-0.96/
./configure  2>&1 | tee oc
make         2>&1 | tee om

#





# (find-grubnode "Making a GRUB bootable CD-ROM")
# (find-man "8 mkisofs")
# (find-man "8 mkisofs" "-b eltorito_boot_image")
# (find-man "8 mkisofs" "-boot-load-size load_sectors")
# (find-man "8 mkisofs" "-boot-info-table")
# (find-man "8 mkisofs" "-no-emul-boot\n")
# (find-man "8 mkisofs" "\nEL TORITO BOOT INFORMATION TABLE\n")
# (find-fline "/boot/grub/menu.lst")

#
# «iso-mkdirs»  (to ".iso-mkdirs")
rm -Rv   /tmp/bb-iso/
mkdir    /tmp/bb-iso/
mkdir -p /tmp/bb-iso/boot/grub/
cp -v ~/usrc/grub-0.96/stage2/stage2_eltorito /tmp/bb-iso/boot/grub/

cp -v /boot/vmlinuz-2.6.8-1-386               /tmp/bb-iso/boot/
cp -v /tmp/bb.img                             /tmp/bb-iso/boot/

#
# «iso-menu.lst»  (to ".iso-menu.lst")
cat > /tmp/bb-iso/boot/grub/menu.lst <<'%%%'
timeout 5
default 0
title  Linux 2.6.8 (busybox)
  kernel /boot/vmlinuz-2.6.8-1-386 root=/dev/hda1 vga=1
  initrd /boot/bb.img
title  Linux 2.6.8 (debian default)
  kernel /boot/vmlinuz-2.6.8-1-386 root=/dev/hda3 vga=1
  initrd /boot/initrd.img-2.6.8-1-386
title  halt
  halt
%%%

#
# «iso-mkisofs»  (to ".iso-mkisofs")
mkisofs -R \
  -b boot/grub/stage2_eltorito -no-emul-boot \
  -boot-load-size 4 -boot-info-table \
  -o /tmp/bb-iso.iso /tmp/bb-iso/

#
# «iso-qemu-run»  (to ".iso-qemu-run")
# Try the CD image with qemu.
# (find-man "1 qemu")
cd /tmp/
qemu -hda    /tmp/hda.img    \
     -cdrom  /tmp/bb-iso.iso \
     -boot d                 \
     -monitor stdio -m 64    \
     -pci

#
# «iso-qemu-main»  (to ".iso-qemu-main")
 (eechannel-xterm "A")
sudo ~/run-zsh
killall -9 telnetd

 (eevnow-at "bb_umounts")
 (eevnow-at "bb_empty_e2fs")
 (eevnow-at "bb_mkdirs")
 (eevnow-at "bb_functions")
 (eevnow-at "bb_addbusybox")
 (eevnow-at "bb_busybox.conf")
 (eevnow-at "bb_mknods")
 (eevnow-at "bb_cp_modules")
 (eevnow-at "bb_etc_passwd_group")
 (eevnow-at "bb_etc_profile")
 (eevnow-at "bb_etc_init.d_rcS")
 (eevnow-at "bb_etc_inittab")

date > /tmp/bb/tmp/VERSION
echo '# (find-eevex "busybox.e" "iso-qemu-main")' \
    >> /tmp/bb/tmp/VERSION

cd     /tmp/
umount /tmp/bb/

 (eevnow-at "iso-mkdirs")
 (eevnow-at "iso-menu.lst")
 (eevnow-at "iso-mkisofs")
 (eevnow-at "sparse-hda.img")

 (eevnow-at "iso-qemu-run")

#
 (eechannel-xterm "B")
telnet 172.20.0.2
root
root

modprobe piix




#####
#
# another initrd for qemu, this one with all the initrd-ish modules
# 2005may12
#
#####

#
# «iso-qemu-main-2»  (to ".iso-qemu-main-2")
 (eechannel-xterm "A")
sudo ~/run-zsh
killall -9 telnetd

 (eevnow-at "bb_umounts")
 (eevnow-at "bb_empty_e2fs")
 (eevnow-at "bb_mkdirs")
 (eevnow-at "bb_functions")
 (eevnow-at "bb_addbusybox")
 (eevnow-at "bb_busybox.conf")
 (eevnow-at "bb_mknods")
 (eevnow-at "bb_cp_modules")
 (eevnow-at "bb_etc_passwd_group")
 (eevnow-at "bb_etc_profile")
 (eevnow-at "bb_etc_init.d_rcS")
 (eevnow-at "bb_etc_inittab")

date > /tmp/bb/tmp/VERSION
echo '# (find-eevex "busybox.e" "iso-qemu-main")' \
    >> /tmp/bb/tmp/VERSION

 ;; Experimental:
 (eevnow-at "standard-initrd.img")
 (eevnow-at "unpack-initrd.img")
 (eevnow-at "rm-some-initrdfiles-modules")
 (eevnow-at "cp-initrdfiles-modules")

cd     /tmp/
umount /tmp/bb/

 (eevnow-at "iso-mkdirs")
 (eevnow-at "iso-menu.lst")
 (eevnow-at "iso-mkisofs")
 ;; (eevnow-at "sparse-hda.img")


 (eechannel-xterm "QEMU" nil '("-geometry" "79x5"))
sudo chmod 666 /dev/net/tun
sudo chmod 666 /tmp/hda.img
R
 (eevnow-at "iso-qemu-run")

 (eechannel-xterm "A")
telnet 172.20.0.2
root
root


 (eevnow-at "iso-qemu-run")

#
 (eechannel-xterm "B")
telnet 172.20.0.2
root
root

modprobe piix


















 (eechannel-xterm "A")
sudo ~/run-zsh
killall -9 telnetd
 (eevnow-at "bb_umounts")
 (eevnow-at "bb_empty_e2fs")
 (eevnow-at "bb_mkdirs")
 (eevnow-at "bb_functions")
 (eevnow-at "bb_addbusybox")
 (eevnow-at "bb_busybox.conf")
 (eevnow-at "bb_mknods")
 (eevnow-at "bb_cp_modules")

 (eevnow-at "bb_etc_passwd_group")
 (eevnow-at "bb_etc_profile")
 (eevnow-at "bb_etc_init.d_rcS")
 (eevnow-at "bb_etc_inittab")

date > /tmp/bb/tmp/VERSION
echo '# (find-eevex "busybox.e" "iso-qemu-main")' \
    >> /tmp/bb/tmp/VERSION

cp -v /sbin/modprobe /tmp/bb/sbin/modprobe
bb_addlibsfor        /tmp/bb/sbin/modprobe

 (eevnow-at "standard-initrd.img")
 (eevnow-at "unpack-initrd.img")
 (eevnow-at "rm-some-initrd.img-modules")
 (eevnow-at "cp-initrd.img-modules")

cd     /tmp/
umount /tmp/bb/

 (eevnow-at "iso-mkdirs")
 (eevnow-at "iso-menu.lst")
 (eevnow-at "iso-mkisofs")
 ;; (eevnow-at "sparse-hda.img")


 (eechannel-xterm "QEMU" nil '("-geometry" "79x5"))
sudo chmod 666 /dev/net/tun
sudo chmod 666 /tmp/hda.img
# sudo ~/run-zsh
 (eevnow-at "iso-qemu-run")

# Test mounting from the qemu'ed guest
 (eechannel-xterm "A")
telnet 172.20.0.2
root
root



 (eevnow-at "iso-qemu-run")

#
 (eechannel-xterm "B")
telnet 172.20.0.2
root
root







#####
#
# Turning off the machine (even in qemu) from the shell
# (Note: "halt" in grub works, apm + busybox's "halt" not yet)
# (find-angg ".zshrc" "aa")
# 2005may03
#
#####

 (eechannel-xterm "B")
telnet 172.20.0.2
root
root
cd /tmp/
fromhost > apm.ko

 (eechannel-xterm "C")
cd /lib/modules/2.6.8-1-386/kernel/arch/i386/kernel/
cat apm.ko \
  | netcat -q 0 172.20.0.2 1234

 (eechannel-xterm "B")
telnet 172.20.0.2
root
root
cd /tmp/
insmod apm.ko power-off=1
halt



# Kill by hand:
# (find-sh0 "sudo killall -9 qemu")




#####
#
# record the iso image on a CD-RW
#
#####

# «cdrecord»  (to ".cdrecord")
# (find-es "cdrom" "cdrecord")

 (eechannel-xterm "A")
cd /tmp/
cdrecord speed=8 -v -v -v -v -v -dev=/dev/hdd /tmp/bb-iso.iso

cd /tmp/
cdrecord speed=8 blank=fast -v -v -v -v -v -dev=/dev/hdd /tmp/bb-iso.iso

# (find-man "cdrecord")
# (find-man "cdrecord" "fixated")
# (find-man "cdrecord" "speed")

# (find-htetfile "CD-Writing-HOWTO.gz")
# (find-htetfile "CD-Writing-HOWTO.gz" "  How to create a multi-session CD?")
# (find-htetfile "CD-Writing-HOWTO.gz" "specify the parameter blank=fast")

# (find-es "mini" "plipnfs")




#####
#
# allowing dirs from the host to be NFS-mounted by the guest
# 2005may04
#
#####

# What I changed in my /etc (by hand):

# (find-man "5 exports")
# (find-fline "$ASROOT/etc/exports")
/ 127.0.0.1(rw,async) 10.0.0.4(rw,async) 172.20.0.2(rw,async)

# Do I need this?
# (find-man "5 hosts.allow")
# (find-fline "$ASROOT/etc/hosts.allow")
127.0.0.1
10.0.0.4
172.20.0.2

# (find-fline "/etc/init.d/nfs-common")
# (find-fline "/etc/init.d/nfs-kernel-server")
# (find-fline "/etc/default/nfs-common")
# (find-fline "/etc/default/nfs-kernel-server")
# (find-fline "/etc/rc2.d/")

 (eechannel-xterm "A")
sudo ~/run-zsh
/etc/init.d/nfs-kernel-server restart
/etc/init.d/nfs-common restart

# Test using local mounts
# (find-man "5 nfs")
mkdir /tmp/host/
mount -t nfs 127.0.0.1:/     /tmp/host/; ls /tmp/host/
umount /tmp/host/
mount -t nfs 127.0.0.1:/etc/ /tmp/host/; ls /tmp/host/
umount /tmp/host/

 (eechannel-xterm "QEMU")
sudo ~/run-zsh
 (eevnow-at "iso-qemu-run")

# Test mounting from the qemu'ed guest
 (eechannel-xterm "GUEST")
telnet 172.20.0.2
root
root

cd /tmp/
insmod sunrpc.ko
insmod lockd.ko
insmod nfs.ko
echo '127.0.0.1 localhost loopback mistletoe' > /etc/hosts

mkdir  /host
mount -t nfs -o nolock 172.20.0.1:/ /host/
ls     /host/
umount /host/

# Note: if I don't use "-o nolock" I get these (kernel?) errors:
#   nfs warning: mount version older than kernel
#   portmap: server localhost not responding, timed out
#   RPC: failed to contact portmap (errno -5).
#   portmap: server localhost not responding, timed out
#   RPC: failed to contact portmap (errno -5).
#   lockd_up: makesock failed, error=-5
#   RPC: failed to contact portmap (errno -5).
#   lockd_down: no lockd running.





#####
#
# how to try it by downloading just the iso image
# 2005may04
#
#####

gzip -c9 < /tmp/bb-iso.iso \
 > /var/www/tmp/bb-iso.iso.gz
#
# (find-fline "/var/www/tmp/")

# «download-iso-run-qemu»  (to ".download-iso-run-qemu")
cd /tmp/
wget http://201.8.142.27/tmp/bb-iso.iso.gz
gunzip bb-iso.iso.gz
python <(cat <<'%%%'
image = open("/tmp/hda.img", "w")
image.truncate(1024 * 1048576L)
image.close()
%%%
)
cd /tmp/
qemu -hda    /tmp/hda.img    \
     -cdrom  /tmp/bb-iso.iso \
     -boot d                 \
     -monitor stdio -m 64    \
     -pci

# Then, if tun support exists and if qemu can open /dev/net/tun, it
# should be possible to telnet to the box...
# (find-es "qemu" "tun-tap")

 (eechannel-xterm "GUEST")
telnet 172.20.0.2
root
root





#####
#
# inspect Debian's initrd
# 2005may11
#
#####

# (find-es "initrd" "debian-busybox-initrd")
#
# «standard-initrd.img»  (to ".standard-initrd.img")
# Copy a standard initrd image from /boot/ to /tmp/initrd.img. 
# (find-sh "cd /var/lib/dpkg/info/; ls kernel-image-*.list")
# (find-fline "/boot/")
#
cp -v /boot/initrd.img-2.6.8-1-386 /tmp/initrd.img

#
# «unpack-initrd.img»  (to ".unpack-initrd.img")
#
mkdir  /tmp/initrdfs
umount /tmp/initrdfiles/dev/pts
umount /tmp/initrdfiles/dev
umount /tmp/initrdfiles/proc
umount /tmp/initrdfs
mount -v -o loop,ro /tmp/initrd.img -t cramfs /tmp/initrdfs

# Cramfs images are hard to use sometimes - among other things, they
# are read-only and they allow hardlinks between directories (most
# other filesystems do not). So we copy the files from the mounted
# image to a directory - and we use tar because "cp -av" doesn't deal
# very well with the hard-linked dirs.

rm -Rf     /tmp/initrdfiles/
mkdir      /tmp/initrdfiles/
( cd       /tmp/initrdfs/  && tar -cf - * ) \
  | tar -C /tmp/initrdfiles/ -xf -
cd         /tmp/initrdfiles/

# Unmount /tmp/initrdfs/ - with /tmp/initrdfs/ unmounted we can change
# /tmp/initrd.img safely.
#
umount     /tmp/initrdfs

#
# Which modules go there?
tkdiff            =(cd /lib/modules/2.6.8-1-386/; find * -type d | sort) \
  =(cd /tmp/initrdfiles/lib/modules/2.6.8-1-386/; find * -type d | sort) &

#
# «about-loadmodules»  (to ".about-loadmodules")
# (find-fline "/tmp/initrdfiles/")
# (find-fline "/tmp/initrdfiles/loadmodules")
# (find-fline "/tmp/initrdfiles/linuxrc")
# (find-fline "/tmp/initrdfiles/sbin/init")
# (find-fline "/tmp/initrdfiles/sbin/init" "call /loadmodules")
# (find-fline "/tmp/initrdfiles/lib/modules/2.6.8-1-386/")

# (find-fline "/usr/sbin/mkinitrd" "loadmodules")

#
# «rm-some-initrdfiles-modules»  (to ".rm-some-initrdfiles-modules")
cd /tmp/initrdfiles/
cd lib/modules/2.6.8-1-386/kernel/drivers/
rm -Rv md/ scsi/

#
# «cp-initrdfiles-modules»  (to ".cp-initrdfiles-modules")
cd /tmp/initrdfiles/
cp -R --parents lib/modules/2.6.8-1-386/ /tmp/bb/

#





#####
#
# Trying to debug busybox (ignore this block)
# 2005apr09
#
#####

# I started this because I couldn't make a telnetd to work on Debian's
# busybox... with busybox-1.00 the built-in telnetd works, but,
# anyway, how do we debug chrooted programs?

#
chroot /tmp/bb bin/login

#
# Needs to be run as root
# (ee-once (eeb-gdb-start "/tmp/bb/usr/sbin/" "chroot"))
set args /tmp/bb /bin/login
br main
br applets.c:164
br chroot_main
run

#




cat > /tmp/bb/etc/issue.net <<'%%%'
Debian GNU/Linux 3.1 initrd %t
%%%





#####
#
# mkinitrd with the new busybox
# 2005apr09
#
#####

# (find-es "initrd" "debian-busybox-initrd")
# (find-fline "/usr/sbin/mkinitrd")
# (find-fline "/usr/sbin/mkinitrd" "if [ $BUSYBOX ]")
# (find-fline "/usr/sbin/mkinitrd" "if [ $BUSYBOX ]" "Curr")
# (find-busyboxsh "./busybox")

Hypothesis: "PATH=$HOME/usrc/busybox-1.00:$PATH mkinitrd ..." will
locate busybox-1.00 before debian's busybox-0.60, and use it. Mkinitrd
gets the list of links from busybox itself, but how does it select in
which dirs they should go?




#####
#
# Debian busybox source (this block is obsolete)
# 2005apr08
#
#####

# (find-status   "busybox")
# (find-vldifile "busybox.list")
# (find-udfile   "busybox/")
# (find-man  "1 busybox")
# (find-udfile "busybox/BusyBox.txt.gz" "\n    init\n")
# (find-udfile "busybox/BusyBox.txt.gz" "\n    init\n" "controlling tty")

# http://ftp.debian.org/debian/pool/main/b/busybox/busybox_0.60.5-2.2.dsc
#
rm -Rv ~/usrc/busybox/
mkdir  ~/usrc/busybox/
cd     ~/usrc/busybox/
dpkg-source -x \
  $S/http/ftp.debian.org/debian/pool/main/b/busybox/busybox_0.60.5-2.2.dsc

#
# (code-c-d "busybox" "~/usrc/busybox/busybox-0.60.5/")
# (find-busyboxfile "")
# (find-busyboxfile "telnet.c")
# (find-busyboxfile "init.c" "static void set_term(int fd)")
# (find-busyboxfile "init.c" "setsid();")




# 2005apr20: qemu doesn't boot the image, maybe there are devices
# missing... I think that there's an option to linux to make it mount
# devfs, but I'll try some mknods first...

# (find-k26file "init/main.c" "/sbin/init")
# (find-esfile "initrd.e" "mknod tty")
# (find-esfile "initrd.e" "mount -v -o loop,ro /tmp/initrd.img")

# Here's a listing of the /dev dir in debian's initrd:
# /tmp/initrdfs/dev(root:pe)# laf
# total 5
# lrwxrwxrwx  1 root root   14 Dec 31  1969 cciss -> ../devfs/cciss
# crw-rw----  1 root root 5, 1 Dec 31  1969 console
# lrwxrwxrwx  1 root root   12 Dec 31  1969 ida -> ../devfs/ida
# lrwxrwxrwx  1 root root   12 Dec 31  1969 ide -> ../devfs/ide
# lrwxrwxrwx  1 root root   15 Dec 31  1969 mapper -> ../devfs/mapper
# lrwxrwxrwx  1 root root   11 Dec 31  1969 md -> ../devfs/md
# crw-rw-rw-  1 root root 1, 3 Dec 31  1969 null
# lrwxrwxrwx  1 root root   13 Dec 31  1969 scsi -> ../devfs/scsi

#



# Problems, 2005apr20:
# init is not very happy - Alt-F2 doesn't work, for example



# (find-busyboxfile "")
# (find-busyboxfile "networking/telnetd.c")
# (find-busyboxfile "init/init.c")
# (find-busyboxfile "init/init.c" "static void parse_inittab")
# (find-busyboxfile "docs/BusyBox.txt" "\n    init\n")
# (find-busyboxfile "console-tools/openvt.c")

# (find-busyboxfile "util-linux/nfsmount.c")

  | netcat -q 0 172.20.0.2 1234

alias w=which




#  Local Variables:
#  coding:               no-conversion
#  modes:                (fundamental-mode sh-mode emacs-lisp-mode)
#  ee-delimiter-hash:    "\n#\n"
#  ee-delimiter-percent: "\n%\n"
#  ee-anchor-format:     "«%s»"
#  End:
