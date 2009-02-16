

(hey, sorry for the mess, but I need to release!)



# «.simple-test»		(to "simple-test")
# «.qemu-debian-br-cdd»		(to "qemu-debian-br-cdd")
# «.create-hd-image»		(to "create-hd-image")
# «.linux.img-to-sarge.hdimg»	(to "linux.img-to-sarge.hdimg")
# «.swap-files»			(to "swap-files")
# «.patch-linux-qemu-fast»	(to "patch-linux-qemu-fast")
# «.rom-bug»			(to "rom-bug")
# «.tun-tap»			(to "tun-tap")
# «.fake-qemu-ifup»		(to "fake-qemu-ifup")
# «.tomsrtbt»			(to "tomsrtbt")
# «.qemu-net»			(to "qemu-net")


# (find-zsh "acse -i qemu")
# (find-dpkg-l "qemu")

# (find-status   "qemu")
# (find-vldifile "qemu.list")
# (find-udfile   "qemu/")

# (find-man "1 qemu")
# (find-man "1 qemu" "-append")
# (find-man "1 qemu-fast")
# (find-man "1 qemu-i386")
# (find-man "1 qemu-mkcow")
# (find-udfile "qemu/README.Debian")
# (find-udw3m  "qemu/qemu-doc.html")
# (find-udw3m  "qemu/qemu-tech.html")



#####
#
# A simple test
# 2004dec02
#
#####

# «simple-test»  (to ".simple-test")
# http://fabrice.bellard.free.fr/qemu/linux-test-0.5.1.tar.gz

#
# Unpack Fabrice Bellard's linux.img and bzImage-2.4.21
#
umount /tmp/linux-test/linux
rm -Rv /tmp/linux-test/
tar -C /tmp/ \
  -xvzf $S/http/fabrice.bellard.free.fr/qemu/linux-test-0.5.1.tar.gz

#
# The simplest test (needs X)
#
cd /tmp/linux-test/
qemu -m 64 -user-net \
  -kernel bzImage-2.4.21 \
  -append "root=/dev/hda" linux.img

#



#####
#
# Booting from an ISO of a Debian installation CD, installing on an HD image
# 2004dec02
#
#####

# From <http://mov.osiris.com.br/qemu.txt>:
#   qemu -cdrom /dev/cdrom -hda hd_virtual
#   qemu -cdrom /dev/cdrom -boot d hd_virtual
# (find-man "1 qemu")
# (find-man "1 qemu" "-boot")
#
# The text also suggests this, that I don't understand well:
#   echo 1024 > /proc/sys/dev/rtc/max-user-freq
# (find-fline "/proc/sys/dev/rtc/")
# (find-k24file "drivers/char/rtc.c")
# (find-k24file "drivers/char/rtc.c" "max-user-freq")
# (find-k24confvar "CONFIG_RTC")

# «qemu-debian-br-cdd»  (to ".qemu-debian-br-cdd")
#
CDIMAGEDIR=$S/http/www.postgresql.org.br/~otavio/debian-br-cdd/1.0_pre4
CDIMAGE=$CDIMAGEDIR/sarge-i386-1.raw

# Create an empty HD image as a sparse 1G file
#
python =(<<'%%%'
image = open("/tmp/br-cdd.hdimg", "w")
image.truncate(1024 * 1048576L)
image.close()
%%%)

qemu -cdrom $CDIMAGE -hda /tmp/br-cdd.hdimg -boot d

#
# Note: the emulated installation above took about 2 hours in my
# 750MHz i586, and at some point it said that the installation was
# finished and that the system would be rebooted (or something like
# that); then it gave a progress bar with some last things that were
# being done, and qemu (version 0.6.0) aborted with:
#
#   (qemu) BIOS panic at rombios.c, line 1558
#
# Saving that hdimg to ~/tmp:

cp -v --sparse=always /tmp/br-cdd.hdimg ~/tmp/br-cdd.hdimg

#
# (find-man "1 qemu-mkcow")
# (find-man "1 qemu")
# (find-sh "unhtml < /usr/share/doc/qemu/qemu-doc.html" "9000")
cd /tmp/
qemu-mkcow -f br-cdd.hdimg br-cdd.cow

CDIMAGEDIR=$S/http/www.postgresql.org.br/~otavio/debian-br-cdd/1.0_pre4
CDIMAGE=$CDIMAGEDIR/sarge-i386-1.raw

qemu -cdrom $CDIMAGE -hda /tmp/br-cdd.cow -boot c

#

Losing too many ticks!
TSC cannot be used as a timesource.
Possible reasons for this are:
  You're running with Speedstep,






#####
#
# Create and partition an HD image
# 2004nov01
#
#####

# «create-hd-image»  (to ".create-hd-image")
#
# Create a sparse 1G file and use it as a HD image
# You'll need to be root for the mounts, umounts, and losetups

umount /tmp/sarge/proc
umount /tmp/sarge/dev/pts
umount /tmp/sarge
rmdir  /tmp/sarge
rm -v  /tmp/sarge.hdimg

# Create a sparse 1G file
#
python =(<<'%%%'
image = open("/tmp/sarge.hdimg", 'w')
image.truncate(1024 * 1048576L)
image.close()
%%%)

# Partition it (create a single partition on it, as big as possible)
# (find-man "fdisk")
#
# echo u:n:p:1:63:2097151:p:w:q: | tr : \\n > $EEG
echo u:n:p:1:63::p:w:q: | tr : \\n > $EEG
eeg fdisk /tmp/sarge.hdimg

# (find-man "losetup")
losetup -o $[63*512] /dev/loop7 /tmp/sarge.hdimg
mke2fs -F /dev/loop7
losetup -d /dev/loop7

umount /tmp/sarge
mkdir  /tmp/sarge
mount -o loop,offset=$[63*512] /tmp/sarge.hdimg /tmp/sarge

#




#####
#
# copy qemu's linux.img to sarge.hdimg
# 2004nov01
#
#####

# «linux.img-to-sarge.hdimg»  (to ".linux.img-to-sarge.hdimg")
#
umount /tmp/linux-test/linuximg
rm -Rv /tmp/linux-test/
tar -C /tmp/ \
  -xvzf $S/http/fabrice.bellard.free.fr/qemu/linux-test-0.5.1.tar.gz
cd  /tmp/linux-test/

#
cd     /tmp/linux-test/
umount /tmp/linux-test/linuximg
rm -Rv /tmp/linux-test/linuximg/
mkdir  /tmp/linux-test/linuximg/
mount -o ro,loop /tmp/linux-test/linux.img /tmp/linux-test/linuximg/
cd     /tmp/linux-test/linuximg/

#
cd     /tmp/linux-test/linuximg/ && \
cp -av * /tmp/sarge/

#




#####
#
# swap files for linux
# 2004nov01
#
#####

# «swap-files»  (to ".swap-files")
#
swapoff   /tmp/swapfile
rm -v     /tmp/swapfile

#
# (find-man "1 dd")
# (find-man "8 mkswap")
# (find-man "8 mkswap" "dd if=/dev/zero")

dd bs=1M count=512 if=/dev/zero of=/tmp/swapfile
chmod 600 /tmp/swapfile
mkswap    /tmp/swapfile
swapon    /tmp/swapfile
cat /proc/swaps

#



#####
#
# patching the Linux kernel for qemu-fast
# 2004dec02
#
#####

# «patch-linux-qemu-fast»  (to ".patch-linux-qemu-fast")
# (find-udw3m  "qemu/qemu-doc.html")
# (find-sh "unhtml < /usr/share/doc/qemu/qemu-doc.html" "9000")
# (find-k24file "include/asm/page.h"    "#define __PAGE_OFFSET")
# (find-k24file "include/asm/fixmap.h"  "#define FIXADDR_TOP")
# (find-k24file "arch/i386/vmlinux.lds" ". = 0x")
# (find-angg ".zshrc" "mydiff")
#
cd ~/bigsrc/kernel-source-2.4.26/

patch -p0 include/asm/page.h <<'%%%'
81c81
< #define __PAGE_OFFSET		(0xC0000000)
---
> #define __PAGE_OFFSET		(0x90000000)
%%%

patch -p0 arch/i386/vmlinux.lds <<'%%%'
9c9
<   . = 0xC0000000 + 0x100000;
---
>   . = 0x90000000 + 0x100000;
%%%

patch -p0 include/asm/fixmap.h <<'%%%'
104c104
< #define FIXADDR_TOP	(0xffffe000UL)
---
> #define FIXADDR_TOP	(0xa7ffe000UL)
%%%

# Not tested
#





#####
#
# qemu bug
# 2004dec23
#
#####

# «rom-bug»  (to ".rom-bug")
# Bug: sarge's qemu doesn't work with the current (sarge's) bochs bioses.
# Bug report: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=281202
# Currently installed versions: (find-dpkg-l)
# Solution: download sid's versions of bochsbios and vgabios from
#     http://ftp.debian.org/debian/pool/main/b/bochs/
# and http://ftp.debian.org/debian/pool/main/v/vgabios/
# and install them by hand.
#
cd $S/http/ftp.debian.org/debian/pool/main/
dpkg -i b/bochs/bochsbios_2.1.1+20041109-2_all.deb \
        v/vgabios/vgabios_0.4c+20041014-1_all.deb
#




#####
#
# notes about TUN/TAP
# 2004dec24
#
#####

# «tun-tap»  (to ".tun-tap")
# (find-udw3m "qemu/qemu-doc.html")
# (find-udw3m "qemu/qemu-doc.html" "Using tun/tap network interface")
# (find-man "1 qemu")
# (find-man "1 qemu" "-n script" "TUN/TAP")
# (find-fline "/etc/qemu-ifup")

# (find-k26confvar "CONFIG_TUN")
# (find-k26docfile "networking/tuntap.txt")
# (find-k26file ".files.chS.used")

# (find-k26file "drivers/net/tun.c")
# (find-k26file "drivers/net/tun.c" "tun_chr_ioctl")
# (find-k26file "include/linux/if_tun.h")
# (find-k26file "include/linux/if_tun.h" "TUNSETIFF")

# Adjust the permissions:
# (find-fline "/dev/net/")
# Default: crw-------  1 root root 10, 200 Dec 23 13:15 tun
# Edrx:    crw-rw-rw-  1 root root 10, 200 Dec 23 13:15 tun
# Default: (find-sh0 "sudo chmod 600 /dev/net/tun")
# Edrx:    (find-sh0 "sudo chmod 666 /dev/net/tun")




#####
#
# Making qemu use a fake qemu-ifup script to setup tun/tap
# 2004dec24
#
#####

# «fake-qemu-ifup»  (to ".fake-qemu-ifup")

# Contents of: (find-fline "/etc/qemu-ifup")
#!/bin/sh
sudo -p "Password for $0:" /sbin/ifconfig $1 172.20.0.1

#
# Unpack Fabrice Bellard's linux.img and bzImage-2.4.21
#
umount /tmp/linux-test/linux
rm -Rv /tmp/linux-test/
tar -C /tmp/ \
  -xvzf $S/http/fabrice.bellard.free.fr/qemu/linux-test-0.5.1.tar.gz

#
# Prepare a fake qemu-ifup script just to understand how the
# real qemu-ifup is being called
#
cat > /tmp/my-qemu-ifup <<'%%%'
#!/bin/sh
# sudo -p "Password for $0:" /sbin/ifconfig $1 172.20.0.1
sudo /sbin/ifconfig $1 172.20.0.1
date         >> /tmp/o
echo - $0 $* >> /tmp/o
%%%
chmod 755 /tmp/my-qemu-ifup
echo -n > /tmp/o
chmod 666 /tmp/o

#
# Invoke qemu using the fake qemu-ifup script
#
cd /tmp/linux-test/
qemu -m 64 -isa -n /tmp/my-qemu-ifup \
  -monitor stdio \
  -kernel bzImage-2.4.21 \
  -append "root=/dev/hda" linux.img

#
# Note: if (find-fline "/tmp/my-qemu-ifup")
# has no real ifconfig command then while qemu is running we should be
# able to do this in a shell:
#
sudo ifconfig tun0 172.20.0.1

# This is a good sign when gets said by qemu:
#   "Connected to host network interface: tun0"

# This is a bad sign when said by the guest kernel:
#   "SIOCSIFADDR: No such device"
#   "eth0: unknown interface: No such device"
# Sometimes the solution is to try with "-isa".
# (find-man "1 qemu" "\n       -isa")



 
#####
#
# tomsrtbt
# 2004dec23
#
#####

# «tomsrtbt»  (to ".tomsrtbt")
# http://www.tux.org/pub/distributions/tinylinux/tomsrtbt/tomsrtbt-2.0.103.tar.gz
#
# Unpack the .tar.gz containg the 1.772 floppy image and other stuff
# (code-c-d "tomsrtbt" "~/usrc/tomsrtbt-2.0.103/")
# (find-tomsrtbtfile "")
# (find-tomsrtbtfile "tomsrtbt.FAQ" "telnet telnetd")
# (find-tomsrtbtfile "tomsrtbt.FAQ" "Virtually all 1.44 drives support 1.722")

rm -Rv ~/usrc/tomsrtbt-2.0.103/
mkdir 
tar -C ~/usrc/ -xvzf \
$S/http/www.tux.org/pub/distributions/tinylinux/tomsrtbt/tomsrtbt-2.0.103.tar.gz

#
# Mount the floppy image.
# Where is the linux fs? It's not in a file, we'll need to dd it out...
# (find-fline "/tmp/toms/")
# (find-fline "/tmp/toms/rc.custom.gz")
# (find-fline "/tmp/toms/settings.s")
cd
sudo umount /tmp/toms/
rm -Rv      /tmp/toms/
mkdir       /tmp/toms/
sudo mount -o ro,loop ~/usrc/tomsrtbt-2.0.103/tomsrtbt.raw /tmp/toms/
cd          /tmp/toms/

#
# Save the files in /tmp/toms/ so that it will be easier to work with
# them later. The sudo is needed because there is a 600 root:root file
# (find-fline "/tmp/toms/")
# (find-tomsrtbtfile "")

cd /tmp/toms/
sudo tar -cvz * > ~/usrc/tomsrtbt-2.0.103/tomsrtbt.fs1.tar.gz

#
# Test that the bz2bzImage is really a linux kernel image
# The last lines are just to make qemu happy
qemu -monitor stdio \
     -kernel /tmp/toms/bz2bzImage \
     -m 64 -isa -n /tmp/my-qemu-ifup \
     -hda /tmp/linux-test/linux.img

#
# Emulate booting from the tomsrtbt floopy
cd ~/usrc/tomsrtbt-2.0.103/
qemu -m 64 -isa -n /etc/qemu-ifup \
  -monitor stdio \
  -fda tomsrtbt.raw

#
# Extract the ramdisk image
# Settings taken from: (find-tomsrtbtfile "settings.s" "#AUTO#")
#
NM=tomsrtbt
PL=103
RZ=2380
RI=618
Z1=1682
OA=841
O1=867
O2=3443
Z2=1709
FR=53

# (find-tomsrtbtfile "")
# (find-tomsrtbtfile "unpack.s" "skip=")
# (find-tomsrtbtfile "settings.s" "RD=/dev/ram5")
cd ~/usrc/tomsrtbt-2.0.103/
NU=/dev/null
RAW=tomsrtbt.raw
 RD=tomsrtbt.fs2.raw
# dd bs=1k if=$1 skip=$O1 2>$NU|dd count=$Z2 2>$NU|bzip2 -d 2>$NU >$RD
dd bs=1k if=$RAW skip=$O1 2>$NU|dd count=$Z2 2>$NU|bzip2 -d 2>$NU >$RD

#
# Mount tomsrtbt's ramdisk image
cd ~/usrc/tomsrtbt-2.0.103/
sudo umount /tmp/tomsfs/
rm -Rv      /tmp/tomsfs/
mkdir       /tmp/tomsfs/
sudo mount -o loop tomsrtbt.fs2.raw /tmp/tomsfs
cd          /tmp/tomsfs/

#
# Pack the files from the ramdisk image for easier access later
# (find-fline "/tmp/tomsfs/")
# (find-tomsrtbtfile "")
cd /tmp/tomsfs/
tar -cvzf ~/usrc/tomsrtbt-2.0.103/tomsrtbt.fs2.tar.gz *

#
# Umount the images
cd
sudo umount /tmp/toms/
sudo umount /tmp/tomsfs/

#



#####
#
# qemu from the upstream sources
# 2004dec14
#
#####

# http://fabrice.bellard.free.fr/qemu/
# http://fabrice.bellard.free.fr/qemu/qemu-0.6.1.tar.gz
# http://packages.debian.org/src:qemu
# http://ftp.debian.org/debian/pool/main/q/qemu/qemu_0.6.1-1.dsc
#
rm -Rv ~/usrc/qemu-0.6.1/
tar -C ~/usrc/ -xvzf $S/http/fabrice.bellard.free.fr/qemu/qemu-0.6.1.tar.gz
cd     ~/usrc/qemu-0.6.1/
./configure	|& tee oc
make		|& tee om

#
# (code-c-d "qemu" "~/usrc/qemu-0.6.1/")
# (find-qemufile "")


sh-2.05b# route
Kernel IP routing table
Destination  Gateway  Genmask     Flags Metric Ref Use Iface
172.20.0.0   *        255.255.0.0 U     0      0     0 eth0
sh-2.05b# ping 10.0.2.2
connect: Network is unreachable
sh-2.05b#

sh-2.05b# ifconfig
eth0      Link encap:Ethernet  HWaddr 52:54:00:12:23:56
          inet addr:172.20.0.2  Bcast:172.20.255.255  Mask:255.255.0.0
(...)


sendkey w
sendkey ctrl-a
sendkey ctrl-e
# not working in linux.img:
sendkey ctrl-c

ctrl-alt-1 and ctrl-alt-2 in


# (find-fline "/tmp/tomsfs/")
# (find-tomsrtbtfile "")

# (find-tomsrtbtfile "buildit.s")







# (find-fline "/tmp/toms/")
# (find-fline "/tmp/toms/rc.custom.gz")
# (find-fline "/tmp/toms/settings.s")

cd /tmp/linux-test/
qemu -m 64 -isa -n /tmp/my-qemu-ifup \
  -monitor stdio \
  -kernel /tmp/toms/bz2bzImage \
  -append "root=/dev/hda" linux.img

cd ~/usrc/tomsrtbt-2.0.103/
qemu -m 64 -isa -n /tmp/my-qemu-ifup \
  -monitor stdio \
  -fda tomsrtbt.raw


kernel /tmp/toms/bz2bzImage \
  -append "root=/dev/hda" linux.img

# (find-htetfile "Ethernet-HOWTO.gz")

# (find-man "1 qemu" "-append")
# (find-udfile "qemu/README.Debian")



#####
#
# Talking to a qemu'ed system through the emulated ethernet
# 2004dec23
#
#####

# «qemu-net»  (to ".qemu-net")



# (find-man "1 qemu")
# (find-man "1 qemu" "\n       -n script")
# (find-man "1 qemu" "\n       -redir")
# (find-man "1 qemu" "\n       -dummy-net")
# (find-man "1 qemu" "-append")
# (find-udfile "qemu/README.Debian")
# (find-udw3m  "qemu/qemu-doc.html")
# (find-udw3m  "qemu/qemu-doc.html" "`-monitor dev'")
# (find-udw3m  "qemu/qemu-tech.html")

# (find-fline "/etc/qemu-ifup")





#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
