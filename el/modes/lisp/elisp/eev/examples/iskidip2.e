# Internet Skills for Disconnected People: iskidip2.e
# This file is a close friend of iskidip.e.
# Currently it is mostly broken.
# This one contains the top-level blocks, with many red-star lines for
# F9. Most of these lines invoke "subroutines" that are in iskidip.e.
# Author:  Eduardo Ochs <edrx@mat.puc-rio.br>
# Version: 2005sep05 5:04
#
# See also: (find-eevexfile "iskidip.e")


(setq ee-shadow-file "$EEVE/iskidip.e")

(defun find-shadow (&rest rest)
  (interactive)
  (apply 'find-anchor ee-shadow-file rest))

(defun eevnow-at-shadow (anchor)
  (interactive "sAnchor: ")
  (if ee-arg (find-shadow anchor)
    (eevnow-at-file ee-shadow-file anchor)))

;; (find-eevex "iskidip.e" "bbinitrd-qemu-main")

% (find-eimage0 "./iskidip.png")



# (find-eevex "iskidip.e" "bbinitrd-qemu-main")


#####
#
# Preparations that only need to be done once
# 2005sep15
#
#####

 (eechannel-xterm "A")

 ;; Compiling busybox.
 ;; This uses a dir inside ~/usrc/ and so it is preserved through reboots.
 ;; NOTE: busybox-compile takes about one minute.

 (eevnow-at-shadow "busybox-config")
 (eevnow-at-shadow "busybox-compile")
 (eevnow-at-shadow "busybox-tags")

 ;; Preparing a tgz with the modules that will go into our initrd.
 ;; This needs to be done at each reboot, as the tgz is stored in /tmp/.
sudo ~/run-zsh
 (eevnow-at-shadow "dinitrd-umount")
 (eevnow-at-shadow "dinitrd-cp-img")
 (eevnow-at-shadow "dinitrd-mount")
 (eevnow-at-shadow "dinitrd-pack")
 (eevnow-at-shadow "dinitrd-unpack")
cp -v --parents \
  /lib/modules/2.6.8-1-386/kernel/drivers/net/8390.ko \
  /lib/modules/2.6.8-1-386/kernel/drivers/net/ne2k-pci.ko \
  /lib/modules/2.6.8-1-386/kernel/drivers/net/plip.ko \
  /lib/modules/2.6.8-1-386/kernel/drivers/parport/parport_pc.ko \
    /tmp/dinitrdfiles/
 (eevnow-at-shadow "dinitrd-rm-some-modules")
 (eevnow-at-shadow "dinitrd-pack-modules")
 (eevnow-at-shadow "dinitrd-umount")
exit

 ;; In case we want to inspect the modules:
 ;; (find-fline "/tmp/" "dinitrd-modules.tgz")
 ;; (find-fline "/tmp/dinitrd-modules.tgz")
 ;; (find-sh "find /lib/modules/2.6.8-1-386/kernel/ -type f | sort")



#####
#
# Create a guest.img (an initrd image)
# 2005sep15
#
#####

 (eechannel-xterm "A")
sudo ~/run-zsh

 (eevnow-at-shadow "umount-guestfs")
 (eevnow-at-shadow "create-guestfs.img")
 (eevnow-at-shadow "mount-guestfs")
 (eevnow-at-shadow "mkdirs-guest")
 (eevnow-at-shadow "instlib-functions")
 (eevnow-at-shadow "install-busybox")
 (eevnow-at-shadow "create-devices")
 (eevnow-at-shadow "create-etcpasswdgroup")
 (eevnow-at-shadow "create-busyboxconf")
 (eevnow-at-shadow "create-etcprofile")
# This /etc/init.d/rcS is for both eth and plip, iirc
 (eevnow-at-shadow "create-etcinitdrcS")
 (eevnow-at-shadow "dinitrd-unpack-modules")

# cp -v /etc/resolv.conf /tmp/guest/etc/
grep ^nameserver /etc/resolv.conf | tee /tmp/guest/etc/resolv.conf

 (eevnow-at-shadow "create-hdaimg-sparse")
cd /tmp/
umount /tmp/guest/
cp -v /boot/vmlinuz-2.6.8-1-386    /tmp/vmlinuz

 (eechannel-xterm "A")
# Try the initrd with qemu.
# (find-man "1 qemu")
cd /tmp/
qemu -hda    /tmp/hda.img  \
     -kernel /tmp/vmlinuz  \
     -monitor stdio -m 64  \
     -pci                  \
     -initrd /tmp/guest.img

 ;;
 ;; Telnet to mistletoe
 ;;
 (eechannel-xterm "B")
telnet 172.20.0.2
root
root

hostname
hostname mistletoe
route
route add default gw 172.20.0.1 eth0
route
exit

traceroute angg.twu.net

traceroute angg.twu.net
traceroute localhost   
traceroute 127.0.0.1

# (find-fline "/etc/hosts")
echo 

cat > /tmp/guest/etc/hosts <<'%%%'
127.0.0.1 localhost loopback mistletoe
172.20.0.1 persephone
%%%



(find-fline "/telnet:root@172.20.0.2:/")



route
wget -O - http://64.246.24.15/
wget -O - http://172.20.0.1/
wget -O - http://172.20.0.1/tmp/
wget -O - http://172.20.0.1/tmp/convite_rosa

wget -O - http://angg.twu.net/thl.html
traceroute mistletoe
traceroute 172.20.0.1
traceroute persephone
wget -O - http://persephone/


host angg.twu.net

# route add default gw 10.0.1.1 eth0
route add default gw 172.20.0.0 eth0
route add default gw 172.20.0.1 eth0

# (find-node "(gdb)Connecting")


date | netcat -q 0 172.20.0.2 1234

# (find-sh0 "sudo killall -9 qemu")

 (eechannel-xterm "A")
quit




Problem:

        my machine             emulated machine
       (persephone)            (mistletoe)
     eth0        tun0          eth0       
---- a.b.c.d  172.20.0.1 ----- 172.20.0.2

how do I tell iptables that all packets arriving from mistletoe that
are not specifically for persephone should be redirected to eth0 and
sent outside?







#  Local Variables:
#  coding:               no-conversion
#  modes:                (fundamental-mode sh-mode emacs-lisp-mode)
#  ee-delimiter-hash:    "\n#\n"
#  ee-delimiter-percent: "\n%\n"
#  ee-anchor-format:     "«%s»"
#  End:
