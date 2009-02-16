



# «.dmissing»			(to "dmissing")
# «.apt-howto»			(to "apt-howto")
# «.repository-with-installed-debs»  (to "repository-with-installed-debs")





#####
#
# dmissing
# 2004oct28
#
#####

# «dmissing»  (to ".dmissing")
#
# Prepare the local files - download Contents-i386.gz and unpack it.
# This block only needs to be run once.
# Obs: some installation CDs contain a copy of Contents-i386.gz.

mkdir -p ~/snarf/http/ftp.br.debian.org/debian/dists/sarge/
cd       ~/snarf/http/ftp.br.debian.org/debian/dists/sarge/
wget           http://ftp.br.debian.org/debian/dists/sarge/Contents-i386.gz
zcat Contents-i386.gz > Contents-i386

#
# You can put this definitions in your .bashrc ou .zshrc.
function dmissing () {
  grep -h $* ~/snarf/http/ftp.br.debian.org/debian/dists/sarge/Contents-i386
}

#
# a test:
dmissing libc | grep /info/

#
# The output is a series of lines like these:
# usr/share/info/cln.info.gz                              libdevel/libcln-dev
# usr/share/info/commoncpp2.info-1.gz                     libdevel/libcommoncp
# usr/share/info/commoncpp2.info-2.gz                     libdevel/libcommoncp
# usr/share/info/commoncpp2.info-3.gz                     libdevel/libcommoncp
# usr/share/info/commoncpp2.info.gz                       libdevel/libcommoncp
# usr/share/info/libc-dir-add.info.gz                     doc/glibc-doc
# usr/share/info/libc.info-1.gz                           doc/glibc-doc
# usr/share/info/libc.info-10.gz                          doc/glibc-doc
# usr/share/info/libc.info-11.gz                          doc/glibc-doc
# usr/share/info/libc.info-12.gz                          doc/glibc-doc
# (...)
# usr/share/info/libc.info-8.gz                           doc/glibc-doc
# usr/share/info/libc.info-9.gz                           doc/glibc-doc
# usr/share/info/libc.info.gz                             doc/glibc-doc
# usr/share/info/libcdio.info.gz                          devel/libcdio-dev
# usr/share/info/zlibc.info.gz                            libs/zlibc




#####
#
# apt-howto
# 2004oct30
#
#####

# «apt-howto»  (to ".apt-howto")
# (find-status   "apt-howto-en")
# (find-vldifile "apt-howto-en.list")
# (find-udfile   "apt-howto-en/")
# (find-apthowtofile "")
# (find-apthowtofile "apt-howto.en.txt.gz")





#####
#
# A repository with the installed debs (in /var/www/vcarchives/)
# 2004sep07
#
#####

# «repository-with-installed-debs»  (to ".repository-with-installed-debs")
# «var-www-vcarchives»  (to ".var-www-vcarchives")
# (find-udfile "Debian/apt-howto/apt-howto.en.txt.gz")
# (find-udfile "Debian/apt-howto/apt-howto.en.txt.gz" "dpkg-scanpackages")
# (find-man "1 dpkg-scanpackages")

# A first exercise: a repository with the debs from /var/cache/apt/archives/
# http://127.0.0.1/
# http://127.0.0.1/vcarchives/
#
sudo rm -Rv    /var/www/vcarchives/
sudo mkdir     /var/www/vcarchives/
sudo chmod 777 /var/www/vcarchives/

#
cd /var/www/vcarchives/
mkdir     debs
cp -ilv /var/cache/apt/archives/*.deb debs/
mkdir -p       dists/sarge/main/binary-i386/
dpkg-scanpackages debs /dev/null \
  | tee        dists/sarge/main/binary-i386/Packages \
  | gzip -c9 > dists/sarge/main/binary-i386/Packages.gz

#



# Now do it seriously
#
mkdir       /tmp/cdd/
sudo umount /tmp/cdd/
sudo mount -o loop /hdb2/CDs/debian-br-cdd/sarge-i386-1.raw /tmp/cdd

#
sudo dselect update
#
# (find-angg "LUA/debs.lua")
grep-status --show-field=Package,Version --field=Status ' installed' \
  > /tmp/installed-debs

( ls /var/cache/apt/archives/*.deb
  find /tmp/cdd/ -name '*.deb' | sort
  ls /hdb2/CDs/cdd-extra/*.deb
)  > /tmp/.debs

lua50 ~/LUA/debs.lua /tmp/installed-debs /tmp/.debs \
  | sort > /tmp/currentdebs

# (find-fline "/tmp/currentdebs")

#
sudo rm -Rv    /var/www/vcarchives/
sudo mkdir     /var/www/vcarchives/
sudo chmod 777 /var/www/vcarchives/
#
rm -Rfv /var/www/vcarchives/debs
rm -Rfv /var/www/vcarchives/dists
cd      /var/www/vcarchives/
mkdir    debs
mkdir -p dists/sarge/main/binary-i386/
#
cd      /var/www/vcarchives/
for i in $(grep -v '#' /tmp/currentdebs); do
  cp -il $i debs/ || cp -i $i debs/
done

cd /var/www/vcarchives/debs/
ls *.deb | lua50 -e '
  for str in io.lines() do
    newstr, n = string.gsub(str, "[0-9]%%3a", "")
    if n>0 then print("mv -v " .. str .. " " .. newstr) end
  end
' | sh

cd /var/www/vcarchives/
dpkg-scanpackages debs /dev/null \
  | tee        dists/sarge/main/binary-i386/Packages \
  | gzip -c9 > dists/sarge/main/binary-i386/Packages.gz

#
cd /var/www/vcarchives/dists/sarge/
cat > main/binary-i386/Release <<'%%%'
Archive: sarge
Component: main
Origin: Debian
Label: Debian
Architecture: i386
%%%

# (find-fline "/usr/bin/apt-move" "exec > Release")
# (find-fline "/usr/bin/apt-move" "get_checksum()")
# (find-node "(libc)Table of Output Conversions")
# (find-fline "/tmp/cdd/dists/sarge/Release")
# (find-fline "/var/www/vcarchives/dists/sarge/Release")

cd /var/www/vcarchives/dists/sarge/
(
echo Origin: Debian
echo Label: Debian
echo Suite: testing
echo Codename: sarge
echo Date: $(TZ=UTC date '+%a, %d %b %Y %T %Z')
echo Architectures: i386
echo Components: main
echo Description: Persephone sub-Sarge
echo MD5Sum:
for l in main/binary-i386/{Release,Packages.gz,Packages}; do
  printf ' %32s %8d %s\n' \
    $(md5sum $l | cut -d ' ' -f 1) $(stat -c %s $l) $l
done
) | tee Release

#
# (find-fline "/var/lib/apt/lists/")

deb http://127.0.0.1/vcarchives sarge main

# (find-man "dpkg-scanpackages")
# (find-status   "dpkg-dev")
# (find-vldifile "dpkg-dev.list")
# (find-udfile   "dpkg-dev/")





#####
#
# debootstrap
# 2004sep04
#
#####

# «debootstrap»  (to ".debootstrap")
# (find-status   "debootstrap")
# (find-vldifile "debootstrap.list")
# (find-udfile   "debootstrap/")
# (find-udfile "debootstrap/README.Debian")
# (find-fline "/usr/lib/debootstrap/")
# (find-fline "/usr/lib/debootstrap/functions")
# (find-fline "/usr/lib/debootstrap/pkgdetails")
# (find-fline "/usr/lib/debootstrap/scripts/")
# (find-fline "/usr/lib/debootstrap/scripts/sarge")
# (find-fline "/usr/lib/debootstrap/scripts/sarge" "Fake start-stop-daemon")
# (find-fline "/usr/lib/debootstrap/scripts/sarge.buildd")
# (find-fline "/usr/sbin/debootstrap")

# (find-man "8 debootstrap")
#
rm -Rv /sarge-root

#
mkdir  /sarge-root
mount  /dev/hdb5 /sarge-root
rm -Rv /sarge-root/*
rm -Rv /sarge-root/.*

#
# Installation using debootstrap and http
cd /
mkdir /sarge-root
debootstrap sarge /sarge-root http://127.0.0.1/vcarchives/

#
# Pack the things that were downloaded by http
# (find-fline "/sarge-root/")
# (find-fline "/sarge-root/var/lib/apt/lists/")
# (find-fline "/sarge-root/var/cache/apt/archives/")
cd /sarge-root/
tar -cvf /tmp/sarge-debs.tar \
  var/lib/apt/lists/ \
  var/cache/apt/archives/

#
# (find-bashnode "Shell Parameter Expansion" "`${PARAMETER#WORD}'")
# (find-bashnode "Shell Parameter Expansion" "`${PARAMETER%WORD}'")
# (find-fline "/usr/sbin/debootstrap" "if [ \"$UNPACK_TARBALL\" ]; then")
# (find-fline "/usr/lib/debootstrap/functions")
rm -Rv /sarge-root2
mkdir  /sarge-root2
cd     /sarge-root2
tar -C /sarge-root2 -xvf /tmp/sarge-debs.tar
# download the whole system:
debootstrap sid /sarge-root2 http://ftp.br.debian.org/debian/

#
# Check that the two directories have the same files:
cd /sarge-root/;  (sudo find) | sort > /tmp/ofind
cd /sarge-root2/; (sudo find) | sort > /tmp/ofind2
diff /tmp/ofind /tmp/ofind2

#
# (find-man "8 debootstrap" "apt-setup")
sudo mount -t proc sarge-root-proc /sarge-root/proc
eeg -f <(<<'%%%'
EDITOR=nano apt-setup
nano /etc/apt/sources.list
# deselect/apti?
%%%
) sudo chroot /sarge-root /bin/bash

#


apt-get install emacs21 zsh lua50 less sudo
apt-get install erc w3m-el
apt-get install expect ssh
apt-get install emacs21-el elisp-manual emacs-lisp-intro
apt-get install dict dictd dict-gcide dict-wn dict-jargon
apt-get install zgv
apt-get install dict-freedict-eng-por
apt-get install build-essential

apt-get install gpm
gpmconfig
# (find-file "/etc/gpm.conf")
# repeat_type=raw

apt-get remove xdm gdm
# now kill X by hand, then:
/etc/init.d/gpm restart

apt-get install fvwm




#
echo "proc-sid /sarge-root/proc proc none 0 0" >> /etc/fstab
mount proc-sid /sarge-root/proc -t proc
cp /etc/hosts /sarge-root/etc/hosts
chroot /sarge-root /bin/bash

#
# (find-fline "/etc/fstab")
echo "proc-sid /sarge-root2/proc proc none 0 0" >> /etc/fstab
mount proc-sid /sarge-root2/proc -t proc
cp /etc/hosts /sarge-root/etc/hosts
chroot /sarge-root /bin/bash

#
# set-up /etc/apt/sources.list:
apt-setup
# point the source to unstable:
vi /etc/apt/sources.list
chroot # dselect
[ you may use aptitude, install mc and vim ... ]

 main # echo "8:23:respawn:/usr/sbin/chroot /sarge-root " \
        "/sbin/getty 38400 tty8"  >> /etc/inittab
[ define a login tty that will use this system ]
 main # init q
[ reload init ]






#####
#
# deboostrap on /sarge-root
# 2004sep13
#
#####

# «debootstrap-quick»  (to ".debootstrap-quick")
#
umount /sarge-root/proc
umount /sarge-root
rm -Rv /sarge-root

#
mkdir /sarge-root
mount /dev/hdb5 /sarge-root
mkdir /sarge-root/proc/
mount -t proc sarge-proc /sarge-root/proc

#
rm -Rv /sarge-root/*
rm -Rv /sarge-root/.*

#
# Installation using debootstrap and http
cd /
mkdir /sarge-root
echo "mini-sarge for livecds" > /sarge-root/.id
debootstrap sarge /sarge-root http://127.0.0.1/vcarchives/
(cd /sarge-root/etc/apt/; mv -v sources.list sources.list.orig)

#
# (find-fline "/sarge-root/etc/apt/")
# (find-fline "/sarge-root/etc/apt/sources.list")

cat > /sarge-root/etc/apt/sources.list <<'%%%'
# (find-es "livecd" "debootstrap-quick")
deb http://127.0.0.1/vcarchives sarge main
%%%

#
sudo chroot /sarge-root \
  /usr/bin/apt-get update
sudo chroot /sarge-root \
  /usr/bin/apt-get install emacs21 zsh lua50 less sudo

#
# (find-man "dpkg-reconfigure")
sudo chroot /sarge-root \
  dpkg-reconfigure --all

#





# (find-es "livecd" "apt-get-autoclean")
# (find-es "livecd" "var-www-vcarchives")
# (find-es "kernel" "kernel-source-2.4.26")


# (find-status   "debian-policy")
# (find-vldifile "debian-policy.list")
# (find-udfile   "debian-policy/")

# (code-c-d "dpol" "/usr/share/doc/debian-policy/")
# (find-dpolfile "")
# (find-dpolfile "policy.txt.gz" "<postinst> `configure'")
# (find-dpolfile "policy.txt.gz" "    <postinst> configure")





#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
