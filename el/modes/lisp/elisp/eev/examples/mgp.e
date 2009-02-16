

(find-es "mgp")


# (find-status   "mgp")
# (find-vldifile "mgp.list")
# (find-udfile   "mgp/")

# (find-udfile "mgp/README.lang" "charset")
# (find-udfile "mgp/SYNTAX.gz" "charset")
# (find-udfile "mgp/changelog.Debian.gz" "charset")
# (find-udfile "mgp/changelog.gz" "charset")

# Home page:
# http://www.mew.org/mgp/
# http://member.wide.ad.jp/wg/mgp/



# (find-man "1 mgp")


# http://ftp.debian.org/debian/pool/main/m/mgp/mgp_1.11b.orig.tar.gz
# (code-c-d "mgp" "~/usrc/magicpoint-1.11b/")
# (find-mgpfile "")
#
A=( debhelper cpp devscripts xutils flex bison pkg-config libttf-dev
libxft2-dev libpng12-0-dev libmng-dev xlibs-dev sharutils zlib1g-dev
libungif4-dev imlib11-dev libtiff4-dev libm17n-devy libfontconfig1-dev
)
apti $A

#
rm -Rv ~/usrc/magicpoint-1.11b/
tar -C ~/usrc/ -xvzf \
  $S/http/ftp.debian.org/debian/pool/main/m/mgp/mgp_1.11b.orig.tar.gz
cd ~/usrc/magicpoint-1.11b/
./configure    |& tee oc
xmkmf          |& tee ox
make Makefiles |& tee omM
make           |& tee om

#
	% ./mgp sample/sample.mgp


rm -Rv ~/usrc/magicpoint-1.11b/




#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
