



apt-get install subversion
apt-get install subversion-tools

# (find-status   "subversion")
# (find-vldifile "subversion.list")
# (find-udfile   "subversion/")
# (find-status   "subversion-tools")
# (find-vldifile "subversion-tools.list")
# (find-udfile   "subversion-tools/")




#####
#
# a local svn server with repository in /svn
# (thx to otavio salvador)
# 2004oct29
#
#####

# (find-fline "/var/lib/svn/")
# (find-fline "/svn/")
# (find-fline "~/tmp/svn/")

#
rm -Rv /var/lib/svn /svn

#
# (find-man "1 svnadmin")
# (find-sh  "svnadmin help")
# (find-sh  "svnadmin help create")

mkdir /var/lib/svn
cd /
ln -s /var/lib/svn .
cd /svn/
svnadmin create teste

#
rm -Rv   ~/tmp/svn/

#
# (find-man "1 svn")
# (find-sh  "svn help")
# (find-sh  "svn help co")
# (find-sh  "svn help add")
# (find-sh  "svn help ci")

mkdir -p ~/tmp/svn/
cd       ~/tmp/svn/

# Here svn asks for a password - but which password?
# When I'm root the root password works.
svn co svn+ssh://localhost/svn/teste teste
cd teste
touch foo
svn add foo
svn ci -m'Adding foo.'

#




#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
