


#####
#
# main loop
# 2004oct15
#
#####

# (find-node "(gawk)Next Statement")
#
awk '
  function has (char) { printf "line %d: \"%s\" has \"%s\"\n", NR, $0, char }
  /a/ { has("a") }
  /b/ { has("b"); print "Running \"next\""; next }
  /c/ { has("c") }
' <<'%%%'
a
b
ab
abc
%%%

#




####
#
# argv
# 2004mar18
#
####

# «ARGV»  (to ".ARGV")
# «ARGC»  (to ".ARGC")
# (find-gawknode "")
# (find-gawknode "Auto-set" "`ARGC, ARGV'")
# (find-gawknode "Auto-set" "`ENVIRON'")
# (find-gawknode "Other Arguments")
# (find-gawknode "Auto-set" "`ARGIND #'")
#
awk 'BEGIN { for (i=0; i<ARGC; i++) print i, ARGV[i] }' arg1 arg2

#
unset FOO;    gawk 'BEGIN { print ENVIRON["FOO"] }'
FOO=bar       gawk 'BEGIN { print ENVIRON["FOO"] }'
FOO="baz baz" gawk 'BEGIN { print ENVIRON["FOO"] }'

#
rm -Rv /tmp/awk/
mkdir  /tmp/awk/
cd     /tmp/awk/

echo FOO > foo
echo BAR > bar

awk ' BEGIN { print "ARGC =", ARGC; for(i=0;i<ARGC;i++) print i,ARGV[i] }
      { print }
  ' foo bar 

echo input from stdin | \
awk ' BEGIN { for(i=0;i<ARGC;i++) print i,ARGV[i]; ARGC=2 }
      { print }
  ' foo bar

#





#####
#
# getline
# 2004oct13
#
#####

# «getline»  (to ".getline")
# (find-node "(gawk)Plain Getline")
# (find-node "(gawk)Getline/Variable/File")
#
rm -Rv /tmp/awk/
mkdir  /tmp/awk/
cd     /tmp/awk/
cat > myfile <<'%%%'
one
two two

four four four four
%%%

awk 'BEGIN {
  do {
    a = (getline line < "myfile")
    print a, line
  } while (a)
}'

awk 'BEGIN {
  while (getline line < "myfile")
    print line
  print "last line:", line
}'

awk 'BEGIN {
  getline first_line < "myfile"
  while (getline last_line < "myfile") {}
  print "first line:", first_line
  print "last line: ", last_line
}'
#







#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
