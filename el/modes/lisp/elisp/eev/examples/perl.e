

# I just pasted some very old notes here...
# Edrx, 2004oct29

# «.perl-nle»		(to "perl-nle")
# «.perldb»		(to "perldb")
# «.Getopt::Std»	(to "Getopt::Std")
# «.argv»		(to "argv")
# «.argc»		(to "argc")
# «.Data::Dumper»	(to "Data::Dumper")
# «.Net::NNTP»		(to "Net::NNTP")
# «.reading_a_file_at_once»  (to "reading_a_file_at_once")
# «.eval_in_regsub»	(to "eval_in_regsub")
# «.filter»		(to "filter")




# (find-status   "perl-doc")
# (find-vldifile "perl-doc.list")
# (find-udfile   "perl-doc/")
# (find-man "1 perl")
# (find-man "1 perltoc")
# (find-man "1 perlsyn")
# (find-man "1 perldata")
# (find-man "1 perlop")
# (find-man "1 perlfunc")
# (find-man "1 perlfunc" "print LIST")

# (find-man "1 perlintro")




#####
#
# perl -nle
# 2000dec26
#
#####

# «perl-nle»  (to ".perl-nle")
# (find-man "1 perlrun")
# (find-man "1 perlrun" "    -n")
# (find-man "1 perlrun" "    -l[octnum]")
# (find-man "1 perlrun" "    -e commandline")
# (find-man "1 perlrun" "| perl -nle")
#
perl -e  'print 2+3, "\n"'
perl -le 'print 2+3'
#
ls /tmp | perl -nle '$n++; print "$n: \"$_\""'
ls /tmp | perl -nle 'print'
ls /tmp | perl -nle 'printf "\"%s\"\n", $_'
ls /tmp | perl -le 'while (<>) { printf "($_)" }'
ls /tmp | perl -e 'while (<>) { print "($_)" }'
#
# (find-man "perlop" "The null filehandle <>")
# Use <STDIN> when you don't want the ARGV trick.





#####
#
# perldb
# 2000nov17
#
#####

# «perldb»  (to ".perldb")
# (find-man "perlfaq3")
# (find-man "perlfaq3" "perl -de 42")
# (find-man "perldebug")
# (find-pl5podfile "")
# (find-pl5podfile "perlfaq3.pod" "How can I use Perl interactively?")
# (find-pl5podfile "perldebug.pod")
# (eeman "perlfaq3" "interactively")
# (eeman "perldebug")
# (find-pl5file "5.004/perl5db.pl")
# (find-pl5file "5.005/perl5db.pl")

#
cat > $EEG <<'---'
h

h h
|h h
---
eeg perl -de 42

#

# (perldb "perl /usr/sbin/update-alternatives --display wish")
# (find-angg ".emacs" "gdbk-mode")
#
# (gdbk-perldb t "/usr/sbin/update-alternatives --display wish" "" t)
#
# (gdbk-perldb nil "/usr/sbin/update-alternatives --display wish")





#####
#
# Getopt::Std
# 2000may08
#
#####

# «Getopt::Std»  (to ".Getopt::Std")
# (eeman "Getopt::Std")
#
cat > /tmp/p <<'---'
  use Getopt::Std;
  use Net::SMTP;
  getopt('s');
  print ":$opt_s:$ARGV[0]:\n"
---
perl /tmp/p -s 'Foo' Bar
perl /tmp/p -s 'Foo'
perl /tmp/p          Bar
perl /tmp/p
#
# Options after the straight args don't work:
perl /tmp/p Bar -s 'Foo'


# Were is it said that GNU's getopt accepts options after the other
# args (but before "--")? Not in the obvious place:

# (find-node "(libc)Getopt")




#####
#
# argv and argc
#
#####

# «argv»  (to ".argv")
# (find-man "perlvar" "@ARGV")
# (find-man "perldata" "$#days")

#
# «argc»  (to ".argc")
# About argc:
perl -le 'print ":", @ARGV+0, ":";'
perl -le 'print ":", @ARGV+0, ":";'  an_arg
perl -le 'print ":$#ARGV:"'
perl -le 'print ":$#ARGV:";'
perl -le 'print ":$#ARGV:";' foo
perl -le 'print ":$#ARGV:";' foo bar
perl -le 'print ":$#ARGV:";' foo bar plic
#




#####
#
# sendEmail
# 2000may08
#
#####

# «sendEmail»

# (find-fline "$S/http/marvin.criadvantage.com/caspian/Software/SendEmail/sendEmail-v1.20.tar.gz")

rm -Rv /usr/src/sendEmail/
cd /usr/src/
tar -xvzf $S/http/marvin.criadvantage.com/caspian/Software/SendEmail/sendEmail-v1.20.tar.gz
cd /usr/src/sendEmail/

# (find-fline "/usr/src/sendEmail/sendEmail")
# (find-fline "/usr/src/sendEmail/sendEmail" "sub help")

cd /usr/src/sendEmail/
./sendEmail -f hahaha -t edrx -u Teste -m Hello -vv


# But instead of learning enough of this to use it to send email from
# my machine bypassing the broken exim, I decided to perfect my code
# that called Net::SMTP... and solved the problem.

# (find-es "mail" "Net::SMTP2")
# (find-fline "~/bin/sendemail")





#####
#
# Data::Dumper
# 2000may09
#
#####

# «Data::Dumper»  (to ".Data::Dumper")
# (find-man "Data::Dumper")

# (eeman "CGI")
# (eeman "CGI" "CGI.INPUTFILE.")
# (find-pl5file "5.005/Data/Dumper.pm" "=head1")
# pod2t /usr/lib/perl5/5.005/Data/Dumper.pm |& l

#
echo foo=bar > /tmp/oc
cat > /tmp/p <<'---'
  use CGI;
  open(QF, "/tmp/oc");
  $query = new CGI(QF);
  #
  use Data::Dumper;
  print Dumper('$query', $query);
  sub pdump { print "\n  $_[0] =\n", Dumper($_[1]); }
  pdump('$query', $query);
---
perl /tmp/p
#
# And this one shows some things about references.
# (eeman "perlref")

cat > /tmp/p <<'---'
  use Data::Dumper;
  sub pdump { print "\n  $_[0] =\n", Dumper($_[1]); }
  @a = ['aa', 'bb'];
  $b = \@a;
  $c = ['aa', 'bb'];
  pdump('@a', @a);
  pdump('$b', $b);
  pdump('$c', $c);
  pdump('join', join('', @$c));
---
perl /tmp/p
#





#####
#
# reading an entire file at once
# 2000oct02
#
#####

# «reading_a_file_at_once»  (to ".reading_a_file_at_once")
# (find-man "perlvar" "input record separator")
# (find-man "perlop" "null filehandle")
#
ls /tmp \
  | perl -e 'undef $/; print "AAAA:", <>, ":ZZZZ\n";'
#



#####
#
# "/e" in regsub
# 2001jan15
#
#####

# «eval_in_regsub»  (to ".eval_in_regsub")
# (find-angg ".zshrc" "save-input")
# (eeman "perlop" "abc246xyz")
# (find-pl5podfile "perlop.pod" "abc246xyz")
# (find-pl5podfile "perlfunc.pod" "=item chr\n")

# (find-man "perlop" "s/PATTERN/REPLACEMENT/egimosx")
# (find-man "perlop" "yields 'abc  246xyz'")
# (find-man "perlfunc" "   chr NUMBER")
# (find-zshnode "Shell Builtin Commands" "echo")
#
echo -ne '\r\a\n\f\t' \
  | perl -nle '
      s/[\000-\037]/"^".chr(ord($&)+64)/eg; print
    '
#




#####
#
# filter
# 2001jul11
#
#####

# «filter»  (to ".filter")
# (find-man "perlfunc" "split /")
# (find-man "perlop" "   Quote and Quote-like Operators\n")
#
# (find-angg ".zshrc" "filter")
function filter () {
  WHICH=$1 \
  perl -nle '
    if (m/;;-> (.*)/) {
      $doprint = 0;
      for $p (split(/ /, $1)) {
	for $w (split(/,/, $ENV{"WHICH"})) {
          if ($p eq $w) { $doprint = 1 }
        }
      }
    } else {
      if ($doprint) { print; }
    }
  '
}

cat > /tmp/file <<'---'
one
;;-> foo
two
;;-> bar baz
three
;;-> foo baz
four
;;-> fiv
five
---

cat /tmp/file | filter foo
cat /tmp/file | filter bar
cat /tmp/file | filter baz
cat /tmp/file | filter foo,fiv
#








/x for whitespace and comments
use re "debug"
will cause the perl regex engine to spit out lots of comments about
what it's doing
use re 'debugcolor'
perl -Mre=debugcolor -e '"hi" =~ /h([a-z]*)/'









#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
