#!/usr/bin/perl

# Convert mIRC's servers.ini to a elisp structure.
# Get the file from http://www.mirc.co.uk/servers.ini

# TODO: We need a separate file where we can store regexp/functions
# to match a erc-announced-server-name against a erc-networks-alist entry.
# For sanity, we'df need a lookup procedure here, and insert the
# regexp/function instead of the nil value in the erc-networks-alist.
#
# There is a error somehow in the servers.ini. There are two
# undernets, undernet and Undernet. They are the same though.
# So we'd need to unify them somehow (change case)?

print "(defcustom erc-server-alist\n'(\n";
while (<>) {
    if(($sname, $shost, $sport, $grp) = $_ =~ /^n\d+=(.*)SERVER:(.*):(.*)GROUP:(.*)/){
  $groups{$grp}+=1;
  @ports = split ",", $sport;
  print "  (\"$sname\" '$grp \"$shost\" ";
  if ($#ports==0) {
      if (($p1,$p2)=$sport=~/(\d+)-(\d+)/) {
	  print "(($p1 $p2))";}
      else {
	  print $sport;
      }
  }
  else {
      print "(";
      foreach $port (@ports) {
	  if (($p1,$p2)=$port=~/(\d+)-(\d+)/) {
	      print "($p1 $p2) ";}
	  else {
	      print $port." ";
	  }
      }
      print ")";
  }
  print ")\n";
}
}
print ")\n  \"Server Alist\"\n  :group 'erc\n  :type 'sexp)\n\n";

print "(defcustom erc-networks-alist\n'(\n";
foreach $grp (sort(keys(%groups))) {
    print "  ($grp nil)\n";
}
print ")\n  \"Network alist\"\n  :group 'erc\n  :type 'sexp)\n";
