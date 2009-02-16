#!/usr/bin/perl
#!/usr/local/bin/perl
#require "/home/sn/bin/jcode.pl";
require 'getopts.pl';
&Getopts('l:bfx') || die "Usage: $0 [-bfx] [-l num] (all ignored)\n";
# All options ignored not to take them as filename(s).
$opt_b && ($nobsatall = 1);
$opt_f && ($halflinefeed = 1);
$opt_x && ($untabify = 1);
$opt_l && ($lbuf = $opt_l);

while (<>) {
#    $code = &jcode'convert(*_, 'euc');
# zenkaku first
#    s/([^\10])\10([^\10])/\1/g;
    s/__\10\10(..)/\1/g;
    s/(..)\10\10(..)/\1/g;
# alphabets
#    s/([^\010])\010([^\010])/\1/g;
    s/_\10(.)/\1/g;
    s/(.)\10(.)/\1/g;
    print $_;
}
