# This is the `rcfiles/channel.tcl' file of GNU eev.
# Author and version: Eduardo Ochs, 2005jan07
# This file is in the Public Domain.

# Note: the `trap' command is from Expect:
# (find-expcommand "trap [[command] signals]")

# Usage:
#   source $env(EEVRCDIR)/channel.tcl
# (find-eevex "tcltk.e")
# This is very new (2005jan03)... Edrx

proc readfile {fname} {
  set ch [open $fname r]; set str [read $ch]; close $ch; return $str
}
proc writefile {fname str} {
  set ch [open $fname w]; puts -nonewline $ch $str; close $ch
}
proc getenv {key {defaultvalue {}}} {
  global env; expr {[info exist env($key)]?$env($key):$defaultvalue}
}
proc EEVTMPDIR {} { getenv EEVTMPDIR [getenv HOME]/eev-current/tmp }
proc EECHANNEL {} { getenv EECHANNEL tcl }
proc pidfile {} { return [EEVTMPDIR]/eeg.[EECHANNEL].pid }
proc strfile {} { return [EEVTMPDIR]/eeg.[EECHANNEL].str }
writefile [pidfile] "[pid]\n"
trap {uplevel #0  [readfile [strfile]]} USR1
