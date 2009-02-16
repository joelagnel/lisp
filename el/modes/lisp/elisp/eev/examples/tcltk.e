

(find-es "tcl")
(find-es "expect")

# «.tclbook»		(to "tclbook")
# «.single-step-wish»	(to "single-step-wish")
# «.eechannel-tcl»	(to "eechannel-tcl")
# «.channel.tcl»	(to "channel.tcl")




#####
#
# Draft of Ousterhout's book
# 2004nov07
#
#####

# «tclbook»  (to ".tclbook")
# http://wwwcs.upb.de/cs/ag-szwillus/lehre/ws96_97/bss/tcltk.html
# http://wwwcs.upb.de/cs/ag-szwillus/lehre/ws96_97/bss/book/p1.ps.gz
# http://wwwcs.upb.de/cs/ag-szwillus/lehre/ws96_97/bss/book/p2.ps.gz
# http://wwwcs.upb.de/cs/ag-szwillus/lehre/ws96_97/bss/book/p3.ps.gz
# http://wwwcs.upb.de/cs/ag-szwillus/lehre/ws96_97/bss/book/p4.ps.gz

# (code-c-d "tclbook" "$S/wwwcs.upb.de/cs/ag-szwillus/lehre/ws96_97/bss/book/")
# (code-ps  "tclbook1" (ee-tclbookfile "p1.ps.gz"))
# (code-ps  "tclbook2" (ee-tclbookfile "p2.ps.gz"))
# (code-ps  "tclbook3" (ee-tclbookfile "p3.ps.gz"))
# (code-ps  "tclbook4" (ee-tclbookfile "p4.ps.gz"))

# (find-tclbook1page 1)
# (find-tclbook2page 1)






#####
#
# single-stepping with wish
# 2004sep11
#
#####

# «single-step-wish»  (to ".single-step-wish")
# (find-man "3tcl split")
# (find-man "3tcl lindex")
# (find-man "3tk bind")
# (find-man "3tk bind" "event type is KeyPress or KeyRelease")
# (find-man "3tk keysyms")

#
wish =(<<'%%%'
  set i 0; set commands [split {
      puts hi
    ! canvas .c -background sienna4
    ! pack .c -expand yes -fill both
    ! .c create line 10 20 30 50
    ! puts bye
    ! exit
  } !]
  bind . <Button-1> {eval [lindex $commands $i]; incr i}
%%%)

#




#####
#
# notes about eegt
# 2004nov08
#
#####

# (find-expcommand "trap [[command] signals]")
# (find-expcommand "send [-flags] string")
# (find-eev "eeg4")
# (find-eev "eegt")
#
#
expect -c '
  source $env(HOME)/TCL/inc.tcl ;# (find-angg "TCL/inc.tcl")
  trap {send_user "Received a SIGUSR1\n"} USR1
  writefile /tmp/eeg.A.pid [pid]
  spawn zsh
  interact
'
#
# (find-sh0 "kill -USR1 $(cat /tmp/eeg.A.pid)")




#####
#
# sending code through a channel to any expect script
# 2005jan03
#
#####

# «eechannel-tcl»  (to ".eechannel-tcl")
# (find-expcommand "sleep")
# (find-expcommand "trap")
# (find-man "1 expect" "prompt1")

 (eebg-channel-xterm "A")
 (eechannel "A")

expect

proc prompt1 {} { return "% " }
#
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

set i 0
while 1 { puts $i; incr i; sleep 1 }

 (eechannel "tcl")

puts hi
puts "hi again"





#####
#
# channel.tcl
# 2005jan03
#
#####

# «channel.tcl»  (to ".channel.tcl")
# (find-eevrc "channel.tcl")

 (eebg-channel-xterm "A")
 (eechannel "A")

expect

proc prompt1 {} { return "% " }
source $env(EEVRCDIR)/channel.tcl
set i 0
while 1 { puts $i; incr i; sleep 1 }

 (eechannel "tcl")

puts hi
puts "hi again"






 (error "Below this point use <f3> and M-e")

#
# (eech-bounded)
puts {a
 b
  c
   d}
#
# (eech "puts foo!")





#####
#
# a resizer
# 2005jan17
#
#####

# «resizer»  (to ".resizer")

# (find-man "3tk wm")
# (find-man "3tk wm" "wm geometry window ?newGeometry?")
# (find-man "3tk wm" "in order to make the change happen")
# (find-man "3tcl regexp")
# (find-man "3tk bind")
# (find-man "3tk bind" "toplevel window")

 (eechannel-xterm "A")
wish

proc incrsize {dx dy} {
  regexp {([0-9]+)(.)([0-9]+)(.)([0-9]+)(.)([0-9]+)} [wm geometry .] \
    -> width sep1 height sep2 x sep3 y
  wm geometry . "[expr $width+$dx]$sep1[expr $height+$dy]$sep2$x$sep3$y"
}
bind . <Left>  { incrsize -5 0 }
bind . <Right> { incrsize  5 0 }
bind . <Up>    { incrsize 0 -5 }
bind . <Down>  { incrsize 0  5 }


# Usage: now give the focus to the "wish" window and use the arrow
# keys to resize it.

# A little bug in fvwm-2.5.12 + tk-8.4.9 (?): move a "wish" window
# around by dragging it with the mouse; after that "wm geometry ."
# will confuse the origin of its fvwm borders and the origin of the
# contained area. The effect of that in the resizer is that in the
# next call to "incrsize" the window will jump a bit to the northwest.




#####
#
# the placer and frames
# 2005jan17
#
#####

# «placer»  (to ".placer")
# «frames»  (to ".frames")

# (find-man "3tk place")
# (find-man "3tk place" "-in master" "defaults to window's parent")
# (find-man "3tk place" "-relx location")
# (find-man "3tk place" "-anchor where")
# (find-man "3tk place" "-x location")

 (eechannel-xterm "A")
wish

proc incrsize {dx dy} {
  regexp {([0-9]+)(.)([0-9]+)(.)([0-9]+)(.)([0-9]+)} [wm geometry .] \
    -> width sep1 height sep2 x sep3 y
  wm geometry . "[expr $width+$dx]$sep1[expr $height+$dy]$sep2$x$sep3$y"
}
bind . <Left>  { incrsize -5 0 }
bind . <Right> { incrsize  5 0 }
bind . <Up>    { incrsize 0 -5 }
bind . <Down>  { incrsize 0  5 }

# (find-man "3tk place")
# (find-man "3tk frame")
#
frame .a -width 40 -height 20 -bg black
frame .b -width 30 -height 60 -bg yellow
frame .c -width 35 -height 35 -bg blue
place .a -x 40 -y 20
place .b -x 30 -y 60
place .c -x 35 -y 35

frame .c.d -width 10 -height 10 -bg red
place .c.d -x 10 -y 10
place configure .c.d -anchor se

frame .e -width 40 -height 40 -bg green
frame .f                      -bg forestgreen
place .e -anchor c  -relx 0.5 -rely 0.5
place .f -anchor nw -relx 0.5 -rely 0.5  -relwidth 0.4 -relheight 0.4

frame .g -width 30 -height 30 -bg mediumvioletred
place .g -in .e
place .g -in .f

frame .h -width 10 -height 10 -bg blueviolet
place .h -in .f -relx 1 -rely 0.5 -anchor c

frame .i -width 10 -height 10 -bg violet
place .i -in .g -relx 1 -rely 0.5 -anchor c

place .g -in .e
place .g -in .f




######
#
# Freehand drawing
# This was borrowed from the Tcler's Wiki and needs comments and cleaning.
# 2005jan18
#
######

# «freehand»  (to ".freehand")
# (find-man "3tk canvas")
# (find-man "3tk canvas" "pathName create line x1 y1... xn yn")
# (find-man "3tk bind")
# (find-man "3tk bind" "%W")

 (eechannel-xterm "A")

wish
pack [canvas .c]
bind .c <ButtonPress-1> {
 set %W(line) [list %W coords [%W create line %x %y %x %y] %x %y]
}
bind .c <B1-Motion> {eval [lappend %W(line) %x %y]}
bind .c <ButtonRelease-1> {unset %W(line)}








#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
