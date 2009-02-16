


# «.compile-ming»	(to "compile-ming")
# «.compile-vnc2swf»	(to "compile-vnc2swf")
# «.compile-swftools»	(to "compile-swftools")
# «.my_swf_functions»	(to "my_swf_functions")

# «.movie1_A0»		(to "movie1_A0")
# «.movie1_B0»		(to "movie1_B0")
# «.movie1_mini_emacs»	(to "movie1_mini_emacs")
# «.movie1»		(to "movie1")
# «.movie1_swf»		(to "movie1_swf")

# «.movie2_A0»		(to "movie2_A0")
# «.movie2_B0»		(to "movie2_B0")
# «.movie2_mini_emacs»	(to "movie2_mini_emacs")
# «.movie2»		(to "movie2")
# «.movie2_swf»		(to "movie2_swf")





#####
#
# compiling vnc2swf-0.4.2 and its dependencies:
#   ming-0.2a (note that vnc2swf-0.4.2 doesn't work with ming-0.3)
#   swftools (for generating the html wrapper around the .swf)
# 2005feb08
#
#####

#
# «compile-ming»  (to ".compile-ming")
# http://www.opaque.net/wiki/index.php?MingInstall
# http://voxel.dl.sourceforge.net/sourceforge/ming/ming-0.2a.tar.bz2
# (code-c-d "ming" "~/usrc/ming-0.2a/")
# (find-mingfile "")
# (find-mingfile "src/")
# (find-mingfile "INSTALL")
# (find-mingfile "Makefile")
#
rm -Rv ~/usrc/ming-0.2a/
tar -C ~/usrc/ -xvzf \
  $S/http/voxel.dl.sourceforge.net/sourceforge/ming/ming-0.2a.tgz
cd     ~/usrc/ming-0.2a/
make \
  PREFIX=/usr/share/php \
  CFLAGS="-L/usr/lib/php4/20020429 -I/usr/include/php4" \
  dynamic static                                   2>&1 | tee om

#
# «compile-vnc2swf»  (to ".compile-vnc2swf")
# http://www.unixuser.org/~euske/vnc2swf/
# http://www.unixuser.org/~euske/vnc2swf/rec_vncserver.html
# http://www.unixuser.org/~euske/vnc2swf/vnc2swf-0.4.2.tar.gz
# (code-c-d "vnc2swf" "~/usrc/vnc2swf-0.4.2/")
# (find-vnc2swffile "")
# (find-vnc2swffile "Makefile")
# (find-vnc2swffile "configure.in")
#
rm -Rv ~/usrc/vnc2swf-0.4.2/
tar -C ~/usrc/ -xvzf \
  $S/http/www.unixuser.org/~euske/vnc2swf/vnc2swf-0.4.2.tar.gz
cd     ~/usrc/vnc2swf-0.4.2/
./configure --with-ming=$HOME/usrc/ming-0.2a/src   2>&1 | tee oc
make                                               2>&1 | tee om

#
# «compile-swftools»  (to ".compile-swftools")
# http://www.quiss.org/swftools/swftools-0.6.3.tar.gz
# (code-c-d "swftools" "~/usrc/swftools-0.6.3/")
# (find-swftoolsfile "")
# (find-swftoolsfile "src/")
#
rm -Rv ~/usrc/swftools-0.6.3/
tar -C ~/usrc/ -xvzf $S/http/www.quiss.org/swftools/swftools-0.6.3.tar.gz
cd     ~/usrc/swftools-0.6.3/
./configure                                        2>&1 | tee oc
make                                               2>&1 | tee om

#

# (find-status   "vncserver")
# (find-vldifile "vncserver.list")
# (find-udfile   "vncserver/")
# (find-vldifile "vncserver.postinst")
# (find-fline "/etc/alternatives/" "vncserver")

# (find-man "1x realvncserver")
# (find-man "1 realvncconnect")
# (find-man "1 Xrealvnc")

# (find-status   "xvncviewer")
# (find-vldifile "xvncviewer.list")
# (find-udfile   "xvncviewer/")

# (find-man "1x realvncserver" "vncserver :1")
# (find-man "1x realvncserver" "vncserver -kill :1")
# (find-vnc2swffile "docs/")
# (find-vnc2swfw3m  "docs/vnc2swf.html")
# (find-vnc2swffile "docs/rec_vncserver.html" "embed src=")

#
# «my_swf_functions»  (to ".my_swf_functions")
# (find-man "8 ld.so" "LD_LIBRARY_PATH")
function my_vnc2swf () {
  LD_LIBRARY_PATH=$HOME/usrc/ming-0.2a/src: \
    ~/usrc/vnc2swf-0.4.2/vnc2swf $*
}
function my_swfdump () { ~/usrc/swftools-0.6.3/src/swfdump $*; }
function my_swftohtml () {
  echo -e "<html>\n<head><title>$1</title></head>\n<body>"
  my_swfdump --html $1
  echo -e "</body>\n</html>"
}

#






#####                                                          
#  __  __            _        ___          _                            _     
# |  \/  | _____   _(_) ___  |_ _|_    ___| |__   __ _ _ __  _ __   ___| |___ 
# | |\/| |/ _ \ \ / / |/ _ \  | |(_)  / __| '_ \ / _` | '_ \| '_ \ / _ \ | __|
# | |  | | (_) \ V /| |  __/  | | _  | (__| | | | (_| | | | | | | |  __/ |__ \
# |_|  |_|\___/ \_/ |_|\___| |___(_)  \___|_| |_|\__,_|_| |_|_| |_|\___|_|___/
#
# A short movie about <f9>
#   In :0.0/(channel A0) we launch vncserver and vnc2swf
#   In :0.0/(channel B0) we treat :2 and /tmp and launch an emacs in :2
#   In :2/(emacs) we prepare emacs and set HOME to /tmp 
#   In :2/(emacs)/<<movie1>> we launch two xterms and make them talk
#      through netcat
#   In :0.0/(channel B0)/<<movie1_post>> we generate an html wrapper
#      around the swf
# 2005feb08 / 2005mar23
#
#####

# (fvwm-sloppy-focus)
# (fvwm-click-to-focus)

# «movie1_A0»  (to ".movie1_A0")
 (eechannel-xterm "A0" nil '("-geometry" "79x10"))
 (ee-at "my_swf_functions" (eev-bounded) (eech "ee\n"))

# Kill the running vncserver
vncserver -kill :2
killall -9 Xrealvnc
rm -fv /tmp/.X2-lock
rm -fv /tmp/.X11-unix/X2

rm -Rv ~/.vnc/     ;# goodbye, old vnc password, goodbye
vncserver -geometry 600x400 -depth 16 :2
password
password

export DISPLAY=:0.0
my_vnc2swf -nostatus /tmp/channels.swf :2
password


# «movie1_B0»  (to ".movie1_B0")
 (eechannel-xterm "B0" nil '("-geometry" "79x10"))
 (ee-at "my_swf_functions" (eev-bounded) (eech "ee\n"))
export DISPLAY=:2
xsetroot -solid black
unclutter -idle 2 &
xhost +

cat > /tmp/.zshrc <<'---'
PS1='%d# '
PS2='> '
setopt autocd interactivecomments
---
cat > /tmp/.Xdefaults-small <<'---'
xterm*geometry: 40x12
xterm*foreground: black
xterm*background: white
---

emacs-cvs -geometry 40x28+0+0 \
  -fg bisque -bg black \
  $EEVDIR/examples/anim.e &

 ---
;; «movie1_mini_emacs»  (to ".movie1_mini_emacs")
(progn
  (cd "/tmp")
  (eechannel "A" "exit\n")
  (eechannel "B" "exit\n")
  (find-sh0 "xrdb -merge <(echo 'xterm*geometry: 40x12')")
  (find-sh0 "xrdb -merge <(echo 'xterm*foreground: black')")
  (find-sh0 "xrdb -merge <(echo 'xterm*background: white')")
  (find-sh0 "xsetroot -solid black")
  (fvwm-sloppy-focus)
  (define-key eev-mode-map [f7] 'eechannel-do-this-line)
  (scroll-bar-mode -1)
  (my-modes :no-scroll-bar :no-pager)
  ;; (my-modes :no-fringe)
  )

;; (setenv "HOME" "/home/edrx")
;; (setenv "HOME" "/tmp")

# «movie1»  (to ".movie1")
# This runs inside the tiny emacs in :2.
# To run this we'd normally type F9, F9, ..., F9, but
# as vnc2swf defines F9 and F8 as `record' and `menu'
# we use a define-key so that we can type F7, F7, ..., F7
# in emacs and get the same effect as F9, F9, ..., F9.

 (eechannel-xterm "A") ;; create
 (eechannel-xterm "B") ;; create
# Listen on port 1234
netcat -l -p 1234

 (eechannel "A") ;; change target
# Send things to port 1234
{
  echo hi
  sleep 1
  echo bye
  sleep 1
} | netcat -q 0 localhost 1234















# «movie1_swf»  (to ".movie1_swf")
 (eechannel-xterm "B0" nil '("-geometry" "79x10"))
 (ee-at "my_swf_functions" (eev-bounded) (eech "ee\n"))

export DISPLAY=:0.0
cd /tmp/
my_swftohtml channels.swf | tee channels.html
firefox /tmp/channels.html &






#####
#  __  __            _        ___ ___      ____ ____  ____  
# |  \/  | _____   _(_) ___  |_ _|_ _|_   / ___|  _ \| __ ) 
# | |\/| |/ _ \ \ / / |/ _ \  | | | |(_) | |  _| | | |  _ \ 
# | |  | | (_) \ V /| |  __/  | | | | _  | |_| | |_| | |_) |
# |_|  |_|\___/ \_/ |_|\___| |___|___(_)  \____|____/|____/ 
#
# Movie 2: a short movie about gdb
#   In :0.0/(channel A0) we launch vncserver and vnc2swf
#   In :0.0/(channel B0) we treat :2 and /tmp and launch an emacs
#      and an xterm in :2
#   In :2/(emacs) we prepare emacs (scroll bars, etc)
#   In :2/(emacs)/<<movie2>> we store a series of steps, start the recording,
#      then create and compile a C program in two parts (by hand, in an xterm)
#      then run gdb on it
#   In :0.0/(channel B0)/<<movie2_post>> we generate an html wrapper
#      around the swf
# 2005mar23
#
#####

# «movie2_A0»  (to ".movie2_A0")
 (eechannel-xterm "A0" nil '("-geometry" "79x5"))
 (ee-at "my_swf_functions" (eev-bounded) (eech "ee\n"))

# Kill the running vncserver
vncserver -kill :2
killall -9 Xrealvnc
rm -fv /tmp/.X2-lock
rm -fv /tmp/.X11-unix/X2

rm -Rv ~/.vnc/     ;# goodbye, old vnc password, goodbye
vncserver -geometry 675x550 -depth 8 :2
password
password

export DISPLAY=:0.0
my_vnc2swf -geometry -0-0 -nostatus -buffered -framerate 6 /tmp/gdb.swf :2
password


# «movie2_B0»  (to ".movie2_B0")
 (eechannel-xterm "B0" nil '("-geometry" "79x5"))
 (ee-at "my_swf_functions" (eev-bounded) (eech "ee\n"))
export DISPLAY=:2
xsetroot -solid black
unclutter -idle 2 &
xhost +
FvwmCommand 'KillModule FvwmPager'
# FvwmCommand   'Module FvwmPager'
FvwmCommand 'Style "*" ClickToFocus'

emacs-cvs -geometry 77x40+0+0 \
  -fg bisque -bg black \
  $EEVDIR/examples/anim.e &
xterm -bg white -fg black -geometry 77x34-15+25 &
 (eev "cd; PS1='%d# '; clear")


 ---
;; «movie2_mini_emacs»  (to ".movie2_mini_emacs")
(progn
  (my-modes :no-scroll-bar :no-pager)
  ;; (my-modes :no-fringe)
  (setq eeflash-default '(highlight 0.75))
  )

;; (find-node "(gdb)Data")
;; (find-node "(gdb)Calling")

(eesteps
 '((eek0 "ee") (eek0 "\r")
   (eek "M-G")
   (eek "M-n")
   (eek "M-n")
   (eek "M-n")
   (eek "M-s")
   (eek "M-n")
   (eek "M-n")
   (eek "M-n")
   (eek0 "ptype f")  (eek0 "\r")
   (eek0 "ptype *f") (eek0 "\r")
   (eek0 "p f")      (eek0 "\r")
   (eek0 "p f()")    (eek0 "\r")
   (eek0 "call f()") (eek0 "\r")
   (eek "M-n")
   (eek "M-n")
   (eek "M-n")
   (eek "M-k")
   (eek "M-k")))

;; «movie2»  (to ".movie2")

#
rm -Rv /tmp/dlopen/
mkdir  /tmp/dlopen/
cd     /tmp/dlopen/

cat > so.c <<'---'
#include <stdio.h>
void externalfunction() {
  printf("Hello from so.so\n");
}
---

cat > prog.c <<'---'
#include <dlfcn.h>
#include <stdio.h>
int main() {
  void *libptr;
  void (*f)();
  printf("Hello from prog\n");
  libptr=dlopen("/tmp/dlopen/so.so", RTLD_LAZY);
  f=dlsym(libptr, "externalfunction");
  f();
  printf("Bye from prog\n");
  return 0;
}
---

ls -lAF

#
cd     /tmp/dlopen/
gcc -g -Wall -shared -o so.so so.c
gcc -g -Wall -ldl    -o prog  prog.c
ls -lAF
./prog

#
# (ee-once (eeb-gdb-start "/tmp/dlopen/" "prog"))
br main
run

#
























# «movie2_swf»  (to ".movie2_swf")
 (eechannel-xterm "B0" nil '("-geometry" "79x5"))
 (ee-at "my_swf_functions" (eev-bounded) (eech "ee\n"))

export DISPLAY=:0.0
cd /tmp/
my_swftohtml gdb.swf | tee gdb.html
firefox /tmp/gdb.html &


#####
#
# garbage
#
#####



(ee-expand "$EEVDIR")
(ee-expand "$EEVDIR/eegchannel")

(getenv "EEVDIR")
(getenv "HOME")
(switch-to-buffer "*Messages*")
(message "Foo!")

(eechannel-running-p "A")
(eebg-channel-xterm "A")
(eechannel "A" "exit\n")
(find-sh0 "xrdb -load ~/.Xdefaults")


#
# (progn (eev-bounded) (find-zsh0 ". $EE"))

# (find-enode "Action Arguments" "--eval=EXPRESSION")

:

 (eechannel-xterm "A")

fvwm &
unclutter -idle 2 &





ps ax | grep vnc



LD_LIBRARY_PATH=$HOME/usrc/ming-0.2a/src: \
  ~/usrc/vnc2swf-0.4.2/vnc2swf out.swf :2




# xterm -display :1 &
export DISPLAY=:0.0


#
cd /tmp/
my_swftohtml out.swf | tee out.html

#
cd ~/eev-current/anim/
swftohtml channels.swf | tee channels.html

#









# (find-node "(make)Catalogue of Rules")
# (find-vnc2swffile "oc")
# (find-vnc2swffile "om")




vncserver -geometry 320x240 -depth 16 :2
# cd ~/usrc/vnc2swf-0.4.2/

cd /tmp/
# (find-man "8 ld.so" "LD_LIBRARY_PATH")
LD_LIBRARY_PATH=$HOME/usrc/ming-0.2a/src: \
  ~/usrc/vnc2swf-0.4.2/vnc2swf out.swf :2



# (ee-expand "~/eev-current/")
# (ee-expand "$EEVDIR")
# (find-sh0 "xrdb -load ~/.Xdefaults")



# fvwm-click-to-focus






#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
