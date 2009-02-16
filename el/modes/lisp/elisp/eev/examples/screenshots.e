

# «.fisl-screenshots»		(to "fisl-screenshots")
# «.fisl-screenshots-F9»	(to "fisl-screenshots-F9")
# «.fisl-screenshots-M-h»	(to "fisl-screenshots-M-h")
# «.fisl-screenshots-modular»	(to "fisl-screenshots-modular")
# «.fisl-screenshots-gdb»	(to "fisl-screenshots-gdb")
# «.intro-to-emacs»		(to "intro-to-emacs")


#
# A comment
# Another comment: (find-elinode "How to Evaluate")
echo $[1+2]
#


%
Hello,
{\bf world}
%







   

lists are values too.

  first element, a symbol:
  the name of the function.
  (note that this symbol
  is not treated as the
  name of a variable)







(+ 1 2 3)
(+ (+ 1 2) (+ 3 4))



# (find-elinode "List Processing")
# (find-elinode "Evaluation")






PS1='%d(%n)# '

(find-angg ".fvwm/")
(find-angg ".fvwm/edrx-menus.fvwm")




#
(insert (propertize "ABCDIMI abcdimi" 'face 'ss-face-1))
(insert (propertize "ABCDIMI abcdimi" 'face 'ss-face-2))

(put-text-property (point) (mark) 'face 'ss-face-1)


  If you press F3 inside a bounded
  region it will be highlighted for a
  short while and saved to a
  temporary file (a "temporary
  script")

  delimiters
 
  a bounded region
  (a block of shell code)

  delimiters of
  another kind

  a bounded region:
  a block of LaTeX code

  type `ee' on a shell to run
  the temporary script

  shell commands (comments)

  a shell command

  its output

  the output of `ee'

  here's a shell prompt
  again














;;
;; (eeb-eval)
;; Prepare to take screenshots

(scroll-bar-mode nil)
(erc-track-disable)

(setq ee-highlight-spec  '(highlight 7))
(setq eeb-highlight-spec '(highlight 7))
(setq eek-highlight-spec '(region    7))

(find-fvwm0 "KillModule FvwmPager")

;;
;; (eeb-eval)
;; Back to normal

(scroll-bar-mode 1)
(erc-track-enable)

(setq ee-highlight-spec  '(highlight 0.75))
(setq eeb-highlight-spec '(highlight 0.5))
(setq eek-highlight-spec '(region 0.75))

(find-fvwm0 "Module FvwmPager")

;;
;; Create the faces
;;
(make-face 'ss-face-1)
(face-spec-reset-face 'ss-face-1)
(make-face 'ss-face-2)
(face-spec-reset-face 'ss-face-2)
(set-face-attribute 'ss-face-1 nil
  :foreground "#ff8902" :height 1.2 :weight 'bold :family "helv")
(set-face-attribute 'ss-face-2 nil
  :foreground "#edf5ff" :height 1.2 :weight 'bold :family "helv")

(defun f (arg) (interactive "P")
  (put-text-property (point) (mark) 'face
    (cond ((eq arg 1) 'ss-face-1)
	  ((eq arg 2) 'ss-face-2))))

;;

(find-efacedescr 'Info-title-3-face)
(find-efacedescr 'Info-title-4-face)
(find-efacedescr 'variable-pitch)
(find-efunctiondescr 'defface)
(find-elnode "Face Attributes")

         Height: 1.2
         Weight: bold
Font or fontset: nil
         Family: helv

(internal-make-lisp-face 'ss-face-1)
(defface ss-face-1
  '((t (:foreground "#ff8902" :height 1.4 :weight "bold" :family "helv")))
  "A face for screenshots")

(find-efacedescr 'ss-face-1)
(find-eapropos "-face")
(find-efile "faces.el")
(find-efunction 'set-face-attribute)

(defun set-face-attribute (face frame &rest args)





running `ee' in a shell executes
the commands in the temporary script

a command (a comment; no output)
a command
its output



an




(put-text-property (point) (mark) 'face '(foreground-color . "white"))
(put-text-property (point) (mark) 'face 'ss-face-1)

 typing F9 on a line starting with  executes the line as Lisp
 typing F9 on any other line sends it to the default channel

 F9 always moves the point to the next line (unless
 there is an error)

 (eebg-channel-xterm "A")   creates an xterm
 		     	      listening on channel A
 (eebg-channel-xterm "B")   creates an xterm
 		     	      listening on channel B
 (eechannel "A")            sets the default channel to A
 (eechannel "B")            sets the default channel to B

 (error "...")		    issues an error - F9 on that
                              line doesn't move down
                          



#####
#
# One way of superposing two images in Gimp, making the black pixels
# of the top one transparent (thanks to lagc for this; I found a
# better way later, but this worked)
# 2004oct??
#
#####


gimp 2.0
View -> Show Menubar

LCP = Window: Layer, Channels, Paths, ...
ss1 = Window: ss1.png


# (find-sh "cd ~/.gimp-2.0/; sort menurc")

<Image>/Tools/Selection Tools/Rect Select   r
<Image>/Edit/Copy                           <Control>c
<Layers>/New Layer...
  (select the "empty layer")
<Image>/Edit/Paste                          <Control>v
<Image>/Layer/Anchor Layer                  <Control>h
  (select the "empty layer")
<Layers>/Add Layer Mask...
  (select "initialize to white")
  (select the "empty layer")
<Image>/Select/By Color                     <Shift>o
  (select the "ask channel")
<Image>/Edit/Fill with FG Color             <Control>comma





#####
#
# B/W screenshots for the FISL paper
# 2005apr14
#
#####

# «fisl-screenshots»  (to ".fisl-screenshots")
#
xinit =(echo "export DISPLAY=:1; exec xterm -e eegchannel 0 zsh") -- \
  /usr/bin/X11/Xnest -br :1 -geometry 590x400 &

#
 (eechannel-xterm "0")
# (find-angg ".fvwm/nokeys.fvwm")
# (find-angg ".zshrc" "fvwm-bw")
fvwm-bw

#
FvwmCommand 'Style "*" Color white/gray50'
FvwmCommand 'Style "*" HilightFore white'
FvwmCommand 'Style "*" HilightBack gray25'
FvwmCommand 'KillModule FvwmPager'
FvwmCommand 'DeskTopSize 1x1'
xsetroot -solid white

xterm -geometry 44x16 &
emacs-cvs -fg black -bg white -cr black -ms black \
  -geometry 52x28 &

#
PS1='%d(%n)# '
clear

#
(my-modes :no-scroll-bar)
(setq eeflash-default '((:background "gray75") 7))
(eev-set-glyph ?\^O 343086 nil)

;; (setq eeflash-default '(highlight 0.5))

;; (progn (find-estring "") (insert (ascstr 341434 343434)))
;; (eev-set-glyph ?\^O ?* 'eev-glyph-face-red)
;; (eev-set-glyph ?\^O 342434 'eev-glyph-face-red)
;; (eev-set-glyph ?\^O 342435 'eev-glyph-face-red)
;; (eev-set-glyph ?\^O 343086 'eev-glyph-face-red)
;; (eev-set-glyph ?\^O 342434 nil)
;; (eev-set-glyph ?\^O 342435 nil)
;; (eev-set-glyph ?\^O 343086 nil)



emacs-cvs -fg black -bg white -cr black -ms black &



Quais eu quero:
F9
page with hyperlinks for up
Debian

lua50-doc

# (find-ekey-links [up])
# (find-debpkg-links "lua50-doc")

fvwm-bw



#
# Global variables
lua50 -e '
  print(print)
  print(_G["print"])
  print(_G.print)
  print(_G)
  print(_G._G)
'
#
# Capture of local variables
lua50 -e '
  foo = function ()
    local storage
    return
      (function () return storage end),
      (function (x) storage = x; return x end)
  end
  get1, set1 = foo()
  get2, set2 = foo()               -- Output:
  print(set1(22), get1())          -- 22 22
  print(set2(33), get1(), get2())  -- 33 22 33
'
#

   ____________________ emacs@localhost _______________________ 
  |                                                _________ xterm __________ 
  |#                                             |/home/edrx(edrx)# ee      |
  |# Global variables                             |# Global variables        |
  |lua50 -e '                                     |lua50 -e '                |
  |  print(print)                                 |  print(print)            |
  |  print(_G["print"])                           |  print(_G["print"])      |
  |  print(_G.print)                              |  print(_G.print)         |
  |  print(_G)                                    |  print(_G)               |
  |  print(_G._G)                                 |  print(_G._G)            |
  |'                                              |'                         |
  |#                                             |function: 0x804dfc0       |
  |# Capture of local variables                   |function: 0x804dfc0       |
  |lua50 -e '                                     |function: 0x804dfc0       |
  |  foo = function ()                            |table: 0x804d420          |
  |    local storage                              |table: 0x804d420          |
  |    return                                     |/home/edrx(edrx)#         |
  |      (function () return storage end),        |__________________________|
  |      (function (x) storage = x; return x end)              |              
  |  end                                                       |
  |  get1, set1 = foo()                                        |
  |  get2, set2 = foo()               -- Output:               |
  |  print(set1(22), get1())          -- 22 22                 |
  |  print(set2(33), get1(), get2())  -- 33 22 33              |
  |'                                                           |
  |#                                                          |
  |                                                            |
  |-:--  lua5.e   91% L325    (Fundamental)--------------------|
  |____________________________________________________________|


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  









#####
#
# Screenshot about F9
# 2005apr18
#
#####

# «fisl-screenshots-F9»  (to ".fisl-screenshots-F9")
#
xinit =(echo "export DISPLAY=:1;
              exec xterm -geometry 35x10 -e eegchannel 0 zsh") -- \
  /usr/bin/X11/Xnest -br :1 -geometry 495x330 &

#
 (eechannel-xterm "0")

# (find-angg ".fvwm/nokeys.fvwm")
# (find-angg ".zshrc" "fvwm-bw")
fvwm &
fvwm-bw
xrdb -merge <(echo '
xterm*geometry: 39x10
xterm*foreground: black
xterm*background: white
')
emacs-cvs -fg black -bg white -cr black -ms black \
  -fn fixed \
  -geometry 35x22+0+0 $EEVE/screenshots.e &

#
cd /tmp/
PS1='%d(%n)# '
clear

#
(my-modes :no-scroll-bar)
(setq eeflash-default '((:background "gray75") 7))
(eev-set-glyph ?\^O ?* 'eev-glyph-face-red)
(eev-set-glyph ?\^O 342434 nil)

;; (setq eeflash-default '(highlight 0.5))



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




(eesteps '(
  "C-x 5 2" "C-x 2" "<<shell>>" (rename-buffer "*shellA*")
            "C-x o" "<<shell>>" (rename-buffer "*shellB*")
  "C-x o" (eek0 "eegchannel A $SHELL\r")
          (eek0 "cd /tmp/; PS1='%d(%n)# '\r")
  "C-x o" (eek0 "eegchannel B $SHELL\r")
          (eek0 "cd /tmp/; PS1='%d(%n)# '\r")
  ))

 (eechannel "B")
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

   _________emacs@localhost____________   ___________channel A______________
  |                                    | |/tmp(edrx)# # Send things to port |
  | (eechannel-xterm "A") ;; create   | | 1234                             |
  | (eechannel-xterm "B") ;; create   | |/tmp(edrx)# {                     |
  |# Listen on port 1234               | |>   echo hi                       |
  |netcat -l -p 1234                   | |>   sleep 1                       |
  |                                   | |>   echo bye                      |
  | (eechannel "A") ;; change target  | |>   sleep 1                       |
  |# Send things to port 1234          | |> } | netcat -q 0 localhost 1234  |
  |{                                   | |/tmp(edrx)#                       |
  |  echo hi                           | |/tmp(edrx)#                       |
  |  sleep 1                           | |__________________________________|
  |  echo bye                          |  ___________channel B______________
  |  sleep 1                           | |/tmp(edrx)# # Listen on port 1234 |
  |} | netcat -q 0 localhost 1234      | |/tmp(edrx)# netcat -l -p 1234     |
  |                                    | |hi                                |
  |-:--  screenshots.e   95% L409   (Fu| |bye                               |
  |_Wrote /home/edrx/.eev/eeg.A.str____| |/tmp(edrx)#                       |
					 |                                  |
  					 |__________________________________|







#####
#
# screenshot about M-h
# 2005may28
#
#####

# «fisl-screenshots-M-h»  (to ".fisl-screenshots-M-h")
#
cat > /tmp/foo <<'%%%'
(my-modes :no-scroll-bar)
(blink-cursor-mode)
(eek "M-h M-f find-file")
(eek "M-h M-k C-x C-f")
%%%

fvwm-bw
xsetroot -solid white
emacs-cvs -fg black -bg white -cr black -ms black \
  -geometry 48x20 /tmp/foo
xsetroot -solid black
fvwm-replace

#
# (find-angg ".emacs" "my-screenshot")

   _________________________________________________________ 
  |# (find-efunction-links 'find-file)                      |
  |                                                         |
  |# (where-is 'find-file)                                  |
  |# (describe-function 'find-file)                         |
  |# (find-efunctiondescr 'find-file)                       |
  |# (find-efunction 'find-file)                            |
  |# (find-efunctionpp 'find-file)                          |
  |# (find-efunctiond 'find-file)                           |
  |# (find-eCfunction 'find-file)                           |
  |# (find-estring (documentation 'find-file))              |
  |# (find-estring (documentation 'find-file t))            |
  |                                                         |
  |# (Info-goto-emacs-command-node 'find-file)              |
  |# (find-enode "Command Index" "* find-file:")            |
  |# (find-elnode "Index" "* find-file:")                   |
  |                                                         |
  |                                                         |
  |                                                         |
  |--:**  *Elisp hyperlinks*   All L18    (Fundamental)-----|
  |_________________________________________________________|





#####
#
# screenshot about big modular e-scripts
# 2005may31
#
#####

# «fisl-screenshots-modular»  (to ".fisl-screenshots-modular")
#
cut -b3- > /tmp/modular.e <<'%%%'
  -*- coding: raw-text-unix -*-
   (my-modes :no-scroll-bar)
   (blink-cursor-mode 0)
   (fvwm-sloppy-focus)
   (setq eeflash-default '((:background "gray75") 7))
   (eev-set-glyph ?\^O 342434 nil)
   (eev-set-glyph ?« ?« nil)
   (eev-set-glyph ?» ?» nil)
   ;; (eev-set-glyph ?\^O ?* 'eev-glyph-face-red)
   ;; (eev-set-glyph ?« ?« 'eev-glyph-face-green)
   ;; (eev-set-glyph ?» ?» 'eev-glyph-face-green)
   ;; (setq eeflash-default '(highlight 0.5))
   ;; (fvwm-click-to-focus)
   (eechannel-xterm "A" nil (ee-split "-geometry 40x15+283+37"))
  cd /tmp/
  PS1='%d(%n)# '
  clear
   (eek "<down> M-0 C-l")

  # Index:
  # «.first_block»        (to "first_block")
  # «.second_block»       (to "second_block")

  #
  # «first_block»  (to ".first_block")
  echo blah
  #
  # «second_block»  (to ".second_block")
  echo blah blah
  #

   (eechannel-xterm "A")
  echo foo
   (eevnow-at "first_block")
   (eevnow-at "second_block")
  echo bar

%%%

#
xinit =(echo "export DISPLAY=:1;
              exec xterm -geometry 35x10 -e eegchannel 0 zsh") -- \
  /usr/bin/X11/Xnest -br :1 -geometry 550x330 &

#
 (eechannel-xterm "0")

# (find-angg ".fvwm/nokeys.fvwm")
# (find-angg ".zshrc" "fvwm-bw")
fvwm &
fvwm-bw
xrdb -merge <(echo '
xterm*geometry: 39x10
xterm*foreground: black
xterm*background: white
')
emacs-cvs -fg black -bg white -cr black -ms black \
  -fn fixed \
  -geometry 44x23+0+0 /tmp/modular.e &

#
pngcrush -c 0 -m 0 /tmp/screenshots/ss2.png \
    ~/FISL/ss-modular-crush.png
laf ~/FISL/ss-modular-crush.png /tmp/screenshots/ss2.png
convert ~/FISL/ss-modular-crush.png ~/FISL/ss-modular.eps




#####
#
# screenshot comparing big modular e-scripts and GDB
# 2005may31
#
#####

# «fisl-screenshots-gdb»  (to ".fisl-screenshots-gdb")

#
cat > /tmp/prog.c <<'%%%'
#include <stdio.h>
void block_one () {
  printf("echo blah\n");
}
void block_two () {
  printf("echo blah blah\n");
}
main() {
  printf("# comment 1\n");
  block_one();
  block_two();
  printf("# comment 2\n");
}
%%%
cd /tmp/
gcc -g -o prog prog.c

cat > /tmp/foo.e <<'%%%'
(my-modes :no-scroll-bar)
(blink-cursor-mode)
(set-face-foreground 'breakpoint-enabled "gray80")
;; (set-face-foreground 'breakpoint-enabled "red")
(eesteps
  '((ee-gdb-start "/tmp/" "prog")
    (set-face-foreground
     'breakpoint-enabled "gray50")
    (eek0 "br main\r")
    (eek0 "run\r")
    "<<flm>>" "M-G" "<<flm>>"
    "M-s" "M-s" "M-s" "M-s" "M-s" "M-s"
    "C-u -2 C-l"
    ))
%%%

fvwm-bw
xsetroot -solid white
emacs-cvs -fg black -bg white -cr black -ms black \
  -geometry 48x33 /tmp/foo.e
xsetroot -solid black
fvwm-replace

#
cd ~/FISL/
pngcrush -c 0 -m 0 \
    /tmp/screenshots/ss1.png ss-gdbtall-crush.png
convert                      ss-gdbtall-crush.png ss-gdbtall.eps
laf /tmp/screenshots/ss1.png ss-gdbtall-crush.png ss-gdbtall.eps 

cd ~/FISL/
pngcrush -c 0 -m 0 \
    /tmp/screenshots/ss3.png ss-gdbwide-crush.png
convert                      ss-gdbwide-crush.png ss-gdbwide.eps
laf /tmp/screenshots/ss3.png ss-gdbwide-crush.png ss-gdbwide.eps 

#






#####
#
# intro to emacs
# 2005oct30
#
#####

# «intro-to-emacs»  (to ".intro-to-emacs")

(my-make-face
 'my-face nil
 :foreground "#ffbb00"
 :weight 'bold
 :inherit 'variable-pitch)

(put-text-property (point) (ee-search-forward "\n;;-\n") 'face 'my-face)

tool bar
menu bar
scroll bar

mode line
buffer name
mode: fundamental-mode
mode: dired
mode
echo area
(the 3 is the result of (+ 1 2))

the cursor ("point")
the lisp expression
("sexp") before point

a window
another
window
a frame

;;-

;; (find-angg ".emacs" "my-modes")
;; (find-efunction 'eek-eval-last-sexp)
;; (my-modes :long-flash   :no-blink)
;; (my-modes :normal-flash :blink)
(progn
  (find-fline "~/.eev/HELP" "(+ 1 2)")
  (eek "M-0 M-E"))






# (find-fline "/tmp/modular.e")



cut -b3- > /tmp/prep.e <<'%%%'
%%%



# (eebg-gv "~/FISL/eev.ps.gz")









#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  modes:                (fundamental-mode emacs-lisp-mode)
#  End:
