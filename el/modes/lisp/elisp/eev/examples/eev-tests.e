# Tests for most functions in `eev-dev.el'
# Edrx, 2004dec18



# «.eev-dev.el»		(to "eev-dev.el")
# «.eev-insert.el»	(to "eev-insert.el")
# «.eev-langs.el»	(to "eev-langs.el")
# «.html»		(to "html")
# «.eechannel»		(to "eechannel")
# «.test-install-beth»	(to "test-install-beth")
# «.test-beth»		(to "test-beth")
# «.check-.files»	(to "check-.files")




####
#
# basic tests - eev-dev.el
# 2005jan13
#
####

;; «eev-dev.el»  (to ".eev-dev.el")
;; (find-eev "eev-dev.el")
;; (find-sh "egrep 'defun|defvar|defmacro|eeb-define' $EEVDIR/eev-dev.el")
;;

;;; autoloads for external functions (no tests)
;;; environment variables
;;
(ee-expand "$HOME/http://foo/~bar/$S/")
(ee-expand "~aleph")
(find-sh "set | grep -a ^EE | tr = \\\\t")
(eev     "set | grep -a ^EE | tr = \\\\t")

;;; variables (no tests)
;;; tools for calling hyperlink functions interactively
;;
(let ((show-it nil)) (ee-maybe-showing-it '(a "b\nc")))
(let ((show-it t))   (ee-maybe-showing-it '(a "b\nc")))
;; ee-stuff-around-point
;; ee-debpkgname-around-point
;; ee-debpkgname-ask
;; ee-manpagename-around-point
;; ee-manpagename-ask

;;; basic hyperlinks (to text files and info nodes)
;;
;; ee-goto-position
;; ee-goto-rest
(find-fline "~/eev-current/")
(find-node "(emacs)Glossary" "\nRestriction")

;;; hyperlinks to the output of Emacs's help-like functions
;;
;; Note:
;; try with both: (setq pop-up-windows nil)
;;           and: (setq pop-up-windows t)
;;
(find-wottb-call '(list-colors-display) "*Colors*" " bisque")
(find-eapropos       "unwind"        "\nunwind-protect")
(find-efunctiondescr 'find-function  "load-path")
(find-evariabledescr 'pop-up-windows "new windows")
(find-evardescr      'same-window-buffer-names "pop-to-buffer")
(find-ekeydescr      "\C-x40"        "delete")
(find-efacedescr     'italic         "Slant")
(find-efaces                         " italic")
(find-ecolors                        " bisque")
(find-efunctiond     'find-file      "switch-to-buffer")



;; find-ebufferandpos
(find-efunction  'describe-key    "click")
(find-evariable  'auto-mode-alist ".el")
(find-eCfunction 'pop-to-buffer)
(find-eCvariable (symbol &rest rest)



(find-efunctiondescr 'describe-key)
(find-eapropos "find-")

(find-efunction 'describe-key)
(find-evariable 'auto-mode-alist)
(find-ebuffer "*Messages*" "Info directory")
(find-etpat)
(progn (find-efaces "Info-title-1") (find-etpat))

(find-sh "date")
(find-sh0 "date")
(find-progoutput "date")
(find-man "1 ls" "--almost-all")
(find-dvipage "/usr/share/doc/texmf/latex/general/essential.dvi.gz" 20)
(find-dvipage "/usr/share/doc/texmf/tetex/TETEXDOC.dvi.gz" 1)
(find-pspage  "/usr/share/doc/debian/FAQ/debian-faq.ps.gz" 4)

(find-Package "/var/lib/dpkg/status" "tetex-doc")
(find-status "tetex-doc")
(find-available "tetex-doc")
(find-availablegrep "emacs-goodies")

(find-anchor "tcltk.e" "single-step-wish")
# «anchor1» (ee-to "anchor2")
# «anchor2» (ee-to "anchor1")

(find-w3m "http://127.0.0.1/")
(find-w3m "$S/http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.html")
(find-w3m "/home/edrx/snarf/http://fly.srk.fer.hr/~hniksic/emacs/")

;;
;; mass-producing hyperlink functions
;;
(defmacro ee-erf-foo (&rest body)
  `(let ((ee-arg 9))
     (find-ebuffer "*ee-eval-read-format*")
     (emacs-lisp-mode)
     . ,body))

(ee-erf-foo
 (code-ps "debianfaq" "/usr/share/doc/debian/FAQ/debian-faq.ps.gz"))
(ee-erf-foo
 (code-dvi "essential" "/usr/share/doc/texmf/latex/general/essential.dvi.gz"))
(ee-erf-foo (code-c-d       "eev" "$EEVDIR/" "elisp"))
(ee-erf-foo (code-c-d-linux "k24" "~/bigsrc/kernel-source-2.4.18/"))
(ee-erf-foo (code-c-d-gdb   "lua" "~/eev-current/lua-5.0/" "libc"))

(code-ps "debianfaq" "/usr/share/doc/debian/FAQ/debian-faq.ps.gz")
(find-debianfaqpage 5)
(code-dvi "essential" "/usr/share/doc/texmf/latex/general/essential.dvi.gz")
(find-essentialpage 21)

(code-c-d "eev" "$EEVDIR/" "elisp")
(find-eevfile "")
(find-eevnode "")

(code-c-d-linux "k24" "~/bigsrc/kernel-source-2.4.18/")
(find-k24confvar "CONFIG_VIDEO_SELECT")

;; The xxxgdb part is broken
(ee-erf-foo (code-c-d-gdb "lua" "~/eev-current/lua-5.0/" "libc"))

(find-evariable 'ee-comment-prefix)
(find-evardescr 'ee-comment-prefix)
(find-function  'ee-comment-prefix)
(ee-comment-prefix)


;;;
;;; mass-producing hyperlink functions: examples (debian-centric)
;;;
(find-efile "")
(find-enode "Help")
(find-elnode "Function Cells")
(find-elinode "Top")
(find-eetcfile "")

(find-eevfile "")
(find-eev     "")
(find-eevtmpfile "")
(find-eevrcfile "")
(find-eevexfile "")

(find-udfile   "bash/")
(find-vldifile "bash.list")
(find-bashfile "")
(find-bashnode "")
(find-zshfile "")
(find-zshnode "")
(find-apthowtofile "")
(find-apthowtofile "apt-howto.en.txt.gz")

(code-c-d "bash" (ee-udfile "bash/") "bashref")
(code-c-d "zsh"  (ee-udfile "zsh/")  "zsh")
(code-c-d "apthowto" "/usr/share/doc/Debian/apt-howto/")

;;;
;;; temporary highlighting
;;; evaluating sexps
;;: saving regions
;;;
(eev "echo hello")
;; write a test for eevs
(eelatex "Hello")
(eegdb "print 1+2")
;; write a test for eeg
(eecd "echo $PWD")




;;;;;;
;;
;; tests for the functions that operate on bounded regions
;; (the structures esdelims+ and esdelims++ are a bit hard to understand)
;;
;;;;;;

;; (make-face           'eebdemo-face)
;; (set-face-background 'eebdemo-face "magenta")
;; (find-efaces         "eebdemo-face")
;; (find-estring (concat "<" (propertize "abc" 'face 'eebdemo-face) ">"))
;; (defun eebdemo (s e) (interactive "r") (message (format "%S" (list s e))))
;; (defun eeb-try (&rest args) (let ((eeb-defaults args)) (eeb-default)))

;;*
;; (eeb-try 'eebdemo "\n;;*\n" "\n;;**\n" '(eebdemo-face 0.5) nil)
;; (eeb-try 'eebdemo "\n;;*\n" "\n;;**\n" '(eebdemo-face 0.5) 0)
;; (eeb-try 'eebdemo "\n;;*\n" "\n;;**\n" '(eebdemo-face 0.5) 1)
;; (eeb-try 'eebdemo "\n;;*\n" "\n;;**\n" '(eebdemo-face 0.5) 2)
;; (eeb-try 'eebdemo "\n;;*\n" "\n;;**\n" '(eebdemo-face 0.5) t)
;;**

;; (setq eebdemo-flash '(eebdemo-face 1))
;; (setq eebdemo-flash '(eebdemo-face 0.5))
;; (setq eebdemo-delim1  "\n;;*\n")
;; (setq eebdemo-delim2 "\n;;**\n")

;; (defun eeb-try (&rest args) (apply 'ee-sedelims++-to-sedelims+ (cdr args)))
;; (defun eeb-try (&rest args) (let ((eeb-defaults args)) (eeb-default)))

;;*
;; (eeb-try 'eebdemo "\n;;*\n"       nil             '(eebdemo-face 0.5) t)
;; (eeb-try 'eebdemo "\n;;*\n"       "\n;;*\n"       '(eebdemo-face 0.5) t)
;; (eeb-try 'eebdemo "\n;;*\n"       "\n;;**\n"      '(eebdemo-face 0.5) t)
;; (eeb-try 'eebdemo 'eebdemo-delim1 'eebdemo-delim2 'eebdemo-flash      t)
;; (eeb-try 'eebdemo 'eebdemo-delim1 'eebdemo-delim1 'eebdemo-flash      t)
;; (eeb-try 'eebdemo 'eebdemo-delim1 nil             nil                 t)
;;**
;;*

#
%
(eeb-default)
(eev-bounded)
(eelatex-bounded)

%
#





;;;;;; old:

;;;
;;; auxiliary functions for saving delimited ("bounded") regions
;;;
#
# (eeb-string         ee-delimiter-hash)
# (eeb-string-with-nl ee-delimiter-hash)
#

;;;
;;; head sexps in delimited regions
;;;
#
# (insert "FOO")
# «head-sexps»
# (ee-eval-head-sexp 0)
#
# (ee-eval-head-sexp-at "head-sexps" 0)

;;;
;;; the default action on delimited regions
;;; saving delimited regions
;;;
#
# (eeg-bounded)
# (ee-once (eeg-bounded))
# (find-evardescr 'eebd-function)
echo foo
exit
#
# (eev-bounded)
# (find-evardescr 'eebd-function)
$EEVDIR/eeg bash
#
%
% (eelatex-bounded)
% (find-eevtmpfile "tmp.tex")
% (find-fline "$EE")
% (find-fline ee-file-tex)
hello
%

;;;
;;; eev keys mode
;;;
(find-evariable 'eev-mode-map)
(find-evardescr 'eev-mode-map)
(describe-mode)
(describe-bindings)

;;;
;;; gud "hyperlinks"
;;; (I haven't written the tests for those yet).
;;;




#####
#
# basic tests - eev-insert.el
# 2004oct18
#
#####

# «eev-insert.el»  (to ".eev-insert.el")
# (find-eevfile "eev-insert.el")

(eesteps '((insert "\nicont\nmawk")
	   "<up> C-a"
	   "<<dfa>>"
	   "<<dff>>"))







#####
#
# basic tests - eev-langs.el
# 2004oct18
#
#####

# «eev-langs.el»  (to ".eev-langs.el")
# (find-eevfile "eev-langs.el")




#####
#
# eev-langs: html
#
#####

# «html»  (to ".html")

#
<!-- (eeb-html)
-->
H&eacute;llo

#
<!-- (eeb-html)
  http://wp.netscape.com/eng/mozilla/3.0/handbook/javascript/
  http://wp.netscape.com/eng/mozilla/3.0/handbook/javascript/getstart.htm
-->
<SCRIPT LANGUAGE="JavaScript">
<!--- Hide script from old browsers.
document.write("Hello, net!")
// End the hiding here. -->
</SCRIPT>
<P>That's all, folks. 

#




#####
#
# eechannel
# 2004dec17
#
#####

# «eechannel»  (to ".eechannel")
# (find-eevfile "eev-dev.el" "defun eechannel ")

# (find-es "net" "netcat")
# (find-udfile "netcat/README.gz")
# (find-man "1 netcat")
# (find-man "1 netcat" "-q seconds")

# (my-modes :no-scroll-bar :no-pager :no-erc-track)
# (eechannel-send "A" "PS1='%d(%n)# '; clear\n")
# (eechannel-send "B" "PS1='%d(%n)# '; clear\n")




 (eebg-channel-xterm "A")
 (eebg-channel-xterm "B")

 (eechannel "A")
echo hi

 (eechannel "B")
# This netcat will listen for input coming through
# the port 1234
netcat -l -p 1234

 (eechannel "A")
date
# Now send the date string throught the port 1234
date | netcat -q 0 localhost 1234

 (eechannel "B")
# input received

 (error "End of the eechannel demo - stop typing F9, please")








#####
#
# ?
#
#####


# I need to write tests for these:
(find-efunction 'ee-inn)
(find-efunction 'ee-inns)
(find-efunction 'ee-dfa)
(find-efunction 'ee-dff)

(setq eelatex-eevscript "cd ~/eev-current/; latex tmp.tex && xdvi tmp.dvi &")
(eev "echo foo")
(eev "echo bar" nil (concat ee-file ".2"))
(eelatex "hello foo")
(eegdb "p 22+33")
(progn (eegdb "p 22+33\n") (eeg "ee\nquit\n") (eev "$EEVDIR/eeg gdb"))






####
#
# compile lua 5.0 into ~/eev-current/tmp/ (with -g and other stuff)
# this is used by the eegud tests below
#
####

# (find-es "lua5" "install-5.0")
# (code-c-d "lua50" "~/tmp/lua-5.0/")

# (code-c-d "lua50" "~/eev-current/tmp/lua-5.0/")
# (code-c-d "lua"   "~/eev-current/tmp/lua-5.0/")
# (find-luaftpfile "")
# (find-lua50file "")
# (find-luafile "")
#
mkdir ~/eev-current/tmp/
cp -iv /home/edrx/snarf/http/www.lua.org/ftp/lua-5.0.tar.gz ~/eev-current/tmp/

#
cd    ~/eev-current/tmp/
tar -xvzf lua-5.0.tar.gz
cd    ~/eev-current/tmp/lua-5.0/

#
rm -Rv ~/eev-current/tmp/lua-5.0/
mkdir  ~/eev-current/tmp/lua-5.0/
tar -xvzf ~/eev-current/tmp/lua-5.0.tar.gz -C ~/eev-current/tmp/
cd     ~/eev-current/tmp/lua-5.0/

find * -name '*.[ch]' | sort > .files.ch
etags $(<.files.ch)

# (find-lua50file "INSTALL")
# (find-lua50file "config")
# (find-lua50file "etc/README")
# (find-lua50file "src/lib/loadlib.c")
cat >> config <<'%%%'

# --- Edrx's changes
# (find-lua50file "config" "dynamic loading on Unix systems")
LOADLIB= -DUSE_DLOPEN=1
DLLIB= -ldl
MYLDFLAGS= -Wl,-E
#
# (find-lua50file "config" "\n#USERCONF=")
USERCONF=-DLUA_USERCONFIG='"$(LUA)/etc/saconfig.c"' -DUSE_READLINE
EXTRA_LIBS= -lm -lreadline -ldl
%%%

make CC='gcc -g' test	|& tee omt
make CC='gcc -g' so	|& tee oms
# (find-lua50file "omt")
# (find-lua50file "oms")

#






#####
#
# A test for the installer: return beth's home dir to its pristine
# state and install eev there. Note: beth is one of the 3 scratch
# users in my machine; the other ones are aleph and gimel.
# 2004oct15
#
#####

# «test-install-beth»  (to ".test-install-beth")
# «to-install-beth»  (find-angg ".emacs" "to-install-beth")
# «test-beth»  (to ".test-beth")

# http://lists.gnu.org/archive/html/eev/2005-07/msg00017.html
# http://article.gmane.org/gmane.emacs.eev.devel/20
#
# (find-eevfile "eev-rctool")
# (find-eevfile "")

# Zero beth's home 
#
sudo         sh -c 'rm -Rv ~beth; mkdir ~beth; chown beth:beth ~beth'
sudo -u beth sh -c 'cd /etc/skel/; cp -av $(/bin/ls -A) ~beth/'
cd ~/eev-current/; tar -cvzf /tmp/eev.tgz $(sort .files)
xhost +

#
 (eechannel-xterm "beth")
su - beth
beth
set
mkdir -p ~/eev-current && cd ~/eev-current && tar -xzf /tmp/eev.tgz
./eev-rctool prepare
./eev-rctool install_rc
cd
DISPLAY=:0.0 emacs &

#
# (find-fline "~beth/")
# PS1="%d(%n:pe:eeg)# "
# PS1='\w(\u:bash:eeg)# '




#####
#
# check .files
# 2004oct17
#
#####

# «check-.files»  (to ".check-.files")
#
# (find-node "(coreutils)comm invocation" "three-column output")
# (find-sh "cd $EEV; sort .files | uniq -d")
# (find-sh ". $EE")
#
cd $EEVDIR/
echo ".files  find    both"
echo "------  ----    ----"
comm =(sort .files | grep -v '^$') \
     =(find .* * -type f | egrep -v 'OLD|~' | sort) \
  | tee ~/o

#
# (find-fline "$EEV/")
# (find-fline "$EEV/.files")
# (find-fline "~/o")

# (find-fline "$EEVE/")





(find-eevex "lua.e" "install-5.0.2")





# Local Variables:
# coding:            raw-text-unix
# modes:             (fundamental-mode emacs-lisp-mode)
# ee-anchor-format:  "«%s»"
# ee-comment-prefix: "#"
# End:
