;; This is the `examples/demos.e' file of GNU eev.
;; Version: 2005jul24

;; Each of the demos in this file is a program of the form:
;;
;;  (eesteps
;;    '(...
;;      ...
;;      ))
;;
;; To execute it, put the cursor after the final "))" and type `M-e';
;; that will load a series of actions ("steps") into Emacs, and you
;; should get a message like
;;
;;   (27 steps stored - use <f12> to execute a step)
;;
;; at the "echo area" at the bottom of the screen. Then type <f12>
;; several times, _paying a lot of attention to everything what
;; happens_. When there are no more steps left you'll get a message
;; like "No more steps" in the echo area and Emacs will beep on you.

;; New things, jul2005:
;; «.eewalk-demo4»	(to "eewalk-demo4")



;;;;;
;;
;; A first demo about evaluation.
;; C-x C-e evaluates the Lisp expression before point (`eval-last-sexp').
;; C-x C-e is a standard Emacs key, but we'll use M-e and M-E much more.
;; M-e and M-E are defined in eev.el, and are *not* standard!!!
;; Note: M-E is meta-shift-e.
;;
;;;;;

(eesteps
 '((progn
     (eekv "C-x b TEMP")
     (eekl ";; Keys for evaluating Lisp expressions (a.k.a. \"sexps\")")
     (eekl ";; C-x C-e   -- eval-last-sexp")
     (eekl ";;     M-E   -- eek-eval-last-sexp (defined in eev.el; like C-x C-e)")
     (eekl ";;     M-e   -- eek-eval-sexp-eol  (defined in eev.el; like C-e M-E)")
     (eekl ";; M-0 M-E   -- flash the sexp instead of evaluating it")
     (eekl ";; M-0 M-e   -- flash the sexp instead of evaluating it")
     (eekl ";; The M-0 is a \"prefix argument\". (More about prefix args later).")
     (eekl ";; Note that the result of the evaluation appears at the")
     (eekl ";; \"echo area\", at the bottom of the screen.")
     (eekl "")
     (eekl "(+ 1 2)")
     (eekl "(+ (+ 1 2) (+ 3 4))")
     (eekv "<up>")
     (eekv "<up>")
   )
   (eekv "C-e     ;;; moves to the end of the line (like <end>)")
   (message "next key: C-x C-e")
   (eek "C-x C-e")
   (eek "<down>")
   (eek "C-e")
   (message "next key: C-x C-e")
   (eek "C-x C-e")
   (message "Now we'll use M-0 M-E to highlight the sexp before point.")
   (eekv "M-0 M-E")
   (eek "<left>")
   (eekv "M-0 M-E")
   (eek "8*<left>")
   (eekv "M-0 M-E")
   (message "next key: M-E   (which is equivalent to C-x C-e)")
   (eek "M-E")
   (eek "8*<right>")
   (eek "M-E")
   (eek "<right>")
   (eek "M-E")
   (message "next:  C-x k TEMP   (to kill the TEMP buffer)")
   (eek "C-x k TEMP")
   ))


;; End of demo 1

;;;;;
;;
;; sexps can span several lines and contain comments (plus indentation?)
;;
;;;;;



;;;;;
;;
;; splitting and uniting windows, choosing buffers
;;
;;;;;

(eesteps
 '((eekv "C-x 1   ;;; delete-other-windows")
   (eekv "C-x b ONE")
   (eekv "C-x 3   ;;; split-window-horizontally (= side to side)")
   (eekv "C-x o   ;;; other-window")
   (eekv "C-x b TWO")
   (eekv "C-x 2   ;;; split-window-vertically (= one above the other)")
   (eekv "C-x 2")
   (eekv "C-x o")
   (eekv "C-x b THREE")
   (eekv "C-x 3")
   (eekv "C-x o")
   (eekv "C-x b FOUR")
   (eekv "C-x o")
   (eekv "C-x b FIVE")
   (eekv "C-x o")
   (eekv "C-x o")
   (eekv "C-x o")
   (eekv "C-x o")
   (eekv "C-x o")
   (eekv "C-x 1")
   (eekv "C-x k FIVE")
   (eekv "C-x k FOUR")
   (eekv "C-x k THREE")
   (eekv "C-x k TWO")
   (eekv "C-x k ONE")
   ))

;; End of demo 2



;;;;;
;;
;; describe-key, links to the source code, font-lock-mode
;;
;;;;;




;;;;;
;;
;; Saving a block of shell commands within a delimited region with <f3>
;; and executing them in a shell with `ee'
;;
;;;;;

(eesteps
 '((progn (cd "/tmp") (message "M-x cd /tmp"))
   (eekv "C-x b TEMP")
   ;; (eekv "M->")
   "RET"
   "#" (eekv "C-q C-o") "RET"
   (eekl "# comment")
   (eekl "echo $[1+2]")
   "#" (eekv "C-q C-o") "RET"
   "3*<up>"
   (eekv "<f3>")
   (eekv "M-x shell")
   (eekl "ee")
   (eekl "# how `ee' works: `ee' is a shell function...")
   (eekl "which ee")
   (eekl "# ...which \"sources\" $EE; EE is an environment variable,")
   (eekl "echo $EE")
   (eekl "# containing the name of a temporary file.")
   (eekl "# Emacs saved the temporary script there.")
   (eekl "# Let's inspect the contents of the file:")
   (eekl "cat $EE")
   ;;
   (eekl "# Now we will kill the shell buffer and")
   (eekl "# the temporary buffer.")
   (eekv "C-x k *shell*")
   (eekv "C-x k TEMP")
   ))

;; End of demo 3



;;;;;
;;
;; downloading (with psne-maybe), unpacking, and compiling
;;
;;;;;



;;;;;
;;
;; creating hyperlinks, refining them with M-h M-y, moving them around
;;
;;;;;

(eesteps
 '((find-enode "Lisp Eval")
   (search-forward "`defvar'")
   (eekvr "C-SPC C-SPC 8*<left>")
   (eekv "M-w       ;;; kill-ring-save")
   (eekv "M-h M-i   ;;; find-einfo-links")
   (search-forward "find-enode")
   (eekv "M-h M-2   ;;; eemklinks-duplicate-this-line")
   (eekv "M-h M-y   ;;; eemklinks-yank-pos-spec")
   "C-a"
   (eekvr "C-SPC C-SPC <down>")
   (eekv "M-w       ;;; kill-ring-save")
   (eekv "C-x b TEMP")
   (eekv "C-y       ;;; yank")
   (message "Next key:  M-e")
   "M-e"
   (eekv "q         ;;; Info-exit")
   (eekv "C-x k TEMP")
   (eekv "M-K       ;;; bury-buffer")
   ))

;; End of demo 4




;;;;;
;;
;; variables
;;
;;;;;

(setq a 22)
(setq a 44)
(makunbound 'a)
a
(* a 100)

(defun foo (a) (* a 100))
(defun foo (a) (* a a))

Functions can also be redefined, bound and unbound. Pretty-printing.
(find-eevfile "EMACS" "fmakunbound")

;;;;;
;;
;; links to debian packages
;;
;;;;;

;;;;;
;;
;; eechannel (just inside emacs)
;;
;;;;;



(setq show-trailing-whitespace t)



;;;
;;; Demos using eewalk
;;;

;;
;; «eewalk-demo4»  (to ".eewalk-demo4")
;; (eewr-bounded)
;; A demo about generating and refining hyperlinks
;; (find-eev "article/eev.txt" "generating-hyperlinks")

;; Open a certain info page and mark a string
(find-enode "Lisp Eval")
(search-forward "`defvar'")
"C-SPC C-SPC 8*<left>"		"M-w"

;; Generate the hyperlink
"M-h M-i"	;; find-einfo-links
(search-forward "find-enode")
"M-h M-2"	;; eemklinks-duplicate-this-line
"M-h M-y"	;; eemklinks-yank-pos-spec

;; Kill/cut the hyperlink
"C-a"	"C-SPC C-SPC <down>"	"M-w"

;; Paste it to a (temporary) buffer and test it
"C-x b TEMP"	"C-y"
(eek0 "# Now test the link\r")
"2*<up>"	"M-e"

;; Clean up
"q"	"C-x k TEMP"	"M-K"

;;






#  Local Variables:
#  coding:               raw-text-unix
#  mode:                 emacs-lisp
#  modes:               (emacs-lisp-mode fundamental-mode)
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
