

(find-es "emacs")



# «.eesteps»		(to "eesteps")
# «.my-ins»		(to "my-ins")
# «.propertize»		(to "propertize")
# «.cvs-emacs»		(to "cvs-emacs")
# «.keymap-property»	(to "keymap-property")
# «.display-property»	(to "display-property")
# «.kill-ring»		(to "kill-ring")
# «.allout-mode»	(to "allout-mode")




#####
#
# eesteps
# 2004oct21
#
#####

;;
;; «eesteps»  (to ".eesteps")
;; (eeb-eval)
;;
(defun eesteps-set (list)
  (setq eesteps-pos 0)
  (setq eesteps-list list))

(defun eesteps-do-step ()
  (interactive)
  (if (>= eesteps-pos (length eesteps-list))
      (error "No more steps"))
  (eval (nth eesteps-pos eesteps-list))
  (setq eesteps-pos (1+ eesteps-pos)))

(global-set-key [f12] 'eesteps-do-step)

;;
;; A test:
;; (eesteps-set '((insert "foobar") (backward-char 3) (insert "!")))

;; (find-eevexfile "tools.el")
;; (load-library "$EEVDIR/examples/tools.el")
;; (read-key-sequence "Key sequence: ")

(eek "C-x 4 C-h")
(eek "C-x 5 C-h")
(eek "C-x C-h")

;; (find-elnode "Keyboard Macros")
;; (find-elnode "Key Sequence Input")

;; (find-efunction 'read-kbd-macro)
;; (find-efunction 'edmacro-mode)
;; (read-kbd-macro   "C-x 2")
;; (read-kbd-macro   "C-x 2" t)
;; (read-kbd-macro   "<f12> 0" t)
;; (format-kbd-macro "\C-x2")
;; (format-kbd-macro "\C-x2" t)






#####
#
# my-ins
# 2004oct21
#
#####

;; «my-ins»  (to ".my-ins")
;; (find-elnode "Markers")
;; (find-elnode "Marker Insertion Types")
;;
(defun my-set-marker ()
  (setq my-marker (make-marker))
  (set-marker-insertion-type my-marker t)
  (set-marker my-marker (point)))

(defun my-ins (&rest stuff)
  (save-excursion
    (set-buffer (or (marker-buffer my-marker)
		    (error "my-marker points nowhere")))
    (goto-char my-marker)
    (apply 'insert stuff)))

;; a demo:
' (eesteps-set '(
    (next-line 2) (my-set-marker) (next-line 2)
    (my-ins "a") (my-ins "b") (my-ins "c")
    ))




#####
#
# propertize
# 2004aug28
#
#####

# «propertize»  (to ".propertize")
# (find-angg "elisp/ptext.el")
# (find-elnode "Special Properties")
# (find-elnode "Face Attributes")
# (find-elnode "Display Property")

(equal (propertize "foo" 'face 'foo)
       "foo")

(insert
 (propertize "a"
   'face '(:foreground "red" :background "#223344")
   'mouse-face '(:foreground "green")
))

(insert (propertize "abcd" 'face '(:font "10x20")))

# (find-sh "xlsfonts | sort | grep    ^-")
# (find-sh "xlsfonts | sort | grep -v ^-")
xterm -fn 10x20 &

Q: how to set a piece of text to a specific (non-fontset-ish) font?




#####
#
# emacs from the CVS
# 2004nov18
#
#####

# «cvs-emacs»  (to ".cvs-emacs")
# (find-node "(tar)gzip" "`-j'")
# (find-man "1 cvs")
# (find-node "(cvs)Top")
# (find-node "(cvs)Connecting via rsh")
# (find-node "(cvs)Connecting via rsh" "set `CVS_RSH' to `SSH'")
# (find-fsbot-answer "CvsEmacs")
# http://www.emacswiki.org/cgi-bin/wiki.pl/EmacsCVS
# http://www.emacswiki.org/cgi-bin/wiki.pl/EmacsCvsAndDebian
# http://angg.twu.net/.emacs.html#emacs-cvs

#
rm -Rv ~/bigsrc/emacs/
mkdir  ~/bigsrc/
cd     ~/bigsrc/
tar -C ~/bigsrc/ -xvjf ~/tmp/emacs-cvs.tar.bz2

#
cd     ~/bigsrc/
export CVS_RSH=ssh
cvs -z3 -d:ext:anoncvs@savannah.gnu.org:/cvsroot/emacs co emacs

cd     ~/bigsrc/
tar -cvjf ~/tmp/emacs-cvs.tar.bz2 emacs

#
cd     ~/bigsrc/emacs/
./configure      2>&1 | tee oc
find * -name '*.[ch]' | sort > .files.ch
make bootstrap   2>&1 | tee omb
make TAGS        2>&1 | tee omT

#




#####
#
# an rpn calculator
# 2004oct22
#
#####

;; (find-elinode "nil explained")
;; (find-elinode "let")
;;
;; (ee-once (eeb-eval))

(setq r-stack ())
(setq r-show nil)

(defun r-push (obj) (setq r-stack (cons obj r-stack)))
(defun r-pop  ()    (prog1 (car r-stack) (setq r-stack (cdr r-stack))))

(setq r-actions
      '((+    . (r-push (+ (r-pop) (r-pop))))
	(*    . (r-push (* (r-pop) (r-pop))))
	(-    . (let ((arg2 (r-pop)) (arg1 (r-pop))) (r-push (- arg1 arg2))))
	(/    . (let ((arg2 (r-pop)) (arg1 (r-pop))) (r-push (/ arg1 arg2))))
	(sqrt . (r-push (sqrt (r-pop))))
	(->   . (set (car r-extra-args) (r-pop)))
	))

(defun r-has-action (symbol)      (assoc symbol r-actions))
(defun r-get-action (symbol) (cdr (assoc symbol r-actions)))

(defun r-calc (ops)
  (while ops
    (setq op  (car ops))
    (setq ops (cdr ops))
    (cond ((numberp op) (r-push op))	            ; example: 2
	  ((and (symbolp op) (r-has-action op))     ; example: +
	   (setq r-extra-args nil)
	   (eval (r-get-action op)))
	  ((and (symbolp op) (boundp op))           ; example: pi
	   (poe (symbol-value op)))
	  ((and (listp op) op (r-has-action (car op))) ; example: (-> a)
	   (setq r-extra-args (cdr op))
	   (eval (r-get-action (car op))))
	  (t (error "I don't know how to execute: %S" op)))
    (if r-show (insert (format "%S\n" (reverse r-stack)))))
  r-stack)

;;
;;

(setq r-stack ())
(setq r-show nil)
(r-calc '(1 2 3 +))





#####
#
# the `keymap' and `display' text properties
# 2004sep13
#
#####

# «keymap-property»  (to ".keymap-property")
# (find-elnode "Format of Keymaps")
# (find-elnode "Special Properties" "`keymap'")

(let ((action (lambda () (interactive)
		(highlight-temporarily
		 (previous-char-property-change (point))
		 (next-char-property-change (point))))))
  (insert (propertize "press `?' on me"
		      'keymap `(keymap (?? . ,action)))))

# «display-property»  (to ".display-property")
# (find-angg ".emacs" "show-as-image")
# (find-elnode "Special Properties" "`display'")
# (find-elnode "Other Display Specs")
# (find-elnode "Image Descriptors")

(let* ((fname "~/IMAGES/kaoya.png")
       (image-data (with-temp-buffer
		     (set-buffer-multibyte nil)
		     (insert-file-contents-literally fname)
		     (buffer-string)))
       (display-props `(image :type png :data ,image-data)))
  (insert (propertize "?" 'display display-props)))





#####
#
# The kill ring
# 2003jan07
#
#####

# «kill-ring»  (to ".kill-ring")
# (find-enode "Kill Ring")
# (find-enode "Earlier Kills")

# (find-es "emacs")
# (describe-key [?\C-y])
# (describe-key [?\M-y])

# (find-efunction 'yank)
# (find-efunction 'yank-pop)
# (find-efunction 'kill-new)
# (find-evariable 'kill-ring)

(progn (setq kill-ring nil)
       (kill-new "An older kill")
       (kill-new "An old kill")
       (kill-new "A buried kill")
       (kill-new "The last kill"))

(describe-variable ' kill-ring)

(progn (yank) kill-ring)

(progn (yank 1) kill-ring)

(progn (yank 2) kill-ring)

(progn (yank 2) kill-ring)

# Not finished






#####
#
# allout-mode (in CVS Emacs)
# 2005aug11
#
#####

# «allout-mode»  (to ".allout-mode")
# (find-efile "allout.el")
# (find-efile "allout.el" "C-c C-n allout-next-visible-heading")
# (find-efunctiondescr 'allout-init)

(require 'allout)
(allout-init t)

(eesteps '(
  (find-efile "allout.el")
  ;; (allout-mode)
  (ee-goto-position ";;;_  = allout-show-bodies")
  "C-c C-h" "C-c C-s"
  "C-c C-h" "C-c C-s"
  "C-c C-h" "C-c C-s"
  ))








#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
