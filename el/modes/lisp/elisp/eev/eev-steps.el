;;; eev-steps.el -- single-steppers.
;; I'm trying to move some block from eev.el to this file,
;; but I haven't moved anything yet - just copied some stuff -
;; and this file isn't loadable yet.

;; Stuff that I wrote in 2006aug17:
;;
;; Introduction
;; ============

;; Think of Emacs as being a huge Turing Machine in which some of the
;; memory cells control what is shown on the screen. Then, following
;; an elisp hyperlink - like, say, (find-node "(elisp)Undo") - means
;; going from one Emacs state to another one, in a way that is usually
;; reversible; killing or burying the new Info buffer will in most
;; cases make Emacs display what it was displaying before.
;; 
;; In a sense, (find-node "(elisp)Undo") "finds" - like `find-file' does,
;; and like web browsers do - an Info node; in another sense, it "finds"
;; a new Emacs state, in which the specified Info node is shown on the
;; current buffer.
;; 
;; Sometimes we want to "find" - or rather, to "find out" - the effect a
;; certain series of operations; we just want to apply those operation to
;; the current Emacs state to go to another state. For example,
;; 
;;  (find-eek "C-x 4 C-h")
;; 
;; finds the "effect of the key sequence" `C-x 4 C-h'. Incidentally, this
;; creates a buffer with some hyperlinks, and it is an operation that is
;; as reversible as a `find-file' or `find-node' - but this doesn't need
;; to be the case always.
;; 
;; A `progn' expression can also be seen as an elisp hyperlink to a new
;; Emacs state.
;;
;;
;; The steppers
;; ============
;; Sometimes we have series of steps that we want to execute, but for one
;; reason or another we have to execute those steps one by one, not all
;; at once; what we do is that we "program" the list of steps somehow,
;; and we create a button - actually a key; we bind a command to it -
;; that when we press it Emacs interprets it as meaning "now execute the
;; next step in the list".
;;
;; Currently there are four different steppers, and they are not unified.
;; They implement their "get the next step from the list" things in
;; different ways, and they use different keys as their "Next" buttons.
;; Let me explain them one by one, in historical order, starting by the
;; ones that appeared earlier.
;;
;;  `ee-yank-one-line' - the last killed string is considered as a
;;    list of steps; in this case "get the next step" means "remove
;;    the first line from the string at the top of the kill ring",
;;    and "execute the step" means insert the removed line at point
;;    and then run the command bound to RET. This is useful, for
;;    example, to send several lines in sequence to an `M-x shell' or
;;    `M-x eshell' buffer as if we were typing the lines one by one.
;;  `eesteps'
;;  `eechannel'
;;  `eepitch'

;; The rest was written in 2006jan10 or earlier.
;; The corresponding portions of code in eev.el have probably been
;; changed in eev.el; I need to sync the functions.

;; See:
;; http://article.gmane.org/gmane.emacs.eev.devel/28
;; http://article.gmane.org/gmane.emacs.eev.devel/32
;; http://article.gmane.org/gmane.emacs.eev.devel/33
;; http://lists.gnu.org/archive/html/eev/2005-12/msg00003.html
;; http://lists.gnu.org/archive/html/eev/2005-12/msg00007.html
;; http://lists.gnu.org/archive/html/eev/2005-12/msg00009.html
;; http://lists.gnu.org/archive/html/eev/2006-01/msg00000.html

;; Copyright (C) 2004,2005,2006 Free Software
;; Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Author:     Eduardo Ochs <edrx@mat.puc-rio.br>
;; Maintainer: Eduardo Ochs <edrx@mat.puc-rio.br>
;; Version:    2005dec20
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex

;;; Commentary:

;; For a description of what eev and e-scripts are please see the
;; README (and (soon) the manual in texinfo format).
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-steps.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-steps.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>

;; Structure of this file:
;; ...



(defvar eesteps-pos 0)
(defvar eesteps-list ())
(defvar eechannel-default nil)
(defvar eepitch-code '(error "eepitch not set up"))
(defvar eepitch-target-buffer nil)

;; Hey, I'm not sure if the code for the help page should be here...

(defvar eev-help-page-file-name "$EEVTMPDIR/HELP")
(defvar eev-help-previous-buffer nil
  "Non-nil when we're on a help page buffer. Used by `eev-help-page'.")

(make-variable-buffer-local 'eev-help-previous-buffer)

(defun ee-bol () (point-at-bol))
(defun ee-eol () (point-at-eol))
(defun ee-eval-string (str)
  "Wrap STR in a progn then read it and eval it.
Examples: (ee-eval-string \"(+ 1 2) (* 3 4) ;; this returns 12=3*4\")
          (ee-eval-string \";; this returns nil\")"
  (eval (read (concat "(progn\n" str "\n)"))))





;;;                 _                 
;;;   ___  ___  ___| |_ ___ _ __  ___ 
;;;  / _ \/ _ \/ __| __/ _ \ '_ \/ __|
;;; |  __/  __/\__ \ ||  __/ |_) \__ \
;;;  \___|\___||___/\__\___| .__/|___/
;;;                        |_|        
;;;
;;; hyperlinks to key sequences and series of Emacs actions
;;;

(defun eesteps (list)
  "Set the LIST of steps that `eesteps-do-step' will execute.\n
Here's an example: run\n
  (eesteps '(\"C-x b * scratch * RET   ;;; change to the buffer *scratch*\"
             \"foobar\"
             \"3*<left>\"
             (insert \"!\")))\n
then type \\[eesteps-do-step] four times.\n
Each step is either a string -- meaning a series of keys, in the
format used by `edmacro-mode' -- or a sexp to be evaluated."
  (setq eesteps-pos 0)
  (setq eesteps-list list)
  `(,(length list) steps stored - use <f12> to execute a step))

(defun eek (s &optional e count)
  "Execute the region between S and E (or the string S) as a keyboard macro.
See `edmacro-mode' for the exact format.\n
An example: (eek \"C-x 4 C-h\")"
  (interactive "r")
  (execute-kbd-macro (read-kbd-macro (ee-se-to-string s e)) count))

(defun eek0 (kbmacro &optional count)
  "This is similar to `eek', but uses the low-level formats for macros.
Example: (eek \"\\C-x4\\C-h\")"
  (execute-kbd-macro kbmacro count))

(defun eesteps-perform (step &rest rest)
  (if (stringp step)
      (eek step)
    (eval step))
  (if rest (apply 'eesteps-eval rest)))

(defun eesteps-do-step (&optional arg)
  (interactive "P")
  (if (>= eesteps-pos (length eesteps-list))
      (error "No more steps"))
  (if (eq arg 0)
      (message "Next step: %d = %S" eesteps-pos (nth eesteps-pos eesteps-list))
    (eesteps-perform (nth eesteps-pos eesteps-list))
    (setq eesteps-pos (1+ eesteps-pos))))

;; (defun eesteps-do-step ()
;;   (interactive)
;;   (if (>= eesteps-pos (length eesteps-list))
;;       (error "No more steps"))
;;   (let ((step (nth eesteps-pos eesteps-list)))
;;     (cond ((stringp step) (eek step))
;;           (t (eval step))))
;;   (setq eesteps-pos (1+ eesteps-pos)))




;;;                                           _     _      
;;;   ___  _____   __     _ __   _____      _| |__ (_) ___ 
;;;  / _ \/ _ \ \ / /____| '_ \ / _ \ \ /\ / / '_ \| |/ _ \
;;; |  __/  __/\ V /_____| | | |  __/\ V  V /| |_) | |  __/
;;;  \___|\___| \_/      |_| |_|\___| \_/\_/ |_.__/|_|\___|
;;;                                                        
(defun eev-newbie ()
  (interactive)
  (setq debug-on-error nil)
  (setq eval-expression-debug-on-error nil)
  ;; (setq pop-up-windows nil)
  (eev-mode 1)
  (message "Newbie settings activated.  Have you tried `M-x eev-demos'?"))


;;;                           _                          
;;;   ___  _____   __      __| | ___ _ __ ___   ___  ___ 
;;;  / _ \/ _ \ \ / /____ / _` |/ _ \ '_ ` _ \ / _ \/ __|
;;; |  __/  __/\ V /_____| (_| |  __/ | | | | | (_) \__ \
;;;  \___|\___| \_/       \__,_|\___|_| |_| |_|\___/|___/
;;;                                                      
;;; eev-newbie and eev-demos
;;;

(defun eekl (str &rest rest)
  (eek0 (concat str "\r"))
  (if rest (apply 'eekl rest)))

(defun eekv (str) (eek str) (message str))

;; A hack for showing the region (temporarily) in Emacs <= 21.1.3.
;; These functions are used by some demos. They may be removed in the future.
;; Note that in recent Emacsen C-SPC C-SPC <movement> highlights the region:
;; (find-efile "ChangeLog" "temporary transient-mark-mode")
;; (find-eetcfile "NEWS" "C-SPC C-SPC; this enables Transient Mark mode")
;;
(defun eekr  (str) (eek str)  (eeflash (point) (mark)))
(defun eekvr (str) (eekv str) (eeflash (point) (mark)))

(defun eev-demos (arg)
  (interactive "P")
  (find-eevexfile "demos.e")
  (if (and arg (>= arg 1) (<= arg 5))
      (progn (ee-goto-position (format "\n;; End of demo %d\n" arg))
	     (forward-line -2)
	     (message "Type M-e to load the demo above the cursor."))
    (let ((message-truncate-lines nil))
      (message (concat "Use `M-1 M-x eev-demos' to go to the 1st demo,\n"
		       "`M-2 M-x eev-demos' for the 2nd demo, etc (up to 4).")))))


;;;  _          _                                
;;; | |__   ___| |_ __    _ __   __ _  __ _  ___ 
;;; | '_ \ / _ \ | '_ \  | '_ \ / _` |/ _` |/ _ \
;;; | | | |  __/ | |_) | | |_) | (_| | (_| |  __/
;;; |_| |_|\___|_| .__/  | .__/ \__,_|\__, |\___|
;;;              |_|     |_|          |___/      
;;;
;;; eev-help-page
;;;

(defun eev-help-page ()
  "Switch to the eev help page buffer, or, if we're already on it, switch back."
  (interactive)
  (if eev-help-previous-buffer		       ; are we on a help page buffer?
      (if (buffer-live-p eev-help-previous-buffer)    ; can we go back?
	  (switch-to-buffer eev-help-previous-buffer) ; yes: switch back
	(bury-buffer))				      ; no: bury the help buffer
    (let ((buffer (current-buffer)))	       ; we're not on a help page bufer.
      (find-fline eev-help-page-file-name)     ; visit the help file
      (setq eev-help-previous-buffer buffer)))) ; and mark it as a help buffer



;;;
;;; more tools
;;;

;; ee-flatten is obsolte - using backticks is better

;; (ee-flatten '((1 2 3) (4 5) (((6)) 7) nil nil 8 9))
;; (ee-flatten '(1 2 3) '(4 5) '(((6)) 7) nil nil 8 9)
;;
(defun ee-flatten (obj &rest rest)
  (cond (rest (append (ee-flatten obj) (ee-flatten rest)))
	((null obj) nil)
	((listp obj) (append (ee-flatten (car obj)) (ee-flatten (cdr obj))))
	(t (list obj))))

(defun ee-read-file (fname)
  (with-temp-buffer
    (insert-file-contents fname)
    (buffer-string)))

(defun ee-no-trailing-nl (str)
  (replace-regexp-in-string "\n$" "" str))



;;;                    _    
;;;  _   _  __ _ _ __ | | __
;;; | | | |/ _` | '_ \| |/ /
;;; | |_| | (_| | | | |   < 
;;;  \__, |\__,_|_| |_|_|\_\
;;;  |___/                  
;;;
;;; yanking lines one of a time inside emacs
;;;

(defun ee-yank-one-line ()
  "Yank the first line of the killed text and do a RET.
Insert the first line from the latest kill-ring entry and run the
action associated to the RET key. The kill-ring entry is then
altered so that subsequent calls of yank-first-line will yank
successive lines.

As a special case, if the first line of the kill starts with
\"\" then evaluate the rest of it instead of yanking it, using
`ee-eval-string'.

For an example of usage see: (find-efunctiondescr 'eestore)"
  (interactive)
  (let ((bigstr (car kill-ring)))
    (if (equal bigstr "") (error "No more lines"))
    (string-match "^\\([^\n]*\\)\\(\n\\|$\\)" bigstr)
    (let ((line (match-string 1 bigstr))           ; first line from the kill
	  (rest (substring bigstr (match-end 0)))) ; rest of the kill
      (if (string-match "^\\(.*\\)" line)         ; lines with a red star
	  (ee-eval-string (match-string 1 line))   ; are eval'ed
	(insert line)			           ; other lines are "typed"
	(call-interactively (key-binding "\r")))   ; and then we do a RET
      (setcar kill-ring rest))))		   ; remove the first line

(defun eestore (s &optional e)
  "Store the region between S and E in the kill ring.
This is especially useful in conjunction with `ee-yank-one-line'.
Here's an example; to run it eval the `eestore-bounded', then
type `\\[ee-yank-one-line]' several times.

#
 (shell) ;; (eestore-bounded)
echo $[1+2]
echo foo
#
"
  (kill-new (ee-se-to-string s e))
  (format "Stored in the kill-ring"))

(eeb-define 'eestore-bounded  'eestore 'ee-delimiter-hash  nil t t)




;;;
;;; Sending lines to processes running in other Emacs buffers
;;;

;; NOTE: this is Rubikitch's "eethrow" function, only a bit less fragile.
;; See: http://lists.gnu.org/archive/html/eev/2005-12/msg00007.html
;;      http://article.gmane.org/gmane.emacs.eev.devel/32

(defun eepitch-prepare (code)
"Run CODE, remember its buffer, then split the frame to show also that buffer.
This function is usually called through the macro `eepitch'.
See `eepitch-this-line' for an example of use."
  (let ((pop-up-windows t)
	(same-window-buffer-names nil))
    (save-window-excursion
      (setq eepitch-code code)
      (eval code)
      (setq eepitch-target-buffer (current-buffer)))
    (display-buffer eepitch-target-buffer)))

(defmacro eepitch (code)
"Call `eepitch-prepare'; this macro quotes its argument CODE, just like setq.
See `eepitch-this-line' for an example of use."
  `(eepitch-prepare ',code))

(defun eepitch-this-line ()
"Send the current line to another buffer (in another window).
When the line starts with \"\" execute it as Lisp instead of sending it.
If the target buffer (stored in `eepitch-target-buffer') is not
visible then make it become visible again, by running the code
stored in `eepitch-code' with `eepitch-prepare'.
Here is an example (execute each line with \\[eepitch-this-line]):\n
 (eepitch (shell))
echo foo
 (message (format \"%S\" `(eepitch ,eepitch-code)))
 (delete-other-windows)
echo bar\n\n"
  (interactive)
  (let ((line (buffer-substring (ee-bol) (ee-eol)))) ; contents of this line
    (if (string-match "^\\(.*\\)" line)             ; lines with a red star
        (ee-eval-string (match-string 1 line))       ; are eval'ed
      (if (not (get-buffer-window eepitch-target-buffer))  ; if we need then
	  (eepitch-prepare eepitch-code))   ; make the target window visible
      (save-selected-window
	(select-window (get-buffer-window eepitch-target-buffer))
	(insert line)                              ; other lines are "typed"
	(call-interactively (key-binding "\r"))))  ; and then we do a RET
    (next-line 1)))



;;;
;;; starting background processes
;;;

(defun eebg-channel-xterm (channel &optional prog-and-args xterm-args)
  "This is the low-level way of creating an xterm listening on channel CHANNEL.
See `eechannel-xterm'."
  (interactive "sChannel: ")
  (apply 'start-process (format "xterm (channel %s)" channel) "*Messages*"
	 (ee-flatten
	  "xterm" "-T" (concat "channel " channel) xterm-args "-e"
	  (ee-expand "$EEVDIR/eegchannel") channel
	  (or prog-and-args (ee-expand "$SHELL")))))



;;;
;;; sending strings to external programs through "channels"
;;;

(defun eechannel-pidfile (channel)
  (ee-expand (format "$EEVTMPDIR/eeg.%s.pid" channel)))
(defun eechannel-strfile (channel)
  (ee-expand (format "$EEVTMPDIR/eeg.%s.str" channel)))

(defun eechannel-send (channel str)
  (if (not channel) (setq channel eechannel-default))
  (ee-write str nil "" "" (eechannel-strfile channel))
  (find-sh0 (format "kill -USR1 $(cat %s)" (eechannel-pidfile channel))))

(defun eechannel (channel &optional str)
  (interactive "sDefault channel: ")
  (if (not str)
      (setq eechannel-default channel)
    (eechannel-send channel str)))

(defun eechannel-do-this-line () (interactive)
  (let ((line (buffer-substring (ee-bol) (ee-eol)))) ; contents of this line
    (if (string-match "^\\(.*\\)" line)             ; lines with a red star
	(ee-eval-string (match-string 1 line))       ; are eval'ed
      (eechannel-send nil (concat line "\n")))       ; other lines are sent
    (next-line 1)))			             ; go down

(defun eech (s &optional e)		; bad name?
  (interactive "r")
  (eechannel-send eechannel-default (ee-se-to-string s e)))

(eeb-define 'eech-bounded 'eech 'ee-delimiter-hash nil t t)



;;;
;;; sending strings through "channels": high-level functions
;;;

(defun ee-pid-running-p (pid)
  "Return t if a process with pid PID is running. This is linux-specific.
This function just checks if a file /proc/<pid> exists.

If you need something that is kernel-independent then the
following idea might work: run \"ps PID\", discard the output,
test its exit code."
  (file-exists-p (format "/proc/%s" pid)))

(defun eechannel-pid (channel)
  "Note: this function returns either a pid (as a string) or nil."
  (let ((pidfile (eechannel-pidfile channel)))
    (if (file-exists-p pidfile) (ee-no-trailing-nl (ee-read-file pidfile)))))

(defun eechannel-running-p (channel)
  "Returns t if there is a process listening on CHANNEL."
  (let ((pid (eechannel-pid channel)))
    (if pid (ee-pid-running-p pid))))

(defun eechannel-xterm (channel &optional prog-and-args xterm-args)
  "If there's no process listening on CHANNEL then create an xterm there.
This function always sets the default channel to CHANNEL.
PROG-AND-ARGS and XTERM-ARGS are lists of strings: see `eebg-channel-xterm'."
  (interactive "sChannel: ")
  (eechannel channel)
  (if (eechannel-running-p channel)
      (message "Reusing channel %s" channel)
    (eebg-channel-xterm channel prog-and-args xterm-args)))

(defun eechannel-kill (channel)
  "Kill the process associated to channel CHANNEL."
  (find-sh0 (format "kill -9 $(cat %s)" (eechannel-pidfile channel))))



;;;
;;; sending blocks through channels
;;;

(defun eevnow (s &optional e)
  (interactive "r")
  (eev s e)
  (eech "ee\n"))

(eeb-define 'eevnow-bounded 'eevnow 'ee-delimiter-hash nil t t)

(defmacro ee-at (anchor &rest body)
  `(save-excursion
     (ee-goto-position (format ee-anchor-format ,anchor))
     . ,body))

(defmacro ee-at-file (fname anchor &rest body)
  `(with-current-buffer (find-file-noselect (ee-expand ,fname))
     (ee-goto-position (format ee-anchor-format ,anchor))
     . ,body))

(defun eevnow-at (anchor)
"Move temporarily to the anchor ANCHOR and run `eevnow-bounded' there.
If EE-ARG is non-nil then work as a hyperlink instead: just jump to ANCHOR.
This function is typically used in red-star lines inside F9-able blocks:
a line like\n
 (eevnow-at \"anchorname\")\n
works as a kind of subroutine call when executed with F9 (when F9 is
bound to `eechannel-do-this-line'), and as a hyperlink when run with,
say, M-2 M-2 M-e (when M-e is bound to `eek-eval-sexp-eol')."
  (if ee-arg (to anchor)
    (ee-at anchor (ee-once (eevnow-bounded)))))

(defun eevnow-at-file (fname anchor)
"Move temporarily to file FNAME, at anchor ANCHOR, run `eevnow-bounded' there.
This function is similar to `eevnow-at'."
  (if ee-arg (find-anchor fname anchor)
    (ee-at-file fname anchor (ee-once (eevnow-bounded)))))




(provide 'eev-steps)



;; Local Variables:
;; mode:              outline-minor
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-anchor-format:  "defun %s "
;; ee-comment-prefix: ";;"
;; no-byte-compile:   t
;; End:
