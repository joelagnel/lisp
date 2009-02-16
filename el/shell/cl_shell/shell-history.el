;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE:          shell-history.el
;;; DESCRIPTION:   Extensions to the code in shell.el for recording and 
;;;                yanking command history in a ring (vector).
;;; AUTHOR:        Eero Simoncelli, 
;;;                Vision Science Group, 
;;;                MIT Media Laboratory.
;;; CREATED:       March, 1989
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.  Refer to the GNU Emacs General Public License
;; for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights
;; and responsibilities.  It should be in a file named COPYING.  Among
;; other things, the copyright notice and this notice must be
;; preserved on all copies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To use this code, you have to insert this line in the function which sends
;;; strings to the shell process:
;;;      (shell-add-history <string>)

;;; For standard shells (as in shell.el), the function of interest is
;;; shell-send-input.  You can modify this function by putting the
;;; following in your .emacs file:
;;;
;;; (require 'shell)
;;; (defvar standard-shell-send-input (symbol-function 'shell-send-input))
;;; (defun shell-send-input ()
;;;   (interactive)
;;;   (funcall standard-shell-send-input)
;;;   (shell-add-history (buffer-substring last-input-start last-input-end)))

;;; The top-level commands are shell-yank-history,
;;; shell-yank-history-forward, shell-yank-matching-history, and
;;; shell-yank-matching-history-forward.  These cycle through the
;;; history ring, pulling out previous commands the user has typed
;;; (like using C-y, M-y to look at the Emacs kill-ring).  FOr the
;;; first two commands, the matches are inserted at the point.  The
;;; "matching" commands differ in that 1) If the user has already
;;; started typing a command, then the ring is searched for commands
;;; which match what has been typed (with leading whitespace trimmed)
;;; and 2) the yanks are inserted at the process-mark (beginning of
;;; new input).

;;; Recommended key bindings:
;;;  (define-key shell-mode-map "\M-\C-y" 'shell-yank-history)
;;;  (define-key shell-mode-map "\M-\C-z" 'shell-yank-history-forward)
;;; and (to match bindings of tcsh):
;;;  (define-key shell-mode-map "\M-p" 'shell-yank-matching-history)
;;;  (define-key shell-mode-map "\M-n" 'shell-yank-matching-history-forward)

;;; Relies only on a buffer variable called history-ring, which is
;;; created automatically by shell-add-history.  The ring is a vector.
;;; The first element of the ring is an index number (current
;;; insertion position).  The rest of the elements are the history
;;; strings.

(defconst *shell-history-default-length* 80)
(defvar *shell-history-min-string-length* 4)

;;; First element is index of current ring position.  This is where the
;;; next history string will be added.
(defun shell-make-history-ring (&optional length)
  (if (null length) (setq length *shell-history-default-length*))
  (let ((vect (make-vector (1+ length) nil))) ;make vector of nils
    (aset vect 0 1)		;initial index is 1
    vect))

(setq-default history-ring nil)   ;could set this to be a global history ring.

;;; Top level function for adding a string to the history ring.  If
;;; buffer has no history-ring, a new one is created.  Initial and
;;; trailing whitespace is trimmed.
(defun shell-add-history (string &optional buffer)
  (let ((original-buffer (current-buffer))
	length index)
    (set-buffer (or buffer original-buffer))
    (if history-ring nil
	(make-local-variable 'history-ring) ;make a new one if necessary
	(setq history-ring (shell-make-history-ring)))
    (setq index (aref history-ring 0))  ;read current ring position
    (setq length (length history-ring))
    (if (string-match "\\`[ \t\n]*" string)
	(setq string (substring string (match-end 0))))
    (if (string-match "[ \t\n]*\\'" string)
	(setq string (substring string 0 (match-beginning 0))))
    ;; check if string is empty or previous input was identical
    (if (or (< (length string) *shell-history-min-string-length*)
	    (equal string (aref history-ring (mod+1 (1- index) length))))
	nil				;don't bother if empty, keyword or repeat
	(aset history-ring index string)
	(aset history-ring 0 (mod+1 (1+ index) (length history-ring))))
    (set-buffer original-buffer)
    string))
	
;;; This should only be called from within the shell buffer.  It yanks
;;; from the history ring, inserting at the current point position.
(defun shell-yank-history (&optional forward-p)
  "Cycle backward through the history ring of commands typed to the shell. The history
ring has length determined by the global variable *shell-history-default-length*."
  (interactive)
  (let (history-string)
    (if (null history-ring) (error "No history ring for current buffer."))
    (setq history-string
	  (catch 'no-history
	    (cond ((or (eq last-command 'shell-yank-history)
		       (eq last-command 'shell-yank-history-forward))
		   (delete-region shell-history-insertion-position (point))
		   (shell-next-history-string history-ring "" forward-p))
		  (t (setq shell-history-insertion-position (point))
		     (shell-next-history-string history-ring "" forward-p t)))))
    (if (stringp history-string)
	(insert history-string)
	(beep)
	(message (concat (if history-string "Beginning"  "End") " of history ring.")))))

(defun shell-yank-history-forward ()
  "Cycle forward through the history ring of commands typed to the shell.  The history
ring has length determined by the global variable *shell-history-default-length*."
  (interactive)
  (shell-yank-history t))

;;; This should only be called from within the shell buffer.  Yanks from
;;; history ring the next command matching the shell-history-substring.
(defun shell-yank-matching-history (&optional forward-p)
  "Cycle backward through the history ring of commands typed to the shell.  If the 
user has begun typing a new command, then looks for a command matching the typed
substring.  The history strings are inserted at the process-mark (where the next
user input will begin). The history ring has length determined by the global variable 
*shell-history-default-length*."
  (interactive)
  (let (history-string proc-mark)
    (if (null history-ring) (error "No history ring for current buffer."))
    (if (or (null (get-buffer-process (current-buffer)))
	    (null (setq proc-mark
			(process-mark (get-buffer-process (current-buffer))))))
	(error "No process-mark for current buffer."))
    (setq history-string
	  (catch 'no-history
	    (if (or (eq last-command 'shell-yank-matching-history)
		    (eq last-command 'shell-yank-matching-history-forward))
		(shell-next-history-string
		 history-ring shell-history-substring forward-p)
		;; Find first non-whitespace char after process-mark:
		(setq shell-history-insertion-position
		      (save-excursion
			(goto-char proc-mark)
			(skip-chars-forward " \t\n")
			(point)))
		;; Goto beginning of trailing whitespace:
		(goto-char (point-max))
		(skip-chars-backward " \t\n" shell-history-insertion-position)
		(setq shell-history-substring 
		      (buffer-substring shell-history-insertion-position (point)))
		(shell-next-history-string
		 history-ring shell-history-substring forward-p t))))
    ;; Assumes the point is at the end of the inserted string!
    (delete-region shell-history-insertion-position (point))
    (if (stringp history-string)
	(insert history-string)
	(beep)
	(message (concat (if history-string "Beginning"  "End")
			 " of history ring"
			 (if (> (length shell-history-substring) 0)
			     ": No more matches."  ".")))
	(insert shell-history-substring)
	(goto-char (point-max)))))

(defun shell-yank-matching-history-forward ()
"Cycle forward through the history ring of commands typed to the shell.  If the 
user has begun typing a new command, then looks for a command matching the typed
substring.  The history strings are inserted at the process-mark (where the next
user input will begin). The history ring has length determined by the global variable 
*shell-history-default-length*."
  (interactive)
  (shell-yank-matching-history t))

;;; These can be global since they only apply to the current buffer.
(defvar shell-history-insertion-position 0
  "Starting buffer position of history insertions.")

(defvar shell-history-substring ""
  "Substring user had started typing before they requested a history yank.")

(defvar shell-history-ring-position 0
  "Index of the last yank -- for use when doing sequential yanks.")

(defun mod+1 (value ring-size)
  (1+ (% (+ ring-size value -2) (1- ring-size))))

;;; Get next string from the-ring which matches substring.  Throws the
;;; value of forward-p to 'no-history tag if it reaches the end
;;; (beginning) of the ring.
(defun shell-next-history-string
    (the-ring substring forward-p &optional first-yank-p)
  (let ((incr (if forward-p 1 -1))
	(length (length the-ring))
	history-entry)
    (if first-yank-p (setq shell-history-ring-position (aref the-ring 0)))
    (if (and forward-p (= shell-history-ring-position (aref the-ring 0)))
	(throw 'no-history forward-p))
    (setq shell-history-ring-position
	  (mod+1 (+ shell-history-ring-position incr) length))
    (if (= shell-history-ring-position (aref the-ring 0))
	(throw 'no-history forward-p))
    (setq history-entry (aref the-ring shell-history-ring-position))
    (while (or (null history-entry)
	       (not (eq 0 (string-match (regexp-quote substring) history-entry))))
      (setq shell-history-ring-position
	    (mod+1 (+ shell-history-ring-position incr) length))
      (if (= shell-history-ring-position (aref the-ring 0))
	  (throw 'no-history forward-p))
      (setq history-entry (aref the-ring shell-history-ring-position)))
    history-entry))

