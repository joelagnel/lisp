;;;; autocue.el
;;; Time-stamp: <2005-01-19 09:51:10 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'autocue)

(defvar autocue-start-hooks nil
  "Hooks to run on starting autocue.")

(defvar autocue-keymap (make-sparse-keymap "Autocue")
  "Keymap for commands while scrolling automatically through buffer.
Not intended to be reached from other keymaps, but looked up explictly.")

(defvar autocue:secs-per-step 1
  "How many seconds to pause per step.")

(defvar autocue:lines-per-step 1
  "How many lines to move per step.")

(defvar autocue:continue t
  "Whether to continue")

(defvar autocue:message-keep 1
  "How many more steps to leave a message up for.")

(defun autocue:faster ()
  "Speed up autocue by reducing the time interval.."
  (interactive)
  (setq autocue:secs-per-step (max 1 (1- autocue:secs-per-step)))
  (autocue-show-params))

(defun autocue:slower ()
  "Slow down autocue by increasing the time interval."
  (interactive)
  (setq autocue:secs-per-step (1+ autocue:secs-per-step))
  (autocue-show-params))

(defun autocue:bigger ()
  "Speed up autocue by increasing the step size."
  (interactive)
  (setq autocue:lines-per-step (1+ autocue:lines-per-step))
  (autocue-show-params))

(defun autocue:smaller ()
  "Slow down autocue by decreasing the step size."
  (interactive)
  (setq autocue:lines-per-step (max 1 (1- autocue:lines-per-step)))
  (autocue-show-params))

(defun autocue-show-params ()
  "Show the autocue params"
  (message "%d lines per %d seconds" autocue:lines-per-step autocue:secs-per-step)
  (setq autocue:message-keep 2)
  )

(defvar autocue:suspended-file-names nil
  "Alist of suspended autocue files to positions in them.")

(defvar autocue:suspended-buffer-names nil
  "Alist of suspended autocue buffers to positions in them.
Buffers having a filename associated with them go onto
autocue:suspended-file-names instead, which see.")

(defun autocue:find-suspended-buffer-descr (&optional create)
  "Find the cons representing a suspended buffer."
  (let ((name (buffer-file-name)))
    (if name
	(let* ((truename (file-truename name))
	       (pair (assoc truename autocue:suspended-file-names)))
	  (when (and create (null pair))
	    (setq pair (cons truename 0)
		  autocue:suspended-file-names (cons pair autocue:suspended-file-names)))
	  pair)
      (let* ((buf (buffer-name))
	     (pair (assoc buf autocue:suspended-buffer-names)))
	(when (and create (null pair))
	  (setq pair (cons buf 0)
		autocue:suspended-buffer-names (cons pair autocue:suspended-file-names)))
	pair))))

(defun autocue:put-aside ()
  "Put this buffer aside, remembering where we were in it."
  (interactive)
  (setq autocue:continue nil)
  (rplacd (autocue:find-suspended-buffer-descr t)
	  (list (point) autocue:secs-per-step autocue:lines-per-step)))

(define-key autocue-keymap "+" 'autocue:faster)
(define-key autocue-keymap "-" 'autocue:slower)
(define-key autocue-keymap ">" 'autocue:bigger)
(define-key autocue-keymap "<" 'autocue:smaller)
(define-key autocue-keymap " " 'scroll-up)
(define-key autocue-keymap "" 'scroll-down)
(define-key autocue-keymap "z" 'autocue:put-aside)

(defvar autocue-one-line-help
  (substitute-command-keys
   "\\<autocue-keymap>Speed: \\[autocue-slower]\\[autocue-faster] step:\\[autocue-smaller]\\[autocue-bigger]")
  "Help string for autocue to flash up in minibuffer as it sees fit.")

;;;###autoload
(defun autocue (buffer &optional sec-per-step lines-per-step start-pattern countdown)
  "Display BUFFER and slowly scroll down it.
Optional extra args are
  SEC-PER-STEP
  LINES-PER-STEP
  START-PATTERN
  COUNTDOWN."
  (interactive "bAutocue buffer: ")
  (setq autocue:secs-per-step (if sec-per-step sec-per-step 1)
	autocue:lines-per-step (if lines-per-step lines-per-step 1)
	autocue:continue t)
  (run-hooks 'autocue-start-hooks)
  (save-window-excursion
    (switch-to-buffer buffer)
    (delete-other-windows)
    (goto-char (point-min))
    (let ((was (autocue:find-suspended-buffer-descr)))
      (cond
       ((and was (cdr was))
	(setq autocue:secs-per-step (second (cdr was))
	      autocue:lines-per-step (third (cdr was)))
	(goto-char (first (cdr was)))
	(rplacd was nil))
       ((stringp start-pattern)
	(re-search-forward start-pattern (point-max) t))
       ((numberp start-pattern)
	(if (> start-pattern 0)
	    (progn
	      (goto-line start-pattern))
	  (progn
	    (goto-char (point-max))
	    (forward-line start-pattern))))))
    (while (and autocue:continue (not (eobp)))
      (sit-for autocue:secs-per-step)
      (when (zerop (decf autocue:message-keep))
	(message nil))
      (when (input-pending-p)
	(let* ((command-key (read-event))
	       (command-key-vec (vector command-key))
	       (command-function (lookup-key autocue-keymap command-key-vec)))
	  (when command-function
	    (funcall command-function))
	  (sit-for autocue:secs-per-step) ; catch up on interrupted sleep
	  ))
      (when (and (integerp countdown)
		 (zerop (decf countdown)))
	(autocue:put-aside))
      (forward-line autocue:lines-per-step))
    (bury-buffer)))

;;; end of autocue.el
