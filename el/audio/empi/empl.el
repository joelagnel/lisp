;;; EMPL.EL --- EMPI playlist editor

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	23 May 2004
;; Version: 	1.0
;; Keywords:	music, empi

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <andyetitmoves@gmail.com>)
;; or from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;; MA 02139, USA.

;; LCD Archive Entry:
;; empl|R.Ramkumar|<andyetitmoves@gmail.com>
;; |EMPI playlist editor
;; |$Date$|$Revision$|~/packages/empl.el

;;; Code:

(require 'empi)
(require 'custom)
(require 'mouse)			; for mouse related functions

;;;; Use serviceable variables

(defgroup empl nil
  "Playlist editor for EMPI."
  :prefix "empl-"
  :group 'empi)

(defgroup empl-faces nil
  "Faces customization for EMPL."
  :group 'empl :group 'faces)

(defface empl-header-face
  '((((background dark)) (:foreground "cyan" :height 2.0 :weight bold))
    (((background light)) (:foreground "blue")))
  "Face to use for the header of the EMPI playlist buffer."
  :group 'empl-faces)

(defface empl-line-number-face
  '((((background dark)) (:foreground "lightblue" :background "grey30"))
    (((background light)) (:foreground "darkgreen" :background "lightyellow2")))
  "Face to use for the EMPI playlist item numbering."
  :group 'empl-faces)

(defface empl-title-face
  '((((background dark))
     (:foreground "lightyellow" :box (:line-width 1 :color "grey30")))
    (((background light))
     (:foreground "DarkGoldenrod" :box (:line-width 1 :color "lightyellow2"))))
  "Face to use for titles in the EMPI playlist."
  :group 'empl-faces)

(defface empl-mark-face
  '((((background dark))
     (:background "orange" :box (:line-width 1 :color "orange")))
    (((background light))
     (:background "cornsilk3" :box (:line-width 1 :color "cornsilk3"))))
  "Face to overlay for marked items in the EMPI playlist."
  :group 'empl-faces)

(defface empl-playing-face
  '((((background dark))
     (:foreground "yellow" :background "black"
      :box (:line-width 3 :color "cyan" :style released-button)))
    (((background light))
     (:foreground "blue" :background "lightyellow"
      :box (:line-width 3 :style released-button))))
  "Face to overlay for the currently playing item in EMPL."
  :group 'empl-faces)

(defface empl-drag-source-face '((t (:foreground "steelblue")))
  "Face to overlay for the source of a drag operation in EMPL."
  :group 'empl-faces)

(defface empl-drag-dest-face
  '((((background dark))
     (:background "grey25" :box (:line-width 2 :style pressed-button)))
    (((background light))
     (:background "grey90" :box (:line-width 2 :style pressed-button))))
  "Face to overlay for the destination of a drag operation in EMPL."
  :group 'empl-faces)

(defcustom empl-refocus-on-change-flag t
  "*Non-nil means cursor is bound to a playlist item in EMPL across updates.
That is, if the playlist changes or if the playlist is reread for some reason,
then the cursor adapts to stay on the playlist item it was originally in,
even if the item changes position. If not set, the cursor stays put in the
safe buffer position across updates."
  :type 'boolean :group 'empl)

(defcustom empl-selection-as-current-flag nil
  "*Non-nil makes an active selection act as a default range in EMPL.
Many of the commands acting on the playlist refer to the marked regions of the
playlist maintained by `empl-do-mark', `empl-do-unmark' and
``empl-do-toggle-mark'. If there is no marked region, then these commands act on
the selection in transient mode, if there is any and this flag is set.
Otherwise, the item at the cursor location is chosen, and an error flagged if
even that fails."
  :type 'boolean :group 'empl)

(defcustom empl-update-interval 1
  "*The time interval between EMPI playlist buffer updates.
The value can either be a number depicting the time interval in seconds or
nil to prevent automatic updation altogether. You will then have to use \
\\<empl-mode-map>\\[empl-force-update]
to manually update. Keep in mind the speed of your machine while coming up
with a good value. However, this is a relatively cheap operation,
see `empl-no-id-update-flag' in this regard."
  :type '(choice (natnum :tag "Timeout") (const :tag "Manual" nil))
  :group 'empl)

(defcustom empl-no-id-update-flag nil
  "*Non-nil means force a periodic refresh in EMPL.
Periodic playlist updation occurs by checking if a playlist version number has
changed after the last update. Only then is the entire playlist updated.
Otherwise, just the playlist position is checked. However, the process of
checking the playlist version may not always succeed, based on your EMPI setup.
In such cases, if this flag is non-nil, then a re-read of the playlist is
forced. Otherwise, the automatic updation limits itself to checking the playlist
position. Note that Emacs could become sluggish for a non-nil value in such
cases, especially if the playlist is long. Further, this may be unnecessary if
you don't expect the playlist to change outside EMPL, as editing done
within EMPL will anyway refresh the playlist properly."
  :tag "Allow time intensive periodic updates"
  :type 'boolean :group 'empl)

(defcustom empl-playlist-locked nil
  "Non-nil means that the EMPI playlist disallows editing.
Operations like marking or anchoring are still allowed, while those like
deletion or marking are disallowed. Automatically becomes buffer-local
when set in any fashion. This option sets the default for any playlist.
Use `empl-toggle-lock' for toggling the local value."
  :type 'boolean :group 'empl)

(defcustom empl-range-mode nil
  "Non-nil means that the EMPI playlist ignores marks for actions.
Marking then exists only for visual enhancement of the selected items.
Automatically becomes buffer-local when set in any fashion.
This option sets the default for any playlist.
Use `empl-toggle-range-mode' for toggling the local value."
  :type '(choice (const :tag "Selection" nil)
		 (const :tag "Mark Group" t)
		 (other :tag "Line")) :group 'empl)

(defcustom empl-go-up-on-mark nil
  "Non-nil means cursor moves up on marking in the EMPI playlist.
Cursor is moved only when a marking action occurs on the item at point
and not on an active selection in transient mode.
Automatically becomes buffer-local when set in any fashion.
This option sets the default for any playlist.
Use `empl-toggle-go-up-on-mark' for toggling the local value."
  :type 'boolean :group 'empl)

(defcustom empl-deselect-on-command nil
  "Non-nil means marked ranges are removed on any command in EMPL.
Command refers here to any command, like delete, crop etc., excluding the
marking and the anchoring actions themselves, which acts on a selection."
  :type 'boolean :group 'empl)

(defcustom empl-mode-hook nil
  "List of hooks to run on starting a EMPI playlist buffer."
  :type 'hook :group 'empl)

(defvar empl-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-keys map
      [?p] 		'empl-goto-playing
      [tab] 		'empl-goto-next-mgroup
      [?`]		'empl-goto-prev-mgroup
      [?n]		'empl-goto-item
      [?f]		'empl-filter-to-marks
      [? ] 		'empl-do-toggle-mark
      [?m] 		'empl-do-mark
      [?s] 		'empl-mark-matching
      [?S] 		'empl-mark-re-matching
      [?u] 		'empl-do-unmark
      [(shift ?u)] 	'empl-unmark-all
      [(meta ?u)] 	'empl-do-delete-mgroup
      [?b] 		'empl-toggle-go-up-on-mark
      [?l] 		'empl-toggle-lock
      [?i] 		'empl-toggle-range-mode
      [M-left] 		'empl-do-set-anchor
      [M-S-left] 	'empl-unset-anchor
      [return] 		'empl-play
      [down-mouse-2] 	'empl-mouse-move
      [mouse-2]		'empl-noop
      [double-mouse-2] 	'empl-mouse-play
      [delete] 		'empl-delete
      [?C] 		'empl-crop
      [M-up] 		'empl-move-selection-up
      [M-down] 		'empl-move-selection-down
      [M-right] 	'empl-do-move-here
      [?c] 		'empl-compact
      [?g] 		'empl-update
      [?G] 		'empl-force-update
      [?q]		'quit-window
      ) map)
  "Keymap for the EMPL playlist buffer.")

(defconst empl-plbegin-line 4
  "Line at which the first playlist item in EMPL is positioned.")

(defconst empl-item-number-regex "^ *\\([0-9]+\\)"
  "Regular expression matching item number on a line of the EMPL buffer.")

(defconst empl-playing-prio 10)
(defconst empl-mark-prio 20)
(defconst empl-drag-source-prio 30)
(defconst empl-drag-dest-prio 40)

(defvar empl-update-timer nil)
(defvar empl-playing-overlay nil)
(defvar empl-playing-position nil)
(defvar empl-plid nil)
(defvar empl-item-count 0)
(defvar empl-marked-items nil)
(defvar empl-anchor nil)

(mapc 'make-variable-buffer-local
      '(empl-playing-overlay empl-playing-position empl-plid empl-item-count
        empl-marked-items empl-anchor empl-update-timer empl-playlist-locked
	empl-range-mode empl-go-up-on-mark))

(defvar empl-inhibit-timed-update nil
  "Inhibit timed update on the EMPI playlist.
Not an user option, this is meant for temporary binding in lisp functions.")

(defvar empl-inhibit-clean nil
  "Inhibit forced update on a dirtying action on the EMPI playlist.
Not an user option, this is meant for temporary binding in lisp functions.")

(defvar empl-inhibit-mark-relocate nil
  "List of items not to relocate on EMPL update.
Not an user option, this is meant for temporary binding in lisp functions.")

(defun empl-make-overlay (face priority)
  (let ((overlay (make-overlay 1 1)))
    (delete-overlay overlay)
    (overlay-put overlay 'face face)
    (overlay-put overlay 'priority priority) overlay))

(defvar empl-drag-source-overlay
  (empl-make-overlay 'empl-drag-source-face empl-drag-source-prio))
(defvar empl-drag-dest-overlay
  (empl-make-overlay 'empl-drag-dest-face empl-drag-dest-prio))

;;;; Package independent helpers

(defsubst dup-cons (item &optional nonil)
  "Return (ITEM . ITEM) unless ITEM is nil and NONIL is non-nil."
  (if (or item (not nonil))
      (cons item item)))
(put 'dup-cons 'side-effect-free 'error-free)

(defun empl-goto-char (char)
  "Go to position CHAR in current buffer, widening if necessary."
  (and (or (< char (point-min)) (> char (point-max))) (widen))
  (goto-char char))

(defmacro assert-type (obj func)
  "Ensure OBJ tests non-nil against type checking function FUNC.
The function emits a \"Wrong type argument\" signal on failure."
  `(or (,func ,obj)
       (signal 'wrong-type-argument (list (quote ,func) ,obj))))
(def-edebug-spec assert-type (form functionp))

(defmacro assert-wholenump (obj) `(assert-type ,obj wholenump))
(def-edebug-spec assert-type (form))

(defmacro with-temp-widen (&rest args)
  "Evaluate ARGS while temporarily widening the current buffer."
  `(save-restriction
     (widen)
     ,@args))
(put 'with-temp-widen 'lisp-indent-function 0)
(def-edebug-spec with-free-buffer (body))

(defmacro with-free-buffer (&rest args)
  "Evaluate ARGS with temporary widening and saved excursion."
  `(save-excursion
     (with-temp-widen ,@args)))
(put 'with-free-buffer 'lisp-indent-function 0)
(def-edebug-spec with-free-buffer (body))

(defmacro def-free-vars (&rest args)
  "Define symbols ARGS to silence the compiler for free variable references."
  (let ((rest args) def)
    (while rest
      (setq def (car rest))
      (assert-type def symbolp)
      (setcar rest (list 'defvar def))
      (setq rest (cdr rest)))
    (cons 'progn args)))
(def-edebug-spec def-free-vars (&rest symbolp))

(defun empl-noop ()
  (interactive))

;;;; Point identification

(defun empl-item-search (&optional start bound backward)
  "Search for a playlist item in the EMPI playlist buffer.
The search is done starting at START, defaulting to the current position and
bound by BOUND defaulting to the end of the buffer in the forward direction
unless BACKWARD is non-nil."
  (with-free-buffer
    (if start (goto-char start))
    (if backward (end-of-line) (forward-line 0))
    (let (item)
      (while (and (not item)
		  (funcall (if backward 're-search-backward 're-search-forward)
			   empl-item-number-regex bound t))
	(forward-line 0)
	(or (= (setq item (- (count-lines 1 (point)) (1- empl-plbegin-line)))
	       (1- (string-to-number (match-string 1))))
	    (setq item nil))) item)))

(defun empl-verify-line-item (item &optional noerror)
  "Verify contents of line to contain playlist item number ITEM in EMPL.
The contents of the line containing position AT is examined, defaulting to the
current position. Unless NOERROR is non-nil, an error is thrown on verification
failure as against a return value of nil. ITEM is returned on success. Point is
not guaranteed to be restored by this function."
  (forward-line 0)
  (if (looking-at empl-item-number-regex)
      (if (= item (1- (string-to-number (match-string 1))))
	  item
	(and (not noerror)
	     (error "Mismatch between playlist index and line position %d"
		    (1+ item))))
    (and (not noerror) (error "No playlist entry at this line"))))

(defsubst empl-verify-current (&optional noerror)
  (empl-verify-line-item
   (- (count-lines 1 (point)) empl-plbegin-line (if (bolp) -1 0)) noerror))

(defun empl-item-at-point (&optional noerror at)
  "Return EMPI playlist item at AT, defaulting to the current position.
An error is signalled if the playlist buffer appears to be invalid unless
NOERROR is non-nil, when nil is returned."
  (with-free-buffer
    (and at (goto-char at))
    (empl-verify-current noerror)))

(defun empl-get-line-info ()
  (and (looking-at "\\(^\\( *\\)\\([0-9]+\\) *\\)\\(.*\\)")
       (vector (length (match-string 1))
	       (1- (string-to-number (match-string 3)))
	       (length (match-string 1)) (match-string 4))))

;;;; Current items

(defsubst empl-can-use-selection ()
  (and transient-mark-mode mark-active empl-selection-as-current-flag))
(put 'empl-can-use-selection 'side-effect-free t)

(defun empl-current-range (&optional noerror)
  (if (empl-can-use-selection)
      (let ((sitem (empl-item-search (region-beginning) (1- (region-end))))
	    (eitem (empl-item-search (1- (region-end)) (region-beginning) t)))
	(cond
	 ((and eitem (not sitem)) (cons eitem eitem))
	 ((and sitem (not eitem)) (cons sitem sitem))
	 ((and sitem eitem (<= sitem eitem)) (cons sitem eitem))
	 (t (error "Unexpected error retrieving selected items"))))
    (dup-cons (empl-item-at-point noerror) noerror)))

;;;; Item marking - 'Set' functions

(defun empl-new-mark-overlay (start end)
  (let ((newo (make-overlay start end)))
    (overlay-put newo 'face 'empl-mark-face)
    (overlay-put newo 'priority empl-mark-prio) newo))

(defun empl-new-mgroup (item after)
  (setq item (vector (empl-new-mark-overlay
		      (point) (line-beginning-position 2)) item item))
  (if after
      (setcdr after (cons item (cdr after)))
    (setq empl-marked-items (cons item empl-marked-items))))

(defun empl-delete-mgroup (after this)
  (delete-overlay (aref (car this) 0))
  (if after
      (setcdr after (cdr this))
    (setq empl-marked-items (cdr empl-marked-items))))

(defun empl-merge-mgroups (pos)
  (move-overlay (aref (car pos) 0) (overlay-start (aref (car pos) 0))
		(overlay-end (aref (cadr pos) 0)))
  (aset (car pos) 2 (aref (cadr pos) 2))
  (empl-delete-mgroup pos (cdr pos)))

(defun empl-toggle-mark-item (add &optional arg)
  (forward-line 0)
  (let ((tail empl-marked-items) last flag)
    (while (and tail (not flag))
      (setq flag (car tail))
      (cond
       ((< add (1- (aref flag 1)))
	(if (or (not arg) (> arg 0))
	    (empl-new-mgroup add last)))
       ((= add (1- (aref flag 1)))
	(and (or (not arg) (> arg 0))
	     (if (and last (= (aref (car last) 2) (1- add)))
		 (empl-merge-mgroups last)
	       (move-overlay (aref flag 0) (point) (overlay-end (aref flag 0)))
	       (aset flag 1 (1- (aref flag 1))))))
       ((and (>= add (aref flag 1)) (<= add (aref flag 2)))
	(unless (and arg (> arg 0))
	  (or (= (aref flag 2) add)
	      (setcdr tail (cons (vector (empl-new-mark-overlay
					  (line-beginning-position 2)
					  (overlay-end (aref flag 0)))
				  (1+ add) (aref flag 2)) (cdr tail))))
	  (if (= (aref flag 1) add)
	      (empl-delete-mgroup last tail)
	    (move-overlay (aref flag 0) (overlay-start (aref flag 0)) (point))
	    (aset flag 2 (1- add)))))
       ((= add (1+ (aref flag 2)))
	(when (or (not arg) (> arg 0))
	  (setq last (cadr tail))
	  (if (and last (= (aref last 1) (1+ add)))
	      (empl-merge-mgroups tail)
	    (move-overlay (aref flag 0) (overlay-start (aref flag 0))
			  (line-beginning-position 2))
	    (aset flag 2 (1+ (aref flag 2))))))
       (t (setq flag nil)))
      (or flag (setq last tail tail (cdr tail))))
    (if (and (not tail) (or (not arg) (> arg 0)))
	(empl-new-mgroup add last) tail)))

(defun empl-delete-enclosing-mgroup (line)
  (catch 'empl-delete-enclosing-mgroup
    (let ((items empl-marked-items) last)
      (while items
	(when (and (>= line (aref (car items) 1))
		   (<= line (aref (car items) 2)))
	  (empl-delete-mgroup last items)
	  (throw 'empl-delete-enclosing-mgroup last))
	(setq last items items (cdr items))))))

(defun empl-do-delete-mgroup ()
  "Delete the mark group enclosing the item at point in EMPL."
  (interactive)
  (or (empl-delete-enclosing-mgroup (empl-item-at-point))
      (error "No group found at point")))

(defun empl-unmark-all ()
  "Remove all marks from the EMPL playlist."
  (interactive)
  (mapc '(lambda (item) (delete-overlay (aref item 0))) empl-marked-items)
  (setq empl-marked-items nil))

(defun empl-toggle-go-up-on-mark (&optional arg)
  "Toggle going up of point on a marking operation in EMPL.
See also `empl-go-up-on-mark'."
  (interactive "P")
  (setq empl-go-up-on-mark
	(if arg
	    (if (> (prefix-numeric-value arg) 0) t)
	  (if empl-go-up-on-mark nil t)))
  (force-mode-line-update))

(defun empl-do-toggle-mark (&optional arg nomove)
  "Toggle marking on the current range in the EMPI playlist.
If ARG is non-nil, mark current range iff ARG is positive.
Unless NOMOVE is non-nil, point moves according to `empl-go-up-on-mark' if the
toggling occurs on the item at point."
  (interactive)
  ;; Note that selection should be always included for marking
  ;; irrespective of the user option, hence the local binding below
  (let* ((empl-selection-as-current-flag t)
	 (item (empl-current-range)) (i (car item)))
    (with-free-buffer
      (goto-line (+ i empl-plbegin-line))
      (while (<= i (cdr item))
	(empl-toggle-mark-item i arg)
	(forward-line)
	(setq i (1+ i))))
    (or (empl-can-use-selection) nomove
	(line-move (if empl-go-up-on-mark -1 1)))))

(defun empl-do-mark ()
  "Mark the current range in the EMPI playlist."
  (interactive)
  (empl-do-toggle-mark 1))

(defun empl-do-unmark ()
  "Unmark the current range in the EMPI playlist."
  (interactive)
  (empl-do-toggle-mark -1))

(defun empl-mark-matching (str &optional regexp)
  "Mark items in EMPI playlist matching search string STR.
If REGEXP is non-nil, STR is treated as a regular expression."
  (interactive (list (read-string "Enter search string: "
				  (and (save-excursion
					 (forward-line 0)
					 (looking-at empl-item-number-regex))
				       (current-word)))))
  (with-free-buffer
    (goto-line empl-plbegin-line)
    (let (curpt item)
      (while (setq curpt (funcall (if regexp 're-search-forward
				    'search-forward) str nil t))
	(and (setq item (empl-item-at-point t))
	     (empl-toggle-mark-item item 1))
	(goto-char curpt)))))

(defun empl-mark-re-matching (regexp)
  "Mark items in EMPI playlist matching regular expresion REGEXP."
  (interactive "sEnter search regexp: ")
  (empl-mark-matching regexp t))

;;;; Item marking - 'Get' functions

(defun empl-count-marks ()
  (let ((count 0) (items empl-marked-items))
    (while items
      (setq count (+ count (- (aref (car items) 2) (aref (car items) 1)) 1)
	    items (cdr items))) count))

(defun empl-mgroup-at-item (item)
  (catch 'empl-group-found
    (let ((items empl-marked-items))
      (while items
	(when (and (>= item (aref (car items) 1))
		   (<= item (aref (car items) 2)))
	  (throw 'empl-group-found
		 (cons (aref (car items) 1) (aref (car items) 2))))
	(setq items (cdr items))))))

(defun empl-render-header (header)
  (insert "\n")
  (indent-line-to (/ (- (frame-width) (length header)) 2))
  (insert (propertize header 'face 'empl-header-face))
  (insert "\n\n"))

(defun empl-filter-to-marks ()
  "Display a buffer with just the marked items in EMPL."
  (interactive)
  (or empl-marked-items (error "No marked items found"))
  (let ((marks empl-marked-items) (buf (current-buffer)))
    (with-current-buffer (get-buffer-create "*empl-selection*")
      (toggle-read-only -1)
      (erase-buffer)
      (empl-render-header "EMPL Selection")
      (mapc '(lambda (group)
	       (insert-buffer-substring buf (overlay-start (aref group 0))
					(overlay-end (aref group 0)))) marks)
      (set-buffer-modified-p nil)
      (toggle-read-only 1)
      (goto-char 1)
      (setq buf (current-buffer)))
    (display-buffer buf)))

(defun empl-toggle-range-mode (&optional arg)
  "Toggle the range mode of the EMPI playlist buffer.
If ARG is non-nil, positive values result in full-selection mode, 0 turns on
group mode and negative values, line mode.
See `empl-range-mode' for a descption of these modes."
  (interactive "P")
  (setq empl-range-mode
	(if arg
	    (if (> (prefix-numeric-value arg) 0) nil
	      (if (= (prefix-numeric-value arg) 0) t 1))
	  (if empl-range-mode
	      (if (eq empl-range-mode t) 1 nil) t)))
  (force-mode-line-update))

(defun empl-marked-ranges (&optional item)
  (cond
   ((not empl-range-mode)
    (mapcar '(lambda (item) (cons (aref item 1) (aref item 2)))
	    empl-marked-items))
   ((eq empl-range-mode t)
    (let ((range (empl-mgroup-at-item (or item (empl-item-at-point t)))))
      (and range (list range))))))

;;;; Selection

(defun empl-selected-ranges (&optional noerror item)
  (or (empl-marked-ranges item)
      (let ((range (empl-current-range noerror)))
	(and range (list range)))))

(defun empl-act-over-selection (func action &rest args)
  (mapc '(lambda (item)
	   (let ((i (cdr item)))
	     (while (>= i (car item))
	       (apply func action i args)
	       (setq i (1- i))))) (nreverse (empl-selected-ranges))))

(defun empl-map-over-selection (func &optional noerror)
  (mapc '(lambda (item)
	   (let ((i (cdr item)))
	     (while (>= i (car item))
	       (funcall func i)
	       (setq i (1- i))))) (nreverse (empl-selected-ranges noerror))))

(defun empl-map-invert-selection (func)
  (let ((i (1- empl-item-count)) (rest (nreverse (empl-marked-ranges))))
    (or rest (error "No marked ranges present"))
    (while (>= i 0)
      (and (or (not rest) (> i (cdar rest)))
	   (funcall func i))
      (and rest (= i (caar rest)) (setq rest (cdr rest)))
      (setq i (1- i)))))

;;;; Navigation

(defun empl-goto-playing ()
  "Goto currently playing playlist item in the EMPI playlist buffer."
  (interactive)
  (if (overlay-buffer empl-playing-overlay)
      (progn
	(empl-goto-char (overlay-start empl-playing-overlay))
	;; Emulate C-u prefix arg
	(recenter '(4)))
    (error "No item is currently playing")))
(put 'empl-goto-playing 'empl-mouse-move-safe t)

(defun empl-goto-item (item &optional offset noverify)
  "Goto playlist item ITEM in the EMPI playlist buffer.
OFFSET, if specified, denotes the column number to position point finally.
Unless NOVERIFY is non-nil, the reached point undergoes a sanity check."
  (interactive (list (1- (prefix-numeric-value current-prefix-arg))))
  (or (and (>= item 0) (< item empl-item-count))
      (error "Invalid item number to move to"))
  (with-temp-widen
    (or offset (setq offset (- (point) (point-at-bol))))
    (goto-line (+ item empl-plbegin-line))
    (setq offset (+ (point) (min offset (- (point-at-eol) (point)))))
    (or noverify (empl-verify-line-item item)))
  (empl-goto-char offset))
(put 'empl-goto-item 'empl-mouse-move-safe t)

(defun empl-mgroup-limit (group &optional end)
  (if end
      (progn
	(goto-char (overlay-end (aref group 0)))
	(line-beginning-position 0))
    (overlay-start (aref group 0))))

(defun empl-goto-next-mgroup (&optional arg)
  "Goto the immediate mark group after point in the EMPI playlist.
If prefix ARG is non-nil, point is moved to the last item in the group,
else the first item is chosen."
  (interactive "P")
  (let ((items empl-marked-items) ref (point (point)))
    (with-temp-widen
      (while (and items (<= (setq ref (empl-mgroup-limit
				       (car items) arg)) point))
	(setq items (cdr items))))
    (if items
	(empl-goto-char ref)
      (goto-char point)
      (error "No more mark groups found"))))
(put 'empl-goto-next-mgroup 'empl-mouse-move-safe t)

(defun empl-goto-prev-mgroup (&optional arg)
  "Goto the immediate mark group before point in the EMPI playlist.
If prefix ARG is non-nil, point is moved to the last item in the group,
else the first item is chosen."
  (interactive "P")
  (let ((items empl-marked-items) ref last (point (point)))
    (with-temp-widen
      (while (and items (< (setq ref (empl-mgroup-limit
				      (car items) arg)) point))
	(setq last ref items (cdr items))))
    (if last
	(empl-goto-char last)
      (goto-char point)
      (error "No previous mark group found"))))
(put 'empl-goto-prev-mgroup 'empl-mouse-move-safe t)

;;;; Anchor

(defun empl-do-set-anchor ()
  "Set anchor to the item at point in the EMPI playlist buffer.
The anchor is a special mark present at atmost one item in the playlist buffer,
and is used by operations like `empl-do-move-here' and `empl-swap'."
  (interactive)
  (setq empl-anchor (empl-item-at-point) overlay-arrow-string ">")
  (or (markerp overlay-arrow-position)
      (setq overlay-arrow-position (make-marker)))
  (with-free-buffer
    (forward-line 0)
    (set-marker overlay-arrow-position (point))))

(defun empl-unset-anchor ()
  "Unset the anchor in the EMPI playlist buffer.
See also `empl-do-set-anchor'."
  (interactive)
  (setq empl-anchor nil)
  (and (markerp overlay-arrow-position)
       (set-marker overlay-arrow-position nil)))

;;;; Playlist locking

(defun empl-toggle-lock (&optional arg)
  "Toggle locking of the EMPI playlist buffer.
If ARG is non-nil, lock buffer iff ARG is positive.
See `empl-playlist-locked' for a description of locking."
  (interactive "P")
  (setq empl-playlist-locked
	(if arg
	    (if (> (prefix-numeric-value arg) 0) t)
	  (if empl-playlist-locked nil t)))
  (force-mode-line-update))

(defsubst empl-assert-unlocked ()
  (and empl-playlist-locked
       (error "Playlist is locked, use %s to unlock"
	      (substitute-command-keys
	       "\\<empl-mode-map>\\[empl-toggle-lock]"))))

;;;; Display

(defun empl-update-playing ()
  (with-empi-cache
    (let ((plpos (empi-query :qplpos)))
      (if (and (wholenump plpos) (< plpos empl-item-count))
	  (or (equal plpos empl-playing-position)
	      (with-free-buffer
		(goto-line (+ plpos empl-plbegin-line))
		(move-overlay empl-playing-overlay (line-beginning-position)
			      (line-beginning-position 2))))
	(delete-overlay empl-playing-overlay))
      (setq empl-playing-position plpos))))

(defun forward-one-line-insert ()
  (forward-line)
  (or (bolp) (insert "\n")))

(def-free-vars empl-marks empl-curmark empl-i empl-len empl-new-anchor
  empl-linfo empl-mark-flag empl-point empl-reloc-point empl-old empl-buffer)

(defun empl-refresh-buffer-hook (item)
  (with-current-buffer empl-buffer
    (or (stringp item) (emperet "qpltitles"))
    (setq empl-linfo (empl-get-line-info) empl-mark-flag nil)
    (delete-region (point)
		   (if empl-linfo (+ (point) (aref empl-linfo 2) -1)
		     (point-at-eol)))
    (insert (propertize (format empl-len (1+ empl-i))
			'face 'empl-line-number-face 'intangible t))
    (if (and empl-linfo (= (aref empl-linfo 1) empl-i)
	     (string= item (aref empl-linfo 3)))
	(progn
	  (forward-one-line-insert)
	  ;; TODO: replace empl-mgroup-at-item with an in-loop FSA.
	  (and (empl-mgroup-at-item empl-i) (setq empl-mark-flag t))
	  (and empl-anchor (= empl-i empl-anchor)
	       (setq empl-new-anchor empl-i)))
      (let (origin)
	(or (memq empl-i empl-inhibit-mark-relocate)
	    (let (fwd bwd)
	      (with-current-buffer empl-old
		(setq origin (point))
		(and (search-forward item nil t)
		     (setq fwd (empl-item-at-point t)))
		(goto-char origin)
		(and (search-backward item nil t)
		     (setq bwd (empl-item-at-point t)))
		(goto-char origin)
		;; Don't insert at end
		(forward-line))
	      (and bwd (or (not fwd) (> (- empl-i fwd) (- bwd empl-i)))
		   (setq fwd bwd))
	      (when fwd
		(and empl-anchor (= fwd empl-anchor)
		     (setq empl-new-anchor empl-i))
		(and (empl-mgroup-at-item fwd) (setq empl-mark-flag t))
		(and empl-point (= empl-point fwd)
		     (setq empl-reloc-point empl-i)))))
	(setq origin (point))
	(forward-one-line-insert)
	(delete-region origin (point))
	(insert (propertize (concat
			     (propertize " " 'display
					 '((space . (:height 2 :ascent 67))))
			     item "\n") 'face 'empl-title-face))))
    (if empl-curmark
	(when (not empl-mark-flag)
	  (aset empl-curmark 2 (1- empl-i))
	  (aset empl-curmark 0
		(empl-new-mark-overlay (aref empl-curmark 0)
				       (line-beginning-position 0)))
	  (setq empl-marks (cons empl-curmark empl-marks) empl-curmark nil))
      (and empl-mark-flag
	   (setq empl-curmark (vector (line-beginning-position 0) empl-i nil))))
    (setq empl-i (1+ empl-i))))

(defun empl-refresh-buffer ()
  (with-critical-lock (concat "empl-refresh-buffer-lock-" (buffer-name))
    (with-empi-cache			;Unwind from here
      (toggle-read-only -1)
      (unwind-protect
	  (let ((case-fold-search nil) (empl-len (empi-query :qpllength))
		(empl-old (generate-new-buffer "*empl-old*")) (empl-i 0)
		empl-linfo empl-mark-flag empl-new-anchor empl-marks
		empl-curmark (empl-buffer (current-buffer))
		(empl-point (and empl-refocus-on-change-flag
				 (empl-item-at-point t)))
		empl-reloc-point (empl-offset (- (point) (point-at-bol))))
	    (with-free-buffer
	      (with-current-buffer empl-old
		(erase-buffer)
		(insert-buffer-substring empl-buffer)
		(goto-line empl-plbegin-line))
	      (goto-char 1)
	      ;; Turn off 'now playing' overlay, insertion could affect it.
	      (delete-overlay empl-playing-overlay)
	      (setq empl-playing-position nil)
	      (delete-region (point)
			     (line-beginning-position empl-plbegin-line))
	      (empl-render-header "EMPI Playlist")
	      (setq empl-len
		    (concat "% " (number-to-string
				  (if (wholenump empl-len)
				      (1+ (ceiling (log (1+ empl-len) 10)))
				    5)) "d "))
	      (unwind-protect
		  (empi-simple-action :qpltitles
				      '(lambda (item)
					 (let ((inhibit-quit t))
					   (empl-refresh-buffer-hook item))))
		(setq empl-item-count empl-i)
		(delete-region (point) (point-max))
		(when empl-curmark
		  (aset empl-curmark 2 (1- empl-i))
		  (aset empl-curmark 0
			(empl-new-mark-overlay (aref empl-curmark 0) (point)))
		  (setq empl-marks (cons empl-curmark empl-marks)))
		(empl-unmark-all)
		(setq empl-marked-items (nreverse empl-marks))
		(if empl-new-anchor
		    (progn
		      (goto-line (+ empl-new-anchor empl-plbegin-line))
		      (empl-do-set-anchor))
		  (empl-unset-anchor))
		(kill-buffer empl-old)))
	    (and empl-reloc-point
		 (empl-goto-item empl-reloc-point empl-offset t))
	    (setq empl-plid (empi-query :qplid))
	    (empl-update-playing))
	(toggle-read-only 1)
	;; Nothing happened all this while, right ???
	(set-buffer-modified-p nil)))))

(defun empl-update ()
  "Update the EMPI playlist buffer, refreshing if necessary.
See `empl-no-id-update-flag' for the semantics of this update.
This occurs automatically, if `empl-update-interval' specifies a valid time
interval between two updates."
  (interactive)
  (with-empi-cache
    (let ((plid (empi-query :qplid)) plpos)
      (if (or (and plid (not (equal plid empl-plid))) empl-no-id-update-flag)
	  (empl-refresh-buffer)
	(empl-update-playing)))))

(defun empl-clean ()
  (or empl-inhibit-clean (empl-refresh-buffer)))

(defun empl-force-update ()
  "Force refresh the EMPI playlist buffer.
This could be slow for long playlists, so use it only when the display seems
suspect. See `empl-no-id-update-flag' for when this occurs automatically."
  (interactive)
  (message "Force refreshing playlist...")
  (empl-refresh-buffer)
  (message "Force refreshing playlist...done"))

(defun empl-cancel-updates ()
  (when (empi-update-ref-p empl-update-timer)
    (empi-update-unregister empl-update-timer)
    (setq empl-update-timer nil)))

(defun empl-mode ()
  "Major mode for displaying playlists for EMPI.
Use `empl' to display a playlist, this is not to be used directly.
\\{empl-mode-map}."
  (kill-all-local-variables)
  (setq major-mode 'empl-mode mode-name "Empl")
  (set (make-local-variable 'minor-mode-alist)
       (append '((empl-playing-position
		  (:eval (format " %d/%d" (1+ empl-playing-position)
				 empl-item-count)))
		 (empl-marked-items
		  (:eval (format " Marks:%d" (empl-count-marks))))
		 (empl-go-up-on-mark " Mark-Up" " Mark-Down")
		 (empl-playlist-locked " Lock")
		 (empl-range-mode
		  (:eval (if (eq empl-range-mode t) " Group" " Line"))))
	       minor-mode-alist))
  (put 'empl-mode 'mode-class 'special)
  (setq truncate-lines t buffer-undo-list t)
  (if (overlayp empl-playing-overlay)
      (delete-overlay empl-playing-overlay)
    (setq empl-playing-overlay
	  (empl-make-overlay 'empl-playing-face empl-playing-prio)))
  (toggle-read-only -1)
  (erase-buffer)
  (condition-case nil
      (empl-refresh-buffer)
    (error nil))
  (and (overlay-buffer empl-playing-overlay)
       (goto-char (overlay-start empl-playing-overlay)))
  (when (and (wholenump empl-update-interval)
	     (> empl-update-interval 0) (not empl-update-timer))
    (setq empl-update-timer
	  (empi-update-register
	   `(lambda ()
	      (with-current-buffer ,(current-buffer)
		(or empl-inhibit-timed-update (empl-update))))
	   empl-update-interval nil))
    (add-hook 'kill-buffer-hook 'empl-cancel-updates nil t)
    (add-hook 'change-major-mode-hook 'empl-cancel-updates nil t))
  (use-local-map empl-mode-map)
  (run-hooks 'empl-mode-hook))

;;;###autoload
(defun empl ()
  (interactive)
  (or (prog1
	  (get-buffer "*empl-buffer*")
	(switch-to-buffer "*empl-buffer*"))
      (empl-mode)))

;;;; Moving items

(defun empl-move-range-up (low high by)
  (empl-assert-unlocked)
  (and (< low by) (error "Invalid value to move up by"))
  (let ((i low))
    (while (<= i high)
      (empi-simple-action :plmove i (- i by))
      (setq i (1+ i)))))

(defun empl-move-range-down (low high by)
  (empl-assert-unlocked)
  (with-empi-cache-let (pllen)
    (setq pllen (empi-query :qpllength))
    (and (wholenump pllen) (>= (+ high by) pllen)
	 (error "Invalid value to move down by")))
  (let ((i high))
    (while (>= i low)
      (empi-simple-action :plmove i (+ i by))
      (setq i (1- i)))))

(defun empl-move-selection-up (&optional by)
  "Move selection in the EMPI playlist buffer up by BY items."
  (interactive "p")
  (if by (and (< by 0) (empl-move-selection-down (- by))) (setq by 1))
  (mapc '(lambda (item)
	   (let* ((start (car item)) (width (- (cdr item) start -1)))
	     (and (< start by) (error "Invalid value to move up by"))
	     (if (< by width)
		 (empl-move-range-down (- start by) (1- start) width)
	       (empl-move-range-up start (cdr item) by))))
	(empl-selected-ranges))
  (empl-clean))

(defun empl-move-selection-down (&optional by)
  "Move selection in the EMPI playlist buffer down by BY items."
  (interactive "p")
  (if by (and (< by 0) (empl-move-selection-up (- by))) (setq by 1))
  (mapc '(lambda (item)
	   (let* ((end (cdr item)) (width (- end (car item) -1)))
	     (if (< by width)
		 (empl-move-range-up (1+ end) (+ end by) width)
	       (empl-move-range-down (car item) end by))))
	(nreverse (empl-selected-ranges)))
  (empl-clean))

(defun empl-move (anchor target &optional anchorpos offset)
  "Move selection or item at ANCHOR from ANCHOR to TARGET in EMPL.
Move selection instead of item at point if item at ANCHOR itself is marked.
If non-nil, ANCHORPOS is assumed to be a point on the anchor item. If OFFSET is
non-nil, position point on the line containing the anchor item; on the OFFSET
column if OFFSET is a number, else decide the column by ANCHORPOS if specified,
defaulting to beginning of the line."
  (empl-assert-unlocked)
  (let ((sentinel (point)))
    (unwind-protect
	(with-temp-widen
	  (progn
	    ;; Point is actually supposed to be here for selection purposes.
	    (if anchorpos
		(goto-char anchorpos)
	      (goto-line (+ empl-plbegin-line anchor)))
	    (when (numberp offset)
	      (and anchorpos (goto-char (point-at-bol)))
	      (forward-char (min offset (- (point-at-eol) (point)))))
	    (if (empl-mgroup-at-item anchor)
		(empl-move-selection-up (- anchor target))
	      (empi-simple-action :plmove anchor target)
	      (empl-clean))
	    ;; We haven't encountered any error, so if we really want to modify
	    ;; point, set sentinel so that the point doesn't revert on the
	    ;; unwind-protect form which follows.
	    (and offset (setq sentinel (point)))))
      (empl-goto-char sentinel))))

(defun empl-do-move-here ()
  "Move selection from the current EMPL anchor to item at point."
  (interactive)
  (empl-move
   (or empl-anchor (error "No anchor to move from")) (empl-item-at-point)
   overlay-arrow-position (- (point) (point-at-bol)))
  (empl-unset-anchor))

;;;; Actions

(defun empl-play ()
  "Play item at point in the EMPI playlist buffer."
  (interactive)
  (empi-simple-action :jumpitemnum (empl-item-at-point))
  (empl-update-playing))

(defun empl-delete ()
  "Delete item at point in the EMPI playlist buffer."
  (interactive)
  (empl-assert-unlocked)
  (let (empl-inhibit-mark-relocate)
    (empl-map-over-selection
     '(lambda (item)
	(and (empi-recov-action :pldel item)
	     (setq empl-inhibit-mark-relocate
		   (cons item empl-inhibit-mark-relocate)))))
    (empl-clean)))

(defun empl-crop ()
  "Crop playlist to selected items in the EMPI playlist buffer.
This command fails to act if there are no marked ranges (i.e. it doesn't default
to the current range in such a case, as is the case usually). This behaviour is
by design, to prevent accidental cropping."
  (interactive)
  (empl-assert-unlocked)
  (empl-map-invert-selection '(lambda (item) (empi-recov-action :pldel item)))
  (empl-clean))
(put 'empl-crop 'disabled t)

(defun empl-swap ()
  "Swap item at anchor and point in the EMPI playlist buffer."
  (interactive)
  (empl-assert-unlocked)
  (or empl-anchor (error "No anchor marked for swap"))
  (empi-simple-action :plswap empl-anchor (empl-item-at-point))
  (empl-clean))

(defun empl-compact ()
  (interactive)
  (empl-assert-unlocked)
  (when empl-marked-items
    (let* ((ranges (empl-marked-ranges)) (ahead ranges) behind
	   (point (empl-item-at-point)) back front)
      (while (and ahead (< (cdar ahead) point))
	(setq behind (cons (car ahead) behind)
	      ahead (cdr ahead)))
      (if (and ahead (> point (setq back (1- (caar ahead)))))
	  (setq front (1+ (cdar ahead)) ahead (cdr ahead))
	(setq back point front (if behind (1+ point) point)))
      (let ((empl-inhibit-clean t) item)
	(when behind
	  (setq item (car behind) back (- back (cdr item)))
	  (while
	      (progn
		(empl-move-range-down (car item) (cdr item) back)
		(and (setq behind (cdr behind))
		     (setq back
			   (+ back (- (car item) 1
				      (cdr (setq item (car behind))))))))))
	(when ahead
	  (setq item (car ahead) front (- (car item) front))
	  (while
	      (progn
		(empl-move-range-up (car item) (cdr item) front)
		(and (setq ahead (cdr ahead))
		     (setq front (+ front (- (1+ (cdr item)))
				    (car (setq item (car ahead))))))))))
      (empl-clean)
      (empl-goto-item point nil t))))

(defun empl-mouse-set-point (func)
  (let ((point (point)) (w (selected-window)))
    (and (condition-case err
	     (progn
	       (command-execute func) t)
	   (error (progn
		    (message "Error executing function %s: %s"
			     func (error-message-string err)) nil)))
;;;	 (message "%s -> %s" point (point))
	 (unless (= point (point))
;;; Unable to set the mouse position correctly :(
;;; Isn't there a way on earth to reliably find frame coords from point ???
;;; 	   (setq point (compute-motion (max (window-start w) (point-min))
;;; 				       '(0 . 0) (point)
;;; 				       (cons (window-width) (window-height))
;;; 				       (1- (window-width))
;;; 				       (cons (window-hscroll w) 0) w))
;;; 	   (set-mouse-position (selected-frame)
;;; 			       (cadr point) (* 2 (nth 2 point)))
	   t))))

(def-free-vars start-item end-item)
(defun empl-mouse-set-dest ()
  (save-excursion
    (let ((item (empl-verify-current t)))
      (if (and item (not (equal item end-item)))
	  (progn
	    (if (= item start-item)
		(delete-overlay empl-drag-dest-overlay)
	      (move-overlay empl-drag-dest-overlay
			    (point) (line-beginning-position 2)))
	    (setq end-item item))))))

(defun empl-mouse-move (start-event)
  "Move dragged item in EMPL to destination or play it if clicked."
  (interactive "e")
  (let* ((echo-keystrokes 0)
	 (cursor-type nil)
	 (start-posn (event-start start-event))
	 (start-point (posn-point start-posn)) start-item
	 (start-window (posn-window start-posn))
	 (bounds (window-edges start-window))
	 (top (nth 1 bounds))
	 (bottom (1- (nth 3 bounds))))
    (select-window start-window)
    (goto-char start-point)
    (if (< (point) start-point)
	(goto-char start-point))
    (setq start-point (point))
    (save-excursion
      (setq start-item (empl-verify-current))
      (overlay-put empl-drag-dest-overlay 'before-string
		   (concat (buffer-substring-no-properties
			    (point) (+ (point) (aref (empl-get-line-info) 2)))
			   "->")))
    (let (event end end-point end-item func)
      (track-mouse
	(while (or (mouse-movement-p (setq event (read-event)))
		   (and (setq func (key-binding (vector event)))
			(symbolp func) (get func 'empl-mouse-move-safe)
			(setq event (if (empl-mouse-set-point func)
					(list 'mouse-movement
					      (list start-window (point)))
				      '(ignore))))
		   (eq (car-safe event) 'switch-frame))
	  (unless (memq (car-safe event) '(switch-frame ignore))
	    (or end (save-excursion
		      (goto-char start-point)
		      (move-overlay empl-drag-source-overlay
				    (line-beginning-position)
				    (line-beginning-position 2))))
	    (setq end (event-end event) end-point (posn-point end))
	    (if (and (eq (posn-window end) start-window)
		     (integer-or-marker-p end-point))
		(progn
		  (goto-char start-point)
		  (goto-char end-point))
	      (let ((mouse-row (cdr (cdr (mouse-position)))))
		(when (and mouse-row (or (< mouse-row top)
					 (>= mouse-row bottom)))
		  (mouse-scroll-subr start-window
				     (if (< mouse-row top)
					 (- mouse-row top)
				       (1+ (- mouse-row bottom)))))))
	    (empl-mouse-set-dest))))
      (when (and (memq 'drag (event-modifiers (car-safe event)))
		 (setq end (event-end event) end-point (posn-point end))
		 (eq (posn-window end) start-window)
		 (integer-or-marker-p end-point))
	(goto-char start-point)
	(goto-char end-point)
	(empl-mouse-set-dest))
      (unwind-protect
	  (if (consp event)
	      (if end
		  (and end-item (not (= end-item start-item))
		       (empl-move start-item end-item start-point t))
		;; We don't need to do anything, pass the command on.
		(setq unread-command-events
		      (cons event unread-command-events))))
	(delete-overlay empl-drag-source-overlay)
	(delete-overlay empl-drag-dest-overlay)))))

(defun empl-mouse-play (start-event)
  (interactive "e")
  (let* ((start-posn (event-start start-event))
	 (start-point (posn-point start-posn)) start-item
	 (start-window (posn-window start-posn)))
    (select-window start-window)
    (goto-char start-point)
    (if (< (point) start-point)
	(goto-char start-point))
    (setq start-point (point))
    (save-excursion
      (setq start-item (empl-verify-current)))
    (unwind-protect
	(empi-simple-action :jumpitemnum start-item)
      (empl-update-playing))))

(provide 'empl)

;;; EMPL.EL ends here
