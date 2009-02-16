;; dired-dd-insert-fname.el -- Another example of dired-dd-non-dired-drop-handler

;; Sat Dec 12 00:41:12 1998: 1st draft
;; Tue Dec 15 22:09:32 1998: a fix. (delq '(drag)... => (delq 'drag...

;; Freeware. GPL2 applied. (c) 1998 S.Namba <sn@asahi-net.email.ne.jp>

;; Insert filename(s) by C-drag-mouse-2 in any non-dired-buffer.
;; Insert fullpath name(s) by M-drag-mouse-2 in any non-dired-buffer.

;; Another sample of `dired-dd-non-dired-drop-handler'.  
;; Buffer mode is not checked,
;; It may be safer to register at the end of dired-dd-non-dired-drop-handlers.

;; To load (in your ~/.emacs):
;;  (add-hook
;;   'dired-load-hook
;;   (function
;;    (lambda ()
;;      (load "dired-x")
;;      ;; Set dired-x variables here.
;;      ;; To and flo...
;;      (if window-system
;;	  (progn (require 'dired-dd)
;;		 (require 'dired-dd-insert-fname))))))

(require 'dired-dd)

;;
;; A handler function dealing with inserting file.
;; 

;;
;; For those who ever want to write similar handler functions,
;;
;; As for the argument which must be supported by this handler, Here is
;; an excerpt from docstring of variable dired-dd-non-dired-drop-handlers:
;;
;; ARGLIST, whose format is:
;; 
;;   (FILENAME-LIST WINDOW MODIFIERS POINT)
;; 
;; wherein
;; 
;;   FILENAME-LIST: a list of filenames (in full path description)
;;   WINDOW:        target window (probably useless)
;;   MODIFIERS:     modifier key list at the relevant drag-and-drop operation
;;   POINT:         buffer position corresponding to point where file is dropped.
;; 
;; Not enough ?  Don't forget that we are lisp programmers.  You can dare to
;; use variables such as `modifiers', `event', `start-window' etc.,
;; which are defined in upper functions.  Refer to the upper functions
;; dired-dd-drag-drop, and  dired-dd-drop-from-to in dired-dd.el,
;; for those parameters.
;; 
;; The handler function can assume that current window/buffer is
;; already switched to target buffer when it is called.  It is
;; responsibility of the handler function to examine whether the
;; current window/buffer is suitable for its job.  The examination
;; should be strict as possible, because it is not predictable that
;; the user registers what handler function in what order.
;;
;; The handler function must return non-nil if its operation was successful,
;; otherwise nil.
;;

(defun dired-dd-insert-fname (arglist)
  "Insert filename by Drag-and-drop in any non-dired-buffer.
C-drag-mouse-2: insert plain filename(s)
M-drag-mouse-2: insert fullpath name(s)
Filenames are inserted as white-space separated list.
Returns non-nil if it succeeds, otherwise nil."
  ;; Current arglist passed by dired-dd: (fn-list to modifiers end-point)
  ;; You can also use variables such as `modifiers', `event', `start-window'
  ;; etc., which are defined in upper functions.  Refer to the upper functions
  ;; dired-dd-drag-drop and  dired-dd-drop-from-to in dired-dd.el,
  ;; for those parameters.
  (let ((files (car arglist))
	(modifiers (delq 'drag (car (cdr (reverse arglist)))))) ; use arglist
    (cond
     ((equal '(control) modifiers)
      (goto-char (car (reverse arglist))) ; works fine in older emacsen
      (setq files
	    (mapcar (lambda (file)
		      (file-name-nondirectory file))
		    files))
      (insert
       (substring  (format "%s" files) 1 -1))
      t)
     ((equal '(meta) modifiers)
      (goto-char (car (reverse arglist))) ; works fine in older emacsen
      ;; insert fullpath names as they are
      (insert
       (substring  (format "%s" files) 1 -1))
      t)
     (t nil) )))

;; After you defined the handler function,
;; register it in dired-dd-non-dired-drop-handlers in a list form
;; (Safer to register at the end of dired-dd-non-dired-drop-handlers ?):
(setq dired-dd-non-dired-drop-handlers
      (cons 'dired-dd-insert-fname dired-dd-non-dired-drop-handlers))

(provide 'dired-dd-insert-fname)
