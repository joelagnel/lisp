;; dired-dd-timidity.el -- Really experimental.

;; Fri Jan 15 03:57:18 1999: 1st draft

;; Freeware. GPL2 applied. (c) 1998,1999 S.Namba <sn@asahi-net.email.ne.jp>

;; Drops midi file(s) into timidity-mode buffer.
;; A function for play-one-midi-file-only interface is inluded, but
;; not registered in dired-dd-non-dired-drop-handlers.

;; Briefly tested with emacs-20.2 and timidity.el TiMidity++1.0.1 -- 1.2.1

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
;;		 (require 'dired-dd-timidity))))))

;; If (eq 'timidity-mode major-mode) then already provided.
;; (require 'timidity)
(require 'dired-dd)

;;
;; Handler function(s) dealing with timidity's buffer.
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
;; use variables such as `modifiers' `event' `start-window' etc.,
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

;; Just for one midi file. Defined, but unused.
(defun dired-dd-timidity-load-file (arglist)
  "Cheap. Play just one file on top of the marked file."
  (let ((files (car arglist)) 
	(modifiers (car (cdr (reverse arglist)))))
    (cond
     ((and
       (eq 'timidity-mode major-mode)
       (and (= 1 (length (delq 'drag modifiers)))
	    (eq 'shift (car (member 'shift modifiers))))) ; Shift+Drag
      ;; Call my one. I don't know why timidity-load-file() won't run nicely.
      ;;(timidity-load-file (car files))
      (raw-timidity-load-file (car files))
      t)
     (t nil) )))

;; This is the same as timidity-load-file() in timidity.el, except
;; that (interactive "FMIDI file: ") => (interactive "fMIDI file: ")
;; I don't know why my emacs-20.2 won't run timidity-load-file().
;; Defined, but unused.
;;
;; emacs_c.c and timidity.el sounds like 
;; timidity-next-files and timidity-prev-files is updated automatically by this:
(defun raw-timidity-load-file (fname)
  ;; (interactive "fMIDI file: ")
  (if fname
      (progn
	(setq timidity-playing-flag t)
	(timidity-run)
	(setq fname (timidity-expand-file-name fname))
	(garbage-collect)
	(process-send-string timidity-cmd-process
			     (format "L\nPLAY %s\n" fname))
	(message "MIDI File: %s" fname))
    (setq timidity-playing-flag nil)))

;; Load bunch of midi files into timidity-next-files, then play a file on top.
(defun dired-dd-timidity-load-file-list (arglist)
  "Cheap drag-and-drop handler for Timidity mode buffer:
whether operation should be done or not is determined by examining major-mode,
modifier keys of the drop event etc., within this function.
Returns non-nil always timidity's playing command is called, otherwise nil.
Multiple files are handed over to play file list."
  (let ((files (car arglist)) 
	(modifiers (car (cdr (reverse arglist)))))
    (cond
     ((and
       (eq 'timidity-mode major-mode)
       (equal '(drag) modifiers))	; Plain drag only.
      ;;(goto-char (car (reverse arglist))) ; drop point means nothing.
      ;; Set play list, and call timidity-next-file().  Straight order.
      (setq timidity-next-files (append files timidity-next-files))
      (timidity-next-file)
      t)
     (t nil) )))

;; Initializer for debug:
;; (setq timidity-next-files nil)

;; After you defined the handler function,
;; register it in dired-dd-non-dired-drop-handlers in a list form:
;; S-drag may be eaten up by dired-dd-insert-file, so place it on top:
;;(setq dired-dd-non-dired-drop-handlers
;;      (cons 'dired-dd-timidity-load-file
;;	    dired-dd-non-dired-drop-handlers))
(setq dired-dd-non-dired-drop-handlers
      (cons 'dired-dd-timidity-load-file-list
	    dired-dd-non-dired-drop-handlers))

(provide 'dired-dd-timidity)
