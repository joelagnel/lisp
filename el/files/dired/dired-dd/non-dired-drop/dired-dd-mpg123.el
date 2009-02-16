;; dired-dd-mpg123.el --- Really experimental.

;; Freeware. GPL2 applied. (c) 2000 S.Namba <sn@asahi-net.email.ne.jp>

;; dired-dd Drag&Drop interface to mpg123.el:
;; drag-and-drops mp3 file(s), playlist(s), or directory(s)
;; into the *mpg123* buffer, and play them.

;;
;; To get dired-dd, see
;; http://www.asahi-net.or.jp/~pi9s-nnb/dired-dd-home.html
;; To get mpg123.el, see
;; http://www.gentei.org/~yuuji/software/mpg123el/
;;

;; INSTALL:  In ~/.emacs should be included:
;;
;;  (add-hook
;;   'mpg123-load-hook
;;   (function
;;    (lambda ()
;;      (if window-system
;;	  (require 'dired-dd-mpg123)))))
;;
;; This program requires `dired-dd', which should be setup correctly
;; in other place.  Refer to `dired-dd' documentation.

;; Tue Nov 14 19:27:16 2000 1st draft

(require 'dired-dd)
(require 'mpg123)
(provide 'dired-dd-mpg123)

;; Current arglist passed by dired-dd: (fn-list to modifiers end-point)
;; You can also use variables such as `modifiers', `event', `start-window'
;; etc., which are defined in upper functions.  Refer to the caller functions
;; dired-dd-drag-drop and dired-dd-drop-from-to in dired-dd.el,
;; for the detail of the parameters.

(defun dired-dd-mpg123-drop-handler (arglist)
  "`dired-dd' drop handler for `mpg123'.
The dropped first file or directory is loaded to the playlist by command
`mpg123', then the rest of the files are added via `mpg123-add-new'.

Returns non-nil if it succeeded, otherwise nil."
  (let ((files (car arglist)) 
	(modifiers (car (cdr (reverse arglist)))) ; use it in arglist
	(firstdrop t)
	result)
    (cond
     ((eq major-mode 'mpg123-mode)
      (cond
       ;; C-drag-mouse-2, or S-drag-mouse-2 just adds files to the playlist.
       ((or (memq 'control modifiers)
	    (memq 'shift modifiers))
	(setq firstdrop nil)))
      ;; Multiple file handling 
      (setq result
	    (mapcar
	     (lambda (f)
	       (cond
		(firstdrop
		 (setq firstdrop nil)
		 (mpg123 f))
		(t (mpg123-add-new f))))
	     files))
      ;; result ;; No ! This is a list.  Should nil/non-nil !
      t)
     (t nil) )))

;; Register handler to the handler list.  add-hook() is good enough.
(add-hook 'dired-dd-non-dired-drop-handlers 'dired-dd-mpg123-drop-handler)
