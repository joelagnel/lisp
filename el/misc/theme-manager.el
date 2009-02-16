;;; theme-manager - Lets you walk through your color themes and sets the
;;; last one you land on as your favorite.
;;; 
;;; Copyright (c) 2006 Jeremy English <jhe@jeremyenglish.org>
;;; 
;;; Permission to use, copy, modify, distribute, and sell this software and its
;;; documentation for any purpose is hereby granted without fee, provided that
;;; the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting
;;; documentation.  No representations are made about the suitability of this
;;; software for any purpose.  It is provided "as is" without express or 
;;; implied warranty.
;;; 
;;; Created: 03-February-2006 
;;; 
;;; theme-next and theme-prev are the functions that you want to bind to
;;; your keys. I use the following in xemacs.
;;; 
;;; (define-key global-map [(f10)] 'theme-prev)
;;; (define-key global-map [(f11)] 'theme-next)
;;;
;;; If you want to override the file where the state is saved then
;;; set the variable *my-current-theme-file*
;;;
;;; Now compatible with emacs 21 02/07/2006 
;;;
;;; define-function missing from emacs 23. Changed to defalias
;;; 08/26/2006


(require 'color-theme)

;; Include all of the themes.
(setq my-color-themes color-themes)

;; You can define your own list if you want a subset.
;;  (setq my-color-themes (list 'color-theme-billw 'color-theme-jsc-dark 
;;                              'color-theme-sitaramv-solaris 'color-theme-resolve
;;                              'color-theme-classic 'color-theme-jonadabian-slate
;;                              'color-theme-kingsajz 'color-theme-shaman
;;                              'color-theme-subtle-blue 'color-theme-snowish
;;                              'color-theme-sitaramv-nt 'color-theme-wheat))

(defvar *my-current-color-theme* nil)
(defvar *my-color-theme-idx* 0)
(defvar *my-current-theme-file* nil)

(defun set-current-theme-file ()
  "Set up the theme file based on the version of emacs"
  (if (null *my-current-theme-file*)
      (progn
	(if (string-match "XEmacs" emacs-version)
	    (setf *my-current-theme-file*
		  (concat (user-home-directory) "/my-current-theme.el"))
	  (setf *my-current-theme-file* "~/my-current-theme.el")))
    t))

(defvar *my-current-theme-file* 
  (concat (user-home-directory) "/my-current-theme.el"))

(defun theme-load-state ()
  "Try to load the state file. If the file is not found go the next theme."
  (if (file-exists-p *my-current-theme-file*)
      (load *my-current-theme-file*)
    (theme-next)))

(defun car-maybe (item) ;
  "Figure out if we need to call car. If we do call it"
  (interactive)
  (cond
   ((atom item) item)
   (t (car item))))

;;; Closure in emacs. It's ugly but it works.
(defun make-wrap-inc (min max)
  (lexical-let ((tmp-min min)
		(tmp-max max))
    #'(lambda (i)
	(incf i)
	(cond ((> i tmp-max)
	       (setf i tmp-min)))
	i)))

(defun make-wrap-dec (min max)
  (lexical-let ((tmp-min min)
		(tmp-max max))
    #'(lambda (i)
	(decf i)
	(cond ((< i tmp-min)
	       (setf i tmp-max)))
	i)))

(defmacro theme-make-walker ( name direction ) 
  "Direction should be incw or decw"
  `(defun ,name ()
     (interactive)
     (setf *my-color-theme-idx* (,direction *my-color-theme-idx*))
     (let ((theme (elt my-color-themes *my-color-theme-idx*)))
       (theme-load theme))))

(defalias 'incw 
  (make-wrap-inc 0 
		 (list-length my-color-themes)))

(defalias 'decw 
  (make-wrap-dec 0 
		 (list-length my-color-themes)))

(defun theme-load (theme)
  "Set theme as the current theme and save the state."
  (funcall (car-maybe theme))
  (setf *my-current-color-theme* theme)
  (theme-write-current)
  (theme-describe))

(defun theme-describe () 
  "Show the current theme."
  (interactive)
  (message "%S" *my-current-color-theme*))

(defun theme-write-current ()
  "Save the state to a file that is loaded the next time emacs starts."
  (interactive)
  (let (buf-name)
    (save-excursion
      (unwind-protect
	  (progn
	    (setq buf-name (get-buffer-create "tmp-theme-buffer"))
	    (set-buffer buf-name)
	    (erase-buffer)
	    (insert "(" 
		    (format "%s" (car-maybe *my-current-color-theme*)) ")\n"
		    "(setf *my-current-color-theme* " 
		    (format "'%s" *my-current-color-theme*) ")\n"
		    "(setf *my-color-theme-idx* " 
		    (format "%s" *my-color-theme-idx*) ")")
	    (write-region (point-min) (point-max) 
			  *my-current-theme-file* nil nil))
	(if buf-name (kill-buffer buf-name)))))
  nil)

(set-current-theme-file)

(theme-make-walker theme-next incw)
(theme-make-walker theme-prev decw)

(theme-load-state)

(provide 'theme-manager)
