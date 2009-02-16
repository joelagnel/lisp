;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:          cl-obvius.el
;;; Author:        Eero Simoncelli 
;;; Description:   Extensions to cl-shell.el for running OBVIUS.
;;; Creation Date: 11 March, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To use this file, put the following lines in your .emacs file:
;;;
;;;   (setq load-path (cons "<directory-containing-this-file>" load-path))
;;;   (setq *obvius-program* "<pathname-of-obvius-executable>")
;;;   (autoload 'run-obvius "<pathname-of-this-file>" "" t)
;;;
;;; Then just type "M-x run-obvius" in emacs...  Note that this file
;;; requires the file cl-shell.el, which should reside in the same
;;; directory.  If you want to add more key bindings, define a
;;; function called cl-obvius-hook to do this.  Only bindings that
;;; everyone agrees on should be put in this file!

(require 'cl-shell)

(defvar *obvius-program* "obvius"
  "Pathname of the OBVIUS program to be run by a call to run-obvius.")

;;; Command to load pre-existing lisp world containing obvius
(defun run-obvius (&optional obvius-program)
  "Run OBVIUS (Object-Based Vision and Image Understanding System) as an emacs
sub-shell.  With prefix arg, prompt for an executable pathname.  Otherwise, use
the default specified by the variable *obvius-program*."
  (interactive "P")
  (cond ((null obvius-program)		;use default program
	 (setq obvius-program *obvius-program*))
	((numberp obvius-program)		;user called with prefix argument
	 (setq obvius-program
	       (read-file-name "OBVIUS Program: "
			       *obvius-program* *obvius-program* t))))
  (let ((*cl-replacement-prompt* "OBVIUS> "))
    (run-cl obvius-program))
  (cl-add-obvius-key-bindings)	;add bindings to cl-shell-mode and lisp-mode
  (run-hooks 'cl-obvius-hook))

;;; Setup local key definitions for the *lisp* buffer and all
;;; lisp-mode buffers.
(defun cl-add-obvius-key-bindings ()
  (define-key cl-shell-mode-map "\C-c\C-l" 'obvius-load-image)
  (define-key cl-shell-mode-map "\C-c\C-s" 'obvius-save-image)
  (define-key lisp-mode-map "\C-c\C-l" 'obvius-load-image)
  (define-key lisp-mode-map "\C-c\C-s" 'obvius-save-image)
  (define-key lisp-mode-map "\C-cf" 'obvius-compile-load-file)
  )

(setq *cl-definition-regexp-alist*
      (append '((CLASS . "(def\\(-simple-\\)?class[ \t\n]*%s"))
	      *cl-definition-regexp-alist*))

(defvar *default-obvius-directory* "/"
  "*The default directory used for loading and saving OBVIUS images.")

(defun obvius-compile-load-file (pathname)
  "Compile and load file of current buffer into the CL process."
  (interactive
   (let ((default-file-name buffer-file-name))
     (list
      (read-file-name "CL compile-load file: " default-file-name default-file-name t))))
  (let ((buffer (get-file-buffer pathname)))
    (if (and buffer 
	     (buffer-modified-p buffer)
	     (yes-or-no-p 
	      (concat "Buffer " (buffer-name buffer) " modified, save it first? ")))
	(save-buffer buffer)))
  (let ((cl-compile-load-command "(compile-load \"%s\")\n"))
    (if *cl-echo-commands*
	(cl-send-string-with-echo (format cl-compile-load-command pathname))
	(cl-send-string
	 (concat "(progn "
		 (format cl-compile-load-command pathname)
		 "(values))\n")))))

;;; Load an image into OBVIUS, making use of emacs filename completion.
(defun obvius-load-image ()
  "Load an image file into OBVIUS, changing the *default-obvius-directory*
to the directory of the image."
  (interactive)
  (let ((fn (read-file-name "Image file name: "
			    *default-obvius-directory* nil t)))
    (cl-send-string-with-echo
     (format "(load-image \"%s\")" (expand-file-name fn)))
    (setq *default-obvius-directory* 
	  (file-name-directory (substring (expand-file-name fn) 0 -1)))))

(defun obvius-save-image ()
  "Save an OBVIUS image in datfile format."
  (interactive)
  (let ((fn (read-string "Save to datfile: " *default-obvius-directory*)))
    (cl-send-string-with-echo
     (format "(save-image (getp viewable) \"%s\")" (expand-file-name fn)))))

(defun obvius-view-image ()
  (interactive)
  (let ((fn (read-file-name "Image file name: "
			    *default-obvius-directory* nil t)))
    (cl-send-string-with-echo
     (format "(view-image \"%s\")" (expand-file-name fn)))
    (setq *default-obvius-directory* 
	  (file-name-directory (substring (expand-file-name fn) 0 -1)))))

;;; Add some more special forms to the indentation list - see
;;; cl-indent.el for more information. The number refers to
;;; the number of special forms passed as arguments.
(put 'loop-over-image-pixels    'common-lisp-indent-hook 1)
(put 'loop-over-image-positions 'common-lisp-indent-hook 2)
(put 'def-simple-class          'common-lisp-indent-hook 'defun)
(put 'with-result               'common-lisp-indent-hook 1)
(put 'with-displaced-vectors    'common-lisp-indent-hook 1)
(put 'with-local-arrays         'common-lisp-indent-hook 1)
(put 'with-local-viewables      'common-lisp-indent-hook 1)
(put 'catch-errors              'common-lisp-indent-hook 1)
(put 'with-locked-pane          'common-lisp-indent-hook 1)
