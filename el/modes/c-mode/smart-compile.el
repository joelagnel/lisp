;;; smart-compile.el --- an interface to `compile'

;; Copyright (C) 1998-2006  by Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; $Id: smart-compile.el 28 2006-10-20 21:29:18Z zenitani $
;; Keywords: tools, unix
;; Created: 1998-12-27
;; Compatibility: Emacs 20, 21
;; URL(en): http://homepage.mac.com/zenitani/comp-e.html
;; URL(jp): http://homepage.mac.com/zenitani/elisp-j.html#smart-compile

;; Contributors: Sakito Hisakura

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides `smart-compile' function.
;; You can associates a particular file with a paticular compile functions,
;; by editing `smart-compile-alist'.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'smart-compile)
;;
;; Note that it requires emacs 21 or later.

;;; Code:

(defgroup smart-compile nil
  "An interface to `compile'."
  :group 'processes
  :prefix "smarct-compile")

(defcustom smart-compile-alist '(
  (emacs-lisp-mode    . (emacs-lisp-byte-compile))
  (html-mode          . (browse-url-of-buffer))
  (html-helper-mode   . (browse-url-of-buffer))
  (octave-mode        . (run-octave))
  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n")
  ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
  ("\\.m\\'"          . "gcc -O2 %f -lobjc -lpthread -o %n")
  ("\\.java\\'"       . "javac %f")
  ("\\.php\\'"        . "php -l %f")
  ("\\.f90\\'"        . "f90 %f -o %n")
  ("\\.[Ff]\\'"       . "f77 %f -o %n")
  ("\\.cron\\(tab\\)?\\'" . "crontab %f")
  ("\\.tex\\'"        . (tex-file))
  ("\\.pl\\'"         . "perl -cw %f")
  ("\\.rb\\'"         . "ruby -cw %f")
)  "List of compile commands. In argument,
some keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

"
   :type '(repeat
	   (cons
	    (choice
	     (regexp :tag "Filename pattern")
	     (function :tag "Major-mode"))
	    (choice
	     (string :tag "Compilation command")
	     (sexp :tag "Lisp expression"))))
   :group 'smart-compile)

(defvar smart-compile-replace-alist '(
  ("%F" . (buffer-file-name))
  ("%f" . (file-name-nondirectory (buffer-file-name)))
  ("%n" . (file-name-sans-extension
           (file-name-nondirectory (buffer-file-name))))
  ("%e" . (or (file-name-extension (buffer-file-name)) ""))
  ))

(defvar smart-compile-check-makefile t)
(make-variable-buffer-local 'smart-compile-check-makefile)

(defcustom smart-compile-make-program "make "
  "The command by which to invoke the make program."
  :type 'string
  :group 'smart-compile)


;;;###autoload
(defun smart-compile ()
  "An interface to `compile'.
It calls `compile' or other compile function,
which is defined in `smart-compile-alist'."
  (interactive)
  (let ((name (buffer-file-name))
        (not-yet t))
    
    (if (not name)
        (error "cannot get filename."))

    ;; make?
    (if (and
         (not
          (and (local-variable-p 'compile-command)
               compile-command))
         smart-compile-check-makefile
         (or (file-readable-p "Makefile")
             (file-readable-p "makefile"))
         (y-or-n-p "Makefile is found. Try 'make'? ")
         )
        (set (make-local-variable 'compile-command)
             smart-compile-make-program)
      (setq smart-compile-check-makefile nil)
      )

    (if (and (local-variable-p 'compile-command)
              compile-command)
        (progn
          (call-interactively 'compile)
          (setq not-yet nil)
          )
      )

    ;; compile
    (let( (alist smart-compile-alist) 
          (case-fold-search nil)
          (function nil) )
      (while (and alist not-yet)
        (if (or
             (and (symbolp (caar alist))
                  (eq (caar alist) major-mode))
             (and (stringp (caar alist))
                  (string-match (caar alist) name))
             )
            (progn
              (setq function (cdar alist))
              (if (stringp function)
                  (progn
                    (set (make-local-variable 'compile-command)
                         (smart-compile-string function))
                    (call-interactively 'compile)
                    )
                (if (listp function)
                    (eval function)
                    ))
              (setq alist nil)
              (setq not-yet nil)
              )
          (setq alist (cdr alist)) )
        ))

    ;; If compile-command is not defined and the contents begins with "#!",
    ;; set compile-command to filename.
    (if (and not-yet
             (not (memq system-type '(windows-nt ms-dos)))
             (not (string-match "/\\.[^/]+$" name))
             (not
              (and (local-variable-p 'compile-command)
                   compile-command))
             )
        (save-restriction
          (widen)
          (if (equal "#!" (buffer-substring 1 (min 3 (point-max))))
              (set (make-local-variable 'compile-command) name)
            ))
      )
    
    ;; compile
    (if not-yet (call-interactively 'compile) )

    ))

(defun smart-compile-string (arg)
  "Document forthcoming..."
  (interactive)
  (let ((rlist smart-compile-replace-alist))
    (while rlist
      (while (string-match (caar rlist) arg)
        (setq arg
              (replace-match
               (eval (cdar rlist)) t nil arg)))
      (setq rlist (cdr rlist))
      )
    )
  arg)

(provide 'smart-compile)

;;; smart-compile.el ends here
