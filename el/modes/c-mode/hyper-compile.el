;; hyper-compile.el --- Different styles of compile with completion

;; Copyright (C) 2003 Paolo Gianrossi 

;; Emacs Lisp Archive Entry
;; Filename: hyper-compile.el
;; Author: Paolo Gianrossi <paolino.gnu@disi.unige.it>
;; Maintainer: Paolo Gianrossi <paolino.gnu@disi.unige.it>
;; Version: 1.2
;; Created: 03/03/03
;; Revised:05/12/03
;; Keywords: compile developing programming
;; Description: Different styles of compile with completion
;; URL: http://digilander.iol.it/linsky/emacs/

;;  This file is not part of GNU Emacs.


;; This program is free software;  you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as  
;; published by the Free Software Foundation; either version 2 of the 
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but 
;; WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See the  GNU 
;; General Public License for more details.

;; You  should  have received  a copy of the GNU General Public 
;; License  along with this program; if not, write to the Free 
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 
;; 02111-1307, USA.

;;; Commentary:

;; This package provides `hyper-compile' function.
;; It allows you to define different compilation styles with autocompletion
;;
;; To use this package, add these lines to your .emacs file:
;;     (load-library "hyper-compile")

;;; Code:

(require 'compile)

(defvar hyper-compile-alist '(
  ("\\.c$"    . c-compilations-alist)
  ("\\.[cC]+[Pp]*$" . cpp-compilations-alist)
  ("\\.java$" . java-compilations-alist)
  ("\\.f90$" . fortran90-compilations-alist)
  ("\\.[Ff]$" . fortran77-compilations-alist)
  ("\\.tex$" . tex-compilations-alist)
  ("\\.texi$" . texinfo-compilations-alist)
  ("\\.pl$" . perl-compilations-alist)
  ("\\.cgi$" . perl-compilations-alist)

;; ("\\.el$"   . (emacs-lisp-byte-compile))   TO BE DONE SOMEHOW!
)  "List of compile commands. In argument of `compile',
some keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")

(defvar c-compilations-alist '(
			       ("basic" . "gcc %f -o %n")
			       ("basic w/math" . "gcc %f -lm -o %n")
			       ("make" . "make")
			       ("make all" . "make all")
			       ("make -k" . "make -k")
			       ("optimized" . "gcc -O2 %f -o %n")
			       ("optimized 1" . "gcc -O1 %f -o %n")
			       ("kernel" . "gcc -DKERNEL -O2 %f -o %n")
)
  
  "List of compile commands for the C language. When used with with `compile', some 
keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")
			       
(defvar cpp-compilations-alist '(
				 ("basic" . "g++ %f -o %n")
				 ("basic w/math" . "g++ %f -o %n")
				 ("make" . "make")
				 ("make all" . "make all")
				 ("make -k" . "make -k")
				 ("optimized" . "g++ -O2 %f -o %n"))
  "List of compile commands for the C++ language. When used with `compile', some 
keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")

(defvar java-compilations-alist '(
				  ("javac" . "javac %f")
				  ("gcj" . "gcj %f -o %n")
				  ("make" . "make")
				  ("make all" . "make all")
				  ("make -k" . "make -k")
)
  "List of compile commands for the Java language. When used with `compile', some 
keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")

(defvar fortran90-compilations-alist '(
					("basic" . "f90 %f -o %n")
					("make" . "make")
					("make all" . "make all")
					("make -k" . "make -k")
    )
  "List of compile commands for the Fortran 90 language. When used with `compile', some 
keywords beginning with '%' will be replaced by:
   
  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")

(defvar fortran77-compilations-alist '(
				       ("basic" . "f77 %f -o %n")
)
  "List of compile commands for the Fortran 77 language. When used with `compile', some 
keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")

 (defvar tex-compilations-alist '(
 				 ("tex" . "tex %f")
 				 ("latex" . "latex %f")
 				 ("make" . "make")
 				 ("make all" . "make all")
 				 ("make -k" . "make -k")
 )
   "List of compile commands for TeX and Latex. When used with `compile', some 
 keywords beginning with '%' will be replaced by:

   %F  absolute pathname            ( /usr/local/bin/netscape.bin )
   %f  file name without directory  ( netscape.bin )
   %n  file name without extention  ( netscape )
   %e  extention of file name       ( bin )

 ")

(defvar texinfo-compilations-alist '(
				     ("info" . "makeinfo %f")
				     ("force info" . "makeinfo --force %f")
				     ("html" . "makeinfo --html %f")
				     ("force html" . "makeinfo --force --html %f")
				     ("xml" . "makeinfo --xml %f")
				     ("docbook" . "makeinfo --docbook %f")
				     ("make" . "make")
				     ("make all" . "make all")
				     ("make -k" . "make -k")
)
  "List of compile commands for Texinfo. When used with `compile', some 
keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")

(defvar perl-compilations-alist '(
				  ("basic" . "perl -cw %f")
				  ("make" . "make")
				  ("make all" . "make all")
				  ("make -k" . "make -k")
)
  "List of compile commands for the Perl language. When used with `compile', some 
keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")

(defvar hyper-compile-replace-alist '(
  ("%F" . (buffer-file-name))
  ("%f" . (file-name-nondirectory (buffer-file-name)))
  ("%n" . (file-name-sans-extension
           (file-name-nondirectory (buffer-file-name))))
  ("%e" . (file-name-extension (buffer-file-name)))
))

(defvar hyper-before-compile-hook nil
  "*Normal hook that is run before actual compilation is performed by `hyper-compile'."
)
(defvar hyper-after-compile-hook nil
  "*Normal hook that is run after actual compilation is performed by `hyper-compile'."
)

(defvar hyper-compile-styles-history nil)
(make-local-variable 'hyper-compile-styles-history)

(defvar hyper-compile-compilestr-history nil)
(make-local-variable 'hyper-compile-compilestr-history)



(defun hyper-compile ()
(interactive)
(let ( (name (buffer-file-name))
       (case-fold-search nil)
       (hcal hyper-compile-alist)
       (rplc hyper-compile-replace-alist)
       (type-o-file nil)
       (type-o-compile nil)
       (compile-string nil)
       (done nil)
)
  
  (if (not name)
      (error "Buffer appears to have no file name...")
    )
  
  (while (and hcal (not done))
    (if (string-match (car (car hcal)) name )
	(progn 
	  (setq type-o-file (eval (cdr (car hcal))))
	  (setq done t)
	  )
      )
    (setq hcal (cdr hcal))
    )
  (if (not type-o-file)
      (call-interactively 'compile))

  (setq type-o-compile (completing-read "choose compilation style: "  type-o-file nil nil nil 
					hyper-compile-styles-history))

  (setq compile-string (cdr (assoc type-o-compile  type-o-file)))
  ;; substitution
  (while rplc  
    (while (string-match (car (car rplc)) compile-string)
      (setq compile-string (replace-match (eval (cdr (car rplc))) t nil compile-string)))
    (setq rplc (cdr  rplc)) )
  (setq compile-string (read-input "string to compile: " compile-string hyper-compile-compilestr-history))
  (run-hooks 'hyper-before-compile-hook)
  (compile compile-string)
  (run-hooks 'hyper-after-compile-hook)
  )
)

(provide 'hyper-compile)