;;;-*- auto-recompile: t -*-
;; runshell-shortcuts.el
;; Time-stamp: <2001-11-05 15:52:56 deego>
;; Copyright (C) Deepak Goel <deego@glue.umd.edu> 2001
;; Emacs Lisp Archive entry
;; Filename: runshell-shortcuts.el
;; Package: runshell-shortcuts
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: nil
;; Punchline: interactive shell-conveniences..
;; For latest version/info:http://www.glue.umd.edu/~deego
;; GPL'ed under GNU'S public license..
;; See also: runshell.el, eshell.el, ansi-color.el

;; Quick start:
(defun runshell-shortcuts-quick-start ()
  (interactive)
  (switch-to-buffer-other-window  "*doc*")
  (kill-region (point-min) (point-max))
  (insert
  "Load this if you like.. then, use rcd to change-directories in
eshell (or even shell etc. ).   Use rcd-fix-eshell-escape-sequences
to remove any escape sequences from eshell-programs.
THIS FILE MAY NOT RESPECT NAMESPACES!

"))
;;
;;
;;
;;
;; Introduction:
(defun runshell-shortcuts-introduction ()
  (interactive)
  (switch-to-buffer-other-window  "*doc*")
  (kill-region (point-min) (point-max))
  (insert
  "Convenient shortcuts for shells.. some built on top of runshell, some
on eshell. Since this is a shortcuts file, it does NOT always respect
namespace conventions.

Ever wish that when you did \"cd ..\" or M-x cd, emacs remembered your
directory with the full name, and also, your shell tracked the changes
correctly (dir-track mode does not always work correctly for me).
Even if you got that working, say via M-x rcd, ever wish you didn't
have to type that M-x (sometimes a long ESC-x for me)?  Come in
eshell/rcd.  You just type \"rcd <path> RET\" in yr eshell, or you just
type \"rcd RET\" and interactively complete the listing.  These listings
are full pathnames, so your M-p's can quickly you back to any
previously visited directories..

Also provides an 'enhancement' to locate-library.


"))
;;
;;
;;
;;; Commentary:
(defun runshell-shortcuts-commentary ()
  ""
  (interactive)
  (switch-to-buffer-other-window  "*doc*")
  (kill-region (point-min) (point-max))
  (insert "
 ... USE M-X EDLIB-DOC OR C-X SPC SPC L HERE...
        TO INSERT DOCUMENTATION HERE


"))
;;
;;
;;
;;
;;==========================================
;;  Code:


(defvar runshell-shortcuts-version "0.1release")
 
;;;###autoload
(defalias 'rcd-library-my 'rcdlibrarymy)
;;;###autoload
(defun rcdlibrarymy (&optional lisp-p)
  "Is meant to be interactive!!! Is an enhancement to locate-library..
Switches current dir. to the valid library, so you can easily find-file.."
  (interactive "P")
  (let ((this-buffer (buffer-name)))
    (unless (string= this-buffer "*scratch*")
      (delete-other-windows)
      (split-window-vertically)
      ;; so that only the default dir. of *this* buffer is affected
      (switch-to-buffer "*scratch*"))
    (let*
	((lib 
	  (if (null lisp-p)
	      (call-interactively 'locate-library)
	    (let ((lib-name 
		   (read-string 
		    "Locate common-lisp library \(no extension needed\) named:"
		    )))
	      (unless (boundp 'common-lisp-path-my)
		;; defined in .emacs.elliott..
		(set-common-lisp-path-my))
	      (or 
	       (locate-library (concat lib-name ".lisp")
			       t common-lisp-path-my)
	       (locate-library (concat lib-name ".fasl")
			       t common-lisp-path-my)
	       (locate-library (concat lib-name )
			       t common-lisp-path-my)))))
	 (lib-dir (ignore-errors (first (dir-file-ext-my lib)))))
      (if (not (null lib))
	  (progn
	    (ignore-errors (runshell-cd lib-dir))
	    (message lib))
	(message "Not found..")))
    ;;(other-window 1)
    ))




 


;;;###autoload
(defmacro save-active-buffer-my (&rest body)
  "Is like save-excursion, but in regards to visible buffer, not
current-buffer.. Thus, the buffer visible before execution of the
command is restored.."
  `(let ((tmpsym (gensym)))
     (set tmpsym (buffer-name))
     ,@body
     (switch-to-buffer (eval tmpsym))))

;;;###autoload
(defmacro save-active-buffer-ignore-errors-my (&rest body)
  "Is like save-excursion, but in regards to visible buffer, not
current-buffer.. Thus, the buffer visible before execution of the
command is restored.. This *makes* sure you are back to the current buffer.."
  `(let ((tmpsym (gensym)))
     (set tmpsym (buffer-name))
     (ignore-errors ,@body)
     (switch-to-buffer (eval tmpsym))))

;;;###autoload
(defun concat-with-spaces-my (&rest strings)
  "Concats with spaces.. inefficient but who cares.."
  (if (null strings) nil
    (if (= (length strings) 1)
	(concat " " (first strings) " ")
      (concat  " " (first strings) (apply 'concat-with-spaces-my (rest
							   strings))))))

;;;###autoload
(defun eshell-shell-command-my (&rest args)
  (let
      ((eshell-prefer-to-shell t))
    (apply 'shell-command args)))



;;;###autoload
(defun eshell/lyn-shell (&rest args)
  "Like lynx... currently works on x-enabled terminals only..
but plan to make it 'browsable'"
  (save-active-buffer-ignore-errors-my
   (runshell-with-shell
   (let ((runshell-cd-upon-startup-p nil))
     (runshell-command
      (concat (apply 'concat-with-spaces-my "xterm -e lynx " args) "& "
	      ))
     (split-window-this-vertically-my)
     ))))

;;;###autoload
(defun eshell/lyn (&rest args)
  "Like lynx.
So named so as to avoid conflict with lynx."
  (runshell-input
   (concat (apply 'concat "xterm -e lynx " args) " &" ) )
)


;;;Mon Jun 18 14:11:44 2001
;; the author has submitted a patch, so am commenting out my own
;; definition once again..
;; Defined by Deepak Goel <deego@glue.umd.edu> 6/18/01
;; The previous defintion was very highly file-dependent and did not
;; allow the user to create new definitions easily.. This one simply
;; checks whether a lisp function exists or not, independent of filename..
;; (defun eshell-find-alias-function (name)
;;   "Check whether a function called 'eshell/NAME' exists.."

;;   (let (existsp)
;;     (with-temp-buffer
;;       (insert "(setq existsp
;;                    (ignore-errors
;;                      (symbol-function (quote eshell/"
;; 	           name "))))")
;;       (eval-buffer)
;;       (if
;; 	  existsp
;; 	  (with-temp-buffer
;; 	    (insert "(setq existsp (quote eshell/" name "))")
;; 	    (eval-buffer)
;; 	    existsp)
;; 	nil))))

(defun split-window-this-vertically-my ()
  "Ensures 2 copies of the current window.."
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  )


(defun eshell/rcd (&rest args)
  "replacement for cd..
See comments at the beginning of file..
Type rcd <name> or rcd <RET>.  when latter, can then use arrows to get
history..  works with eshell..

Henceforth, use cd'ing for normal cd.. use rcd RET once in a while to
ensure that this dir. goes on the history-list..
"
  (if (null args)
      (call-interactively 'runshell-cd)
    (apply 'runshell-cd args))
  (goto-char (point-max))
  ""
)

;;;###autoload
(defun rcd-fix-eshell-escape-sequcnes ()
  (interactive)
  (defvar eshell-preoutput-filter-functions nil)
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)
)

;;;###autoload
(defun eshell/parse (arg1 &rest others)
  "Easy parsing of commands..Consider also stuff like  
~ eshell-parse-command 'ls -al'
or
\(eshell-parse-command \"ls -al\"\)"
  (interactive)
  (ignore-errors (eshell/which arg1))
  (eshell-parse-command arg1 )
  )


;;;###autoload
(defun eshell/mor (&rest args)
  "Invoke `view-file' on the file.
\"more +42 foo\" also goes to line 42 in the buffer.

Written by Kai.. on gnu.emacs.help, in response to my complaints that
the default more does not work on eshell..
"
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (goto-line line))
~/tmp $ cpp hi1 hi2 hi3 ttt
 cp -p  hi1 hi2 hi3 ttt
~/tmp $ ls
hi  hi.tar  hi1  hi2  print-home.txt  tt  tt.tar  ttt
~/tmp $      (view-file (pop args)))))


(defun eshell/cpp (&rest args)
  (message(apply 'concat-with-spaces-my "cp -p " args)))

(provide 'runshell-shortcuts)

;;; runshell-shortcuts ends here..

