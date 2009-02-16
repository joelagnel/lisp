;; Time-stamp: <2001-04-19 13:55:05 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Filename: runscript.el
;; Package: runscript
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.0alpha

(defvar runscript-version "0.0alpha")

;;COMMENTARY:
;; runscript helps you run simple shell-scripts in an enhanced way. 
;; runscript allows your shell-script to give "inputs" to the programs
;; it is running.  A normal shell-script cannot do that.  For
;; instance, a shell-script starts lisp, and wants to input (+ 1 2 3)
;; to it.  Runscript allows you to do that.

;; Yes, You can do all this using runshell.el or using the
;; perl-utility expect, but runscript is just an easy interface to
;; runshell.  With runscript, you don't have to deal with any lisp.
;; You simply write your inputs in a script file, and runscript runs
;; the script file...



;; ALSO SEE: runshell.el, expect.el, perl's expect.

;; To USE:  Drop this file in your load-path, and type M-x runscript

(require 'lines)
(require 'runshell)
(require 'cl)

(defun runscript (file)
  "Runs shell-script in file FILE.  
The file's last line will NOT be read if it the file does not end in
newline.  This function makes sure you (optionally) add that newline.
The shell-scripts can also contain inputs for the programs that the
shell may call.. this is something you cannot do in tcsh directly.."
  (interactive "f")
  (find-file file)
  (let ((require-final-newline "ask"))
    (write-file file))
  (runscript-parse file))

(defun runscript-parse (file)
  (runshell)
  (let ((filename 
	 (file-name-nondirectory file)))
    (set-buffer filename)
    (goto-char (point-min))
    (lines-narrow-initial)
    (let ((maxlines (lines-max)))
      (while (< (lines-what) maxlines)
	(runshell-input (lines-at-point))
	(ignore-errors (next-line 1))
	))
    (widen)
    )
)


(provide 'runscript)

;;; runscript.el ends here
