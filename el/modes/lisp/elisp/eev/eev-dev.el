;;; eev-dev.el -- add support for e-scripts in Emacs.
;;; <<<<<<<< Use eev.el instead of this file >>>>>>>>

;; Copyright (C) 1999,2000,2001,2002,2003,2004,2005 Free Software
;; Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Author:     Eduardo Ochs <edrx@mat.puc-rio.br>
;; Maintainer: Eduardo Ochs <edrx@mat.puc-rio.br>
;; Version:    2005feb14
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex

;;; Commentary:

;; For a description of what eev and e-scripts are please see the
;; README.
;;
;; Note: in mid-2004 I decided to rewrite eev. The new version had
;; almost all code in a single file - called `eev-dev.el' - and had no
;; changelogs or docstrings. That will be fixed at some point in the
;; future.
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-dev.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-dev.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>
;;            and: <http://angg.twu.net/eev-current/eev-langs.el.html>

;; Structure of this file:
;; autoloads for external functions
;; environment variables
;; variables
;; tools for calling hyperlink functions interactively
;; basic hyperlinks (to text files and info nodes)
;; hyperlinks to the output of Emacs's help-like functions
;; hyperlinks to some things internal to Emacs (functions, variables, etc)
;; hyperlinks to the output of shell commands
;; hyperlinks to manpages
;; hyperlinks to files in html
;; hyperlinks to pages in dvi/ps/pdf documents
;; hyperlinks to anchors
;; hyperlinks to information about Debian packages
;; mass-producing hyperlink functions
;; mass-producing hyperlink functions: examples (debian-centric)
;; temporary highlighting (flashing)
;; evaluating sexps (alternatives to eval-last-sexp)
;; saving regions (as temporary scripts)
;; auxiliary functions for saving delimited ("bounded") regions
;; the default action on bounded regions
;; saving delimited regions
;; hyperlinks to key sequences and series of Emacs actions
;; eev-newbie and eev-demos
;; more tools
;; starting background processes
;; sending strings to external programs through "channels"
;; sending strings through "channels": high-level functions
;; eev keys mode
;; invading the global namespace
;; aliases for compatibility with previous versions



;;;
;;; autoloads for external functions
;;;

;; (find-elnode "Autoload")

(autoload 'Info-goto-node "info")
(autoload 'Info-find-node "info")
(autoload 'find-function-read "find-func")
(autoload 'pp-to-string "pp")
(autoload 'Man-fontify-manpage "man" nil t)
(autoload 'word-at-point "thingatpt")



;;;
;;; environment variables
;;;

;; (find-eevrcfile ".bashrc")
;; (find-eevrcfile ".zshrc")

(defun ee-expand (fname)
"Expand \"~\"s and \"$ENVVAR\"s in file names, but only at the beginning of the string."
  (cond ((string-match "^\\$\\([A-Za-z_][0-9A-Za-z_]*\\)\\(.*\\)" fname)
	 (concat (getenv (match-string 1 fname))
		 (match-string 2 fname)))
	((string-match "^\\(~\\([a-z][0-9a-z_]*\\)?\\)\\(/.*\\)?$" fname)
	 (concat (expand-file-name (match-string 1 fname))
		 (match-string 3 fname)))
	(t fname)))

(defun ee-setenv (envvar value)
  "In case the environment variable ENVVAR was not set set it to VALUE."
  (if (null (getenv envvar))
      (setenv envvar (ee-expand value))))

(ee-setenv "EEVDIR"    "~/eev-current")
(ee-setenv "EEVRCDIR"  "$EEVDIR/rcfiles")
(ee-setenv "EEVTMPDIR" "$EEVDIR/tmp")
(ee-setenv "EE"        "$EEVTMPDIR/ee.sh")
(ee-setenv "EEG"       "$EEVTMPDIR/ee.eeg")
(ee-setenv "EEGDB"     "$EEVTMPDIR/ee.gdb")
(ee-setenv "EETEX"     "$EEVTMPDIR/ee.tex")
(ee-setenv "EEC"       "$EEVTMPDIR/ee.c")
(ee-setenv "EETMPC"    "$EEVTMPDIR/tmp.c")
(ee-setenv "EEAOUT"    "$EEVTMPDIR/ee.aout")


;;;
;;; variables
;;;

;; (setq eeb-highlight-spec '(highlight 0.2))
(defvar ee-highlight-spec  '(highlight 0.75)) ; to do: rename highlight->flash
(defvar eeb-highlight-spec '(highlight 0.5))
(defvar eek-highlight-spec '(region 0.75))
(defvar eeflash-default '(highlight 0.5))

(defvar ee-eevdir       (ee-expand "$EEVDIR/"))
(defvar ee-eevtmpdir    (ee-expand "$EEVTMPDIR/"))
(defvar ee-eevrcdir     (ee-expand "$EEVRCDIR/"))
(defvar ee-file         (ee-expand "$EE"))
(defvar ee-file-tex     (ee-expand "$EETEX"))
(defvar ee-file-gdb     (ee-expand "$EEGDB"))
(defvar ee-file-generic (ee-expand "$EEG"))

(defvar eelatex-eevscript "cd $EEVTMPDIR/; latex tmp.tex && xdvi tmp.dvi &")
(defvar ee-delimiter-hash    "\n#\n")
(defvar ee-delimiter-percent "\n%\n")
(defvar ee-delimiter-semicolon "\n;;\n")

(defvar ee-find-man-flag nil)		; for asynchronous `ee-goto-position's
(defvar ee-find-man-pos-spec-list nil)	; for asynchronous `ee-goto-position's
(defvar ee-buffer-name nil)		; overridden by `let's
(defvar ee-arg nil)			; overridden by `let's
(defvar ee-once nil)			; overridden by `let's
(defvar ee-info-file nil)		; for eev-insert.el
(defvar ee-info-code nil)		; for eev-insert.el
(defvar ee-pop-up-windows nil)		; for an eegud hack, not working

(defvar eeb-defaults '(eev ee-delimiter-hash nil t t))

(defvar eesteps-pos 0)
(defvar eesteps-list ())
(defvar eechannel-default nil)

;; (defvar eev-mode-map nil)		; moved down to make the html nicer

(defvar code-c-d-list nil
  "Each (code-c-d C D) call generates an entry (C (ee-expand D)) in this list.
A new entry with the same C as a previous one removes the old one.
See `code-c-d-register'.")




;;;
;;; tools for calling hyperlink functions interactively
;;; inspired by: (find-efile "thingatpt.el")
;;;

(defmacro ee-maybe-showing-it (it)
  `(let ((value ,it))
     (if ,show-it (message "%S" value))
     value))

(defun ee-stuff-around-point (chars &optional show-it)
  (interactive "MChars: \np")
  (ee-maybe-showing-it
   (save-excursion
     (let* ((e (progn (skip-chars-forward  chars) (point)))
	    (s (progn (skip-chars-backward chars) (point))))
       (buffer-substring s e)))))

(defun ee-debpkgname-around-point (&optional show-it)
  (interactive "p")
  (ee-stuff-around-point "a-z0-9-+." show-it))

(defun ee-debpkgname-ask (&optional prompt show-it)
  (interactive (list nil t))
  (ee-maybe-showing-it
   (read-string (or prompt "Debian package name: ")
		(ee-debpkgname-around-point))))

(defun ee-manpagename-around-point (&optional show-it)  
  (interactive "p")
  (ee-stuff-around-point "A-Za-z0-9-+_:." show-it))

(defun ee-manpagename-ask (&optional prompt show-it)
  (interactive (list nil t))
  (ee-maybe-showing-it
   (read-string (or prompt "Manpage: ")
		(ee-manpagename-around-point))))



;;;
;;; basic hyperlinks (to text files and info nodes)
;;;

(defun ee-goto-position (&optional pos-spec &rest rest)
  (if (or pos-spec rest)
      (goto-char (point-min)))
  (when pos-spec
    (cond ((numberp pos-spec)
	   (forward-line (1- pos-spec)))
	  ((stringp pos-spec)
	   (search-forward pos-spec))
	  (t (error "This is not a valid pos-spec: %S" pos-spec)))
    (if rest (ee-goto-rest rest))))

(defun ee-goto-rest (list)
  (cond ((stringp (car list))
	 (search-forward (car list))
	 (ee-goto-rest (cdr list)))))

(defun find-fline (fname &rest pos-spec-list)
  (find-file (ee-expand fname))
  (apply 'ee-goto-position pos-spec-list))

(defun find-node (nodestr &rest pos-spec-list)
  (Info-goto-node nodestr)
  (apply 'ee-goto-position pos-spec-list))



;;;
;;; hyperlinks to the output of Emacs's help-like functions
;;;

(defun find-wottb-call (sexp bufname &rest pos-spec-list)
  "Hyperlink to functions that call `with-output-to-temp-buffer'.
First evaluate SEXP with a trick to not let it split the current window,
then switch to the buffer that it created (it must be called BUFNAME),
then go to the position specified by POS-SPEC-LIST.\n
 (Do I need to say that this is a horrible hack?)"
  (let ((same-window-buffer-names
	 (cons bufname same-window-buffer-names)))
    (eval sexp))
  (set-buffer bufname)			; why is this needed?
  (apply 'ee-goto-position pos-spec-list))

(defun find-eapropos (regexp &rest pos-spec-list)
  (interactive "sApropos symbol (regexp): ")
  (apply 'find-wottb-call '(apropos regexp) "*Apropos*" pos-spec-list))

(defun find-efunctiondescr (symbol &rest pos-spec-list)
  (interactive (find-function-read))
  (apply 'find-wottb-call '(describe-function symbol) "*Help*" pos-spec-list))

(defun find-evariabledescr (symbol &rest pos-spec-list)
  (interactive (find-function-read 'variable))
  (apply 'find-wottb-call '(describe-variable symbol) "*Help*" pos-spec-list))

(defalias 'find-evardescr 'find-evariabledescr)

(defun find-ekeydescr (key &rest pos-spec-list)
  (interactive "kFind function on key: ")
  (apply 'find-wottb-call '(describe-key key) "*Help*" pos-spec-list))

(defun find-efacedescr (face &rest pos-spec-list)
  (interactive (list (read-face-name "Describe face")))
  (apply 'find-wottb-call '(describe-face face) "*Help*" pos-spec-list))

(defun find-efaces (&rest pos-spec-list)
  (interactive)
  (apply 'find-wottb-call '(list-faces-display) "*Faces*" pos-spec-list))

(defun find-ecolors (&rest pos-spec-list)
  (interactive)
  (apply 'find-wottb-call '(list-colors-display) "*Colors*" pos-spec-list))

(defun find-efunctiond (function &rest pos-spec-list)
  (interactive (find-function-read))
  (apply 'find-wottb-call '(disassemble function) "*Disassemble*"
	 pos-spec-list))



;;;
;;; hyperlinks to some things internal to Emacs (functions, variables, etc)
;;;

(defun find-ebufferandpos (buffer-and-pos &rest rest)
  (if (not (bufferp (car buffer-and-pos)))
      (error "Bad (BUFFER . POS): %S" buffer-and-pos))
  (switch-to-buffer (car buffer-and-pos))
  (goto-char (cdr buffer-and-pos))
  (ee-goto-rest rest))

(defun find-efunction (symbol &rest rest)
  (interactive (find-function-read))
  (apply 'find-ebufferandpos (find-function-noselect symbol) rest))

(defun find-evariable (symbol &rest rest)
  (interactive (find-function-read 'variable))
  (apply 'find-ebufferandpos (find-variable-noselect symbol) rest))

(defun find-eCfunction (fun &rest rest)
  (interactive (find-function-read))
  (apply 'find-ebufferandpos
	 (find-function-search-for-symbol
	  fun nil (help-C-file-name (indirect-function fun) 'fun))
	 rest))

(defun find-eCvariable (symbol &rest rest)
  (interactive (find-function-read 'variable))
  (apply 'find-ebufferandpos
	 (find-variable-noselect
	  symbol (help-C-file-name symbol 'var))
	 rest))



(defun find-ebuffer (buffer &rest rest)
  (interactive "bBuffer: ")
  (switch-to-buffer buffer)
  (apply 'ee-goto-position rest))

(defalias 'find-escratchbuffer 'find-ebuffer) ; missing: check scratchness

(defun find-estring (str &rest pos-spec-list)
  (find-escratchbuffer (or ee-buffer-name "*string*"))
  (delete-region (point-min) (point-max))
  (insert str)
  (goto-char (point-min))
  (apply 'ee-goto-position pos-spec-list))

(defun find-epp (object &rest pos-spec-list)
  (let ((ee-buffer-name (or ee-buffer-name "*pp*")))
    (apply 'find-estring (pp-to-string object) pos-spec-list)))

(defun find-epp0 (object)
  (message (ee-pp0 object)))

(defun find-efunctionpp (symbol &rest pos-spec-list)
  (interactive (find-function-read))
  (let ((ee-buffer-name
	 (or ee-buffer-name (format "*function %S*" symbol))))
    (apply 'find-epp
	   (symbol-function symbol)
	   ;; or: `(fset ',symbol ',(symbol-function symbol))
	   pos-spec-list)))

(defun find-etpat (&rest rest)
  (interactive)
  (let* ((ee-buffer-name
	  (or ee-buffer-name "*(text-properties-at (point))*")))
    (apply 'find-epp (text-properties-at (point)) rest)))

(defun find-etpat0 ()
  (interactive)
  (find-epp0 (text-properties-at (point))))



;;;
;;; hyperlinks to the output of shell commands
;;;

(defun find-sh (command &rest pos-spec-list)
  (interactive "sShell command: ")
  (if (get-buffer command)		; if the buffer already exists
      (switch-to-buffer command)	; then just switch to it
    (switch-to-buffer command)		; otherwise create it
    (insert (shell-command-to-string command)) ; prepare its contents
    (goto-char (point-min)))		; and place point at its beginning
  (apply 'ee-goto-position pos-spec-list))

;; (defalias 'find-sh0 'shell-command-to-string)
(defun find-sh0 (command)
  (replace-regexp-in-string "\n$" "" (shell-command-to-string command)))



;;;
;;; hyperlinks to manpages
;;;

(defadvice Man-notify-when-ready (around find-man (man-buffer) activate)
  (if (not ee-find-man-flag)
      ad-do-it
    (switch-to-buffer man-buffer)
    (apply 'ee-goto-position ee-find-man-pos-spec-list)
    (setq ee-find-man-flag nil)))

(defun find-man (manpage &rest pos-spec-list)
  (interactive (list (ee-manpagename-ask)))
  (setq ee-find-man-flag t
	ee-find-man-pos-spec-list pos-spec-list)
    (man manpage))



;;;
;;; hyperlinks to files in html
;;;

(defun find-w3m (url &rest rest)
  (interactive "Murl: ")
  (let ((enable-local-variables nil)	; workaround for a w3m-el bug
	(w3m-async-exec nil))
    (w3m (replace-regexp-in-string "^/" "file:///" (ee-expand url))))
  (ee-goto-rest rest))



;;;
;;; hyperlinks to pages in dvi/ps/pdf documents
;;;

(defun find-dvipage (fname &optional n &rest ignore)
  (interactive "fDVI file: ")
  (let ((command (format "xdvi +%d %s &" (or n 1) fname)))
    (eev command nil)
    command))

(defun find-pspage (fname &optional n &rest ignore)
  (interactive "fPS or PDF file: ")
  (let ((command (format "gv -page %d %s &" (or n 1) fname)))
    (eev command nil)
    command))



;;;
;;; hyperlinks to anchors
;;;

(defun find-anchor (fname &optional tag &rest rest)
  (find-fline fname)
  (if tag (apply 'ee-goto-position (format ee-anchor-format tag) rest)))

(defun ee-to (anchor &rest rest)
  (interactive "sAnchor: ")
  (apply 'ee-goto-position (format ee-anchor-format anchor) rest))



;;;
;;; hyperlinks to information about Debian packages
;;;

(defun find-Package (fname &optional packagename &rest rest)
  (find-fline fname)
  (if packagename
      (apply 'ee-goto-position (format "\nPackage: %s\n" packagename) rest)))

(defun find-status (packagename &rest rest)
  (interactive (list (ee-debpkgname-ask)))
  (apply 'find-Package "/var/lib/dpkg/status" packagename rest))

(defun find-available (packagename &rest rest)
  (interactive (list (ee-debpkgname-ask)))
  (apply 'find-Package "/var/lib/dpkg/available" packagename rest))

(defun find-grep-status (grepargs &rest rest)
  (interactive "sgrep-status ")
  (apply 'find-sh (concat "grep-status " grepargs) rest))

(defun find-grep-available (grepargs &rest rest)
  (interactive "sgrep-available ")
  (apply 'find-sh (concat "grep-available " grepargs) rest))



;;;
;;; mass-producing hyperlink functions
;;;

(defun ee-eval-read-format (formatstr &rest rest)
  (let ((s (apply 'format formatstr     ; or (concat "(progn " formatstr ")")
		  rest)))
    (if ee-arg (insert "\n" s)		; for debugging
      (eval (read s)))))

(defun code-ps (code psfile)
  (ee-eval-read-format
   "(defun find-%spage (n &rest comments) (find-pspage %S n))"
   code psfile))

(defun code-dvi (code dvifile)
  (ee-eval-read-format
   "(defun find-%spage (n &rest comments) (find-dvipage %S n))"
   code dvifile))



(defun ee-find-tag (tag &rest pos-spec-list)
  (let ((tags-add-tables nil))
    (find-tag tag))
  (ee-goto-rest pos-spec-list))

;; This `ee-read-file-name' is semi-bogus and needs fixing.
;; A test: (ee-read-file-name nil data-directory)
;; Remember, its result will be concatenated to ee-xxxdir.
;; When it returns absolute filenames things break.
;;
(defun ee-read-file-name (prompt defaultdir)
  (let ((default-directory (or defaultdir default-directory)))
    (read-file-name (or prompt "Find file: " nil) nil ".")))

(defun ee-find-codenode (code infofile nodename &rest pos-spec-list)
  (if code (setq ee-info-code code
		 ee-info-file infofile))
  (apply 'find-node (format "(%s)%s" infofile nodename) pos-spec-list))

(defun ee-find-cd-sh (dir command &rest pos-spec-list)
  (find-sh (format "cd %s\n%s" dir command) pos-spec-list))
(defun ee-find-cd-sh0 (dir command)
  (find-sh0 (format "cd %s\n%s" dir command)))

(defun ee-aref (alist idx)
  "Like `aref', but for alists."
  (cdr (assoc idx alist)))
(defun ee-adel (alist idx)
  "Like `remq', but for alists. This is non-destructive, so wrap it in a setq."
  (remq (assoc idx alist) alist))
(defun ee-aset (alist idx newelt)
  "Like `aset', but for alists. This is non-destructive, so wrap it in a setq."
  (cons (cons idx newelt) (ee-adel alist idx)))

(defun code-c-d-register (c d)
  "Add the pair (C D) to `code-c-d-list'.
If `code-c-d-list' already has an entry with the same C delete
the previous one. The new entry is always added to the beginning
of the list."
  (let ((c-d (assoc c code-c-d-list)))
    (if c-d (setq code-c-d-list (delq c-d code-c-d-list))))
  (setq code-c-d-list (cons (list c d) code-c-d-list)))

(defun code-c-d (c d &optional infofile)
  (ee-eval-read-format "(setq ee-%sdir \"%s\")" c d)
  (ee-eval-read-format "(setq ee-%stagsfile \"%sTAGS\")" c d)
  (ee-eval-read-format "
    (defun ee-%sfile (str)
      (concat (ee-expand ee-%sdir) str))" c c)
  (ee-eval-read-format "
    (defun ee-use-%s-tags ()
      (setq tags-file-name ee-%stagsfile))" c c)
  (ee-eval-read-format "
    (defun find-%sfile (str &rest pos-spec-list)
      (interactive (list \"\"))
      (ee-use-%s-tags)
      (apply 'find-fline (ee-%sfile str) pos-spec-list))" c c c)
  (ee-eval-read-format "
    (defun find-%stag (str &rest pos-spec-list)
      (ee-use-%s-tags) (apply 'ee-find-tag str pos-spec-list))" c c)
  (ee-eval-read-format "
    (defun find-%ssh (command &rest pos-spec-list)
      (apply 'ee-find-cd-sh ee-%sdir command pos-spec-list))" c c)
  (ee-eval-read-format "
    (defun find-%ssh0 (command)
      (apply 'ee-find-cd-sh0 ee-%sdir command))" c c)
  (ee-eval-read-format "
    (defun find-%sw3m (furl &rest pos-spec-list)
      (apply 'find-w3m (ee-%sfile furl) pos-spec-list))" c c)
  (ee-eval-read-format "(setq ee-info-code %S)" c)
  (ee-eval-read-format "(setq ee-info-file %S)" c)
  (if infofile
      (ee-eval-read-format "
        (defun find-%snode (nodename &rest pos-spec-list)
          (apply 'ee-find-codenode %S %S nodename pos-spec-list))
        " c c infofile))
  (unless (equal d "")
    (code-c-d-register c (ee-expand d)))
  c)


(defun code-c-d-anchor (c d &optional infofile)
  (code-c-d c d infofile)
  (ee-eval-read-format "
    (defun find-%s (file &rest rest)
      (apply 'find-anchor (ee-%sfile file) rest))" c c))

(defun code-c-d-linux (c d)
  (code-c-d c d)
  (ee-eval-read-format "
    (defun find-%sconfvar (var &rest rest)
      (apply 'find-%sfile \"Documentation/Configure.help\"
	     (concat \"\\n\" var \"\\n\") rest))" c c))

;; Untested: the eegud functions have changed.
(defun code-c-d-gdb (c d &optional infofile)
  (code-c-d c d infofile)
  (ee-eval-read-format "
    (defun eeb-%sgdb-start (once &optional fname)
      (ee-use-%s-tags)
      (eeb-gdb-start (once ee-%sdir fname)))" c c c))


;;;
;;; mass-producing hyperlink functions: examples (debian-centric)
;;;

(setq ee-emacs-lisp-directory
  (or (file-name-directory (locate-library "loadup.el"))
      (format "/usr/share/emacs/%d.%d/lisp/"
	      emacs-major-version emacs-minor-version)))

(code-c-d "e"   ee-emacs-lisp-directory "emacs")
(code-c-d "el"  ee-emacs-lisp-directory "elisp")
(code-c-d "eli" ee-emacs-lisp-directory "emacs-lisp-intro")
(code-c-d "eetc" data-directory)

(code-c-d-anchor "eev"    "$EEVDIR/" "eev")     ; (find-eevfile "eev-dev.el")
(code-c-d-anchor "eevtmp" "$EEVTMPDIR/")        ; (find-eevtmpfile "")
(code-c-d-anchor "eevrc"  "$EEVRCDIR/")         ; (find-eevrcfile "")
(code-c-d-anchor "eevex"  "$EEVDIR/examples/")  ; (find-eevexfile "")

(code-c-d "ud" "/usr/share/doc/")	 ; (find-udfile "bash/")
(code-c-d "vldi" "/var/lib/dpkg/info/")	 ; (find-vldifile "bash.list")

(code-c-d "bash" (ee-udfile "bash/") "bashref")
(code-c-d "zsh"  (ee-udfile "zsh/")  "zsh")
(code-c-d "apthowto" "/usr/share/doc/Debian/apt-howto/")



;;;
;;; temporary highlighting (flashing)
;;;

(defun eeflash (start end &optional face duration)
  "Highlight the region between START and END using FACE, for time DURATION."
  (let ((ovl (make-overlay start end)))
    (overlay-put ovl 'face (or face 'region))
    (run-at-time (or duration 1) nil 'delete-overlay ovl)))

(defun eeflash+ (s &optional e spec add-to-e)
  "Highlight the region between S and E; face and duration are taken from SPEC.
This function only tries to do any work when S is a number and SPEC is non-nil.
When SPEC is non-nil it should be a pair of the form (FACE DURATION).
The argument ADD-TO-E is a hack for when we know that the region between S and
E+1 ends with a newline and it looks nicer to highlight the newline too; then
we set ADD-TO-E to 1."
  (if (and (numberp s) spec)
      (eeflash s (+ e (or add-to-e 0))
	       (car spec) (cadr spec)))
  (list s e spec add-to-e))



;;;
;;; evaluating sexps (alternatives to eval-last-sexp)
;;;

;; ee-eval-sexp-eol may be obsolete
;; ee-arg is still used in eev-insert.el (ack!)
;;

;; We used to have
;; M-e -> `ee-eval-sexp-eol', but now it's
;; M-e -> `eek-eval-sexp-eol' and `ee-eval-sexp-eol' is not bound to
;; any key.
;;
(defun ee-eval-sexp-eol (ee-arg)	; this ee-arg shadows the global one
  (interactive "P")
  (end-of-line)
  (eval-last-sexp nil))

;; See (find-efunction 'eval-last-sexp-1)
(defun eek-backward-sexp ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (forward-sexp -1)
    (when (eq (preceding-char) ?\\)
      (forward-char -1)
      (when (eq (preceding-char) ??)
	(forward-char -1))))
  (point))

(defun eek-forward-sexp ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (forward-sexp 1))
  (point))

(defun eek-last-sexp ()
  (save-excursion
    (buffer-substring-no-properties
     (eek-backward-sexp) (eek-forward-sexp))))

(defun eek-eval-last-sexp (&optional arg)
  (interactive "P")
  (cond ((eq arg 0)
	 (save-excursion
	   (eeflash+ (eek-backward-sexp) (eek-forward-sexp)
		     eek-highlight-spec)))
	((eq arg 1) (prin1 (eek-last-sexp)))
	((eq arg 2) (prin1 (read (eek-last-sexp))))
	((eq arg 3) (eval (read (eek-last-sexp))))
	((eq arg 4) (let ((sexp (read (eek-last-sexp)))) (debug) (eval sexp)))
	((eq arg 5) (let ((sexp (read (eek-last-sexp)))
			  (debug-on-error t))
		      (eval sexp)))
	((eq arg 8) (find-epp (eval (read (eek-last-sexp)))))
	((eq arg 9) (let ((interactive-clause (read (eek-last-sexp))))
		      (call-interactively
		       `(lambda (&rest args) ,interactive-clause
			  (message "%S" args)))))
	(t (prin1 (let ((ee-arg arg))
		    (eval (read (eek-last-sexp))))))))

(defun eek-eval-sexp-eol (&optional arg)
  (interactive "P")
  (end-of-line)
  (eek-eval-last-sexp arg))




;;;
;;; saving regions (as temporary sripts)
;;;

(defun ee-se-to-string (s e)
  (cond ((numberp s) (buffer-substring-no-properties s e))
        ((stringp s) s)))

(defun octal-to-num (str)
  (let ((lastv (- (string-to-char (substring str -1)) ?0))
	(rest (substring str 0 -1)))
    (if (string= "" rest) lastv (+ lastv (* 8 (octal-to-num rest))))))

(defun ee-write-string (str &optional altfile fmode)
  (let ((fname (substitute-in-file-name (or altfile ee-file))))
    (write-region str nil fname)	; a standard kludge
    (if fmode (set-file-modes fname (octal-to-num fmode)))))

(defun ee-write (s e pre post &optional altfile fmode)
  (ee-write-string (concat pre (ee-se-to-string s e) post)
		   altfile fmode))

(defun ee-se-to-string-with-nl (s e)
  (let ((str (ee-se-to-string s e)))
    (if (string-match "[^\n]\\'" str) (concat str "\n") str)))

(defun ee-write-with-nl (s e pre post &optional altfile fmode)
  (ee-write-string (concat pre (ee-se-to-string-with-nl s e) post)
		   altfile fmode))



(defun eev (s &optional e altfile)
  (interactive "r")
  (ee-write-with-nl s e "" "" altfile)
  (format "eev: wrote %s" (or altfile ee-file)))

(defun eevs (s &optional e suffix)
  (interactive "r\nsSuffix: ")
  (eev s e (concat ee-file suffix)))

(defun eelatex (s &optional e)
  (interactive "r")
  (ee-write s e "" "" ee-file-tex)
  (eev eelatex-eevscript nil)
  (format "eelatex: wrote %s and %s" ee-file-tex ee-file))

(defun eegdb (s &optional e)
  (interactive "r")
  (ee-write s e "" "" ee-file-gdb)
  (format "eegdb: wrote %s" ee-file-gdb))

(defun eeg (s &optional e)
  (interactive "r")
  (ee-write s e "" "" ee-file-generic)
  (format "eeg: wrote %s" ee-file-gdb))

(defun eeeval (s &optional e)
  (interactive "r")
  (eval (read (concat "(progn " (ee-se-to-string s e) "\n)"))))



(defun ee-default-directory ()
  (if (eq major-mode 'w3-mode)
      (let ((url (url-view-url 0)))
	(if (string-match "^file:\\(.*/\\)[^/]*$" url)
	    (match-string 1 url)
	  (error "Current url is %S, which is not a local file" url)))
    default-directory))

;; 2005jan10, incompatible change: added "dir"
(defun eecd (&optional dir command)
  (interactive)
  (eev (concat "cd " (or dir (ee-default-directory)) "\n"
	       (or command ""))))




;;;
;;; auxiliary functions for saving delimited ("bounded") regions
;;;

(defun ee-search-backward (str)
  (+ (save-excursion (search-backward str))
     (length str)))
(defun ee-search-forward (str &optional adjust)
  (+ (save-excursion (search-forward str))
     (- (length str))
     (or adjust 0)))

(defun ee-prefixp (prefix str)
  "Return t if STR begins with PREFIX."
  (and (<= (length prefix) (length str))
       (equal prefix (substring str 0 (length prefix)))))

(defun ee-sedelims+-to-se+ (sdelim &optional edelim flash-spec add-to-e)
  (list (ee-search-backward sdelim) (ee-search-forward edelim)
	flash-spec add-to-e))

(defun ee-sedelims++-to-sedelims+ (sdelim &optional edelim flash-spec add-to-e)
  (if (not edelim) (setq edelim sdelim))
  (if (symbolp sdelim) (setq sdelim (symbol-value sdelim)))
  (if (symbolp edelim) (setq edelim (symbol-value edelim)))
  (if (eq flash-spec t) (setq flash-spec 'eeflash-default))
  (if (symbolp flash-spec) (setq flash-spec (symbol-value flash-spec)))
  (if (eq add-to-e t) (setq add-to-e (if (ee-prefixp "\n" edelim) 1 0)))
  (list sdelim edelim flash-spec add-to-e))

;;;
;;; the default action on bounded regions
;;;

(defun eeb-default ()
  (interactive)
  (let* ((fun (car eeb-defaults))
	 (sedelims++ (cdr eeb-defaults))
	 (sedelims+ (apply 'ee-sedelims++-to-sedelims+ sedelims++))
	 (se+ (apply 'ee-sedelims+-to-se+ sedelims+)))
    (apply 'eeflash+ se+)
    (funcall fun (car se+) (cadr se+))))

;;;
;;; saving delimited regions
;;;

(defun eeb-define (eexxx-bounded
		   eexxx sdelim &optional   edelim flash-spec add-to-e)
  (let ((eexxx-sedelims+ (list eexxx sdelim edelim flash-spec add-to-e)))
    (set eexxx-bounded eexxx-sedelims+))
  (eval `(defun ,eexxx-bounded ()
	   (interactive)
	   (setq eeb-defaults ,eexxx-bounded)
	   (eeb-default))))

(defmacro eeb-once (&rest body) `(let (eeb-defaults) . ,body))
(defalias 'ee-once 'eeb-once)

(eeb-define 'eev-bounded     'eev     'ee-delimiter-hash      nil t t)
(eeb-define 'eeg-bounded     'eeg     'ee-delimiter-hash      nil t t)
(eeb-define 'eegdb-bounded   'eegdb   'ee-delimiter-hash      nil t t)
(eeb-define 'eelatex-bounded 'eelatex 'ee-delimiter-percent   nil t t)
(eeb-define 'eeb-eval        'eeeval  'ee-delimiter-semicolon nil t t)




;;;
;;; Hyperlinks to key sequences and series of Emacs actions
;;;

(defun eesteps (list)
  "Set the LIST of steps that `eesteps-do-step' will execute.\n
Here's an example: run\n
  (eesteps '(\"C-x b * scratch * RET   ;;; change to the buffer *scratch*\"
             \"foobar\"
             \"3*<left>\"
             (insert \"!\")))\n
then type \\[eesteps-do-step] four times.\n
Each step is either a string -- meaning a series of keys, in the
format used by `edmacro-mode' -- or a sexp to be evaluated."
  (setq eesteps-pos 0)
  (setq eesteps-list list)
  `(,(length list) steps stored - use <f12> to execute a step))

(defun eek (s &optional e count)
  (interactive "r")
  "Execute the region between S and E (or the string S) as a keyboard macro.
See `edmacro-mode' for the exact format.\n
An example: (eek \"C-x 4 C-h\")"
  (execute-kbd-macro (read-kbd-macro (ee-se-to-string s e)) count))

(defun eek0 (kbmacro &optional count)
  "This is similar to `eek', but uses the low-level formats for macros.
Example: (eek \"\\C-x4\\C-h\")"
  (execute-kbd-macro kbmacro count))

(defun eesteps-do-step ()
  (interactive)
  (if (>= eesteps-pos (length eesteps-list))
      (error "No more steps"))
  (let ((step (nth eesteps-pos eesteps-list)))
    (cond ((stringp step) (eek step))
	  (t (eval step))))
  (setq eesteps-pos (1+ eesteps-pos)))



;;;
;;; eev-newbie and eev-demos
;;;

(defun eekl (str &rest rest)
  (eek0 (concat str "\r"))
  (if rest (apply 'eekl rest)))

(defun eekv (str) (eek str) (message str))

;; A hack for showing the region (temporarily) in Emacs <= 21.1.3.
;; These functions are used by some demos. They may be removed in the future.
;; Note that in recent Emacsen C-SPC C-SPC <movement> highlights the region:
;; (find-efile "ChangeLog" "temporary transient-mark-mode")
;; (find-eetcfile "NEWS" "C-SPC C-SPC; this enables Transient Mark mode")
;;
(defun eekr  (str) (eek str)  (eeflash (point) (mark)))
(defun eekvr (str) (eekv str) (eeflash (point) (mark)))

(defun eev-newbie ()
  (interactive)
  (setq debug-on-error nil)
  (setq eval-expression-debug-on-error nil)
  (setq pop-up-windows nil)
  (message "Newbie settings activated.  Have you tried `M-x eev-demos'?"))

(defun eev-demos (arg)
  (interactive "P")
  (find-eevexfile "demos.e")
  (if (and arg (>= arg 1) (<= arg 5))
      (progn (ee-goto-position (format "\n;; End of demo %d\n" arg))
	     (forward-line -2)
	     (message "Type M-e to load the demo above the cursor."))
    (message (concat "Use `M-1 M-x eev-demos' to go to the 1st demo,\n"
		     "`M-2 M-x eev-demos' for the 2nd demo, etc (up to 4)."))))



;;;
;;; more tools
;;;

(defun ee-bol () (point-at-bol))
(defun ee-eol () (point-at-eol))
(defun ee-eval-string (str) (eval (read (concat "(progn\n" str "\n)"))))

;; (ee-flatten '((1 2 3) (4 5) (((6)) 7) nil nil 8 9))
;; (ee-flatten '(1 2 3) '(4 5) '(((6)) 7) nil nil 8 9)
;;
(defun ee-flatten (obj &rest rest)
  (cond (rest (append (ee-flatten obj) (ee-flatten rest)))
	((null obj) nil)
	((listp obj) (append (ee-flatten (car obj)) (ee-flatten (cdr obj))))
	(t (list obj))))

(defun ee-read-file (fname)
  (with-temp-buffer
    (insert-file-contents fname)
    (buffer-string)))

(defun ee-no-trailing-nl (str)
  (replace-regexp-in-string "\n$" "" str))




;;;
;;; starting background processes
;;;

;; (eebg-xdvi "/usr/share/doc/gdb/refcard.dvi.gz")
;; (eebg-gv "/usr/share/doc/gv/gv.ps.gz" 3 '("-scale" "-2"))
;; (eebg-channel-xterm "A" "bash" '("-gravity" "nw"))

(defun eebg-xdvi (fname &optional page xdviargs)
  (interactive "fdvi file: ")
  (apply 'start-process "xdvi" "*Messages*"
	 (ee-flatten
	  "xdvi" (if page (format "+%d" page)) xdviargs
	  (ee-expand fname))))

(defun eebg-gv (fname &optional page gvargs)
  (interactive "fPS or PDF file: ")
  (apply 'start-process "gv" "*Messages*"
	 (ee-flatten
	  "gv" (if page (list "-page" (format "%d" page))) gvargs
	  (ee-expand fname))))

(defun eebg-channel-xterm (channel &optional prog-and-args xterm-args)
  "This is the low-level way of creating an xterm listening on channel CHANNEL.
See `eechannel-xterm'."
  (interactive "sChannel: ")
  (apply 'start-process (format "xterm (channel %s)" channel) "*Messages*"
	 (ee-flatten
	  "xterm" "-T" (concat "channel " channel) xterm-args "-e"
	  (ee-expand "$EEVDIR/eegchannel") channel
	  (or prog-and-args (ee-expand "$SHELL")))))



;;;
;;; sending strings to external programs through "channels"
;;;

(defun eechannel-pidfile (channel)
  (ee-expand (format "$EEVTMPDIR/eeg.%s.pid" channel)))
(defun eechannel-strfile (channel)
  (ee-expand (format "$EEVTMPDIR/eeg.%s.str" channel)))

(defun eechannel-send (channel str)
  (if (not channel) (setq channel eechannel-default))
  (ee-write str nil "" "" (eechannel-strfile channel))
  (find-sh0 (format "kill -USR1 $(cat %s)" (eechannel-pidfile channel))))

(defun eechannel (channel &optional str)
  (interactive "sDefault channel: ")
  (if (not str)
      (setq eechannel-default channel)
    (eechannel-send channel str)))

(defun eechannel-do-this-line () (interactive)
  (let ((line (buffer-substring (ee-bol) (ee-eol)))) ; contents of this line
    (if (string-match "^\\(.*\\)" line)             ; lines with a red star
	(ee-eval-string (match-string 1 line))       ; are eval'ed
      (eechannel-send nil (concat line "\n")))       ; other lines are sent
    (next-line 1)))			             ; go down

(defun eech (s &optional e)		; bad name?
  (interactive "r")
  (eechannel-send eechannel-default (ee-se-to-string s e)))

(eeb-define 'eech-bounded 'eech 'ee-delimiter-hash nil t t)



;;;
;;; sending strings through "channels": high-level functions
;;;

(defun ee-pid-running-p (pid)
  "Return t if a process with pid PID is running. This is linux-specific."
  (file-exists-p (format "/proc/%s" pid)))

(defun eechannel-pid (channel)
  "Note: this function returns either a pid (as a string) or nil."
  (let ((pidfile (eechannel-pidfile channel)))
    (if (file-exists-p pidfile) (ee-no-trailing-nl (ee-read-file pidfile)))))

(defun eechannel-running-p (channel)
  "Returns t if there is a process listening on CHANNEL."
  (let ((pid (eechannel-pid channel)))
    (if pid (ee-pid-running-p pid))))

(defun eechannel-xterm (channel &optional prog-and-args xterm-args)
  "If there's no process listening on CHANNEL then create an xterm there.
This function always sets the default channel to CHANNEL.
PROG-AND-ARGS and XTERM-ARGS are lists of strings: see `eebg-channel-xterm'."
  (interactive "sChannel: ")
  (eechannel channel)
  (if (eechannel-running-p channel)
      (message "Reusing channel %s" channel)
    (eebg-channel-xterm channel prog-and-args xterm-args)))

(defun eechannel-kill (channel)
  "Kill the process associated to channel CHANNEL."
  (find-sh0 (format "kill -9 $(cat %s)" (eechannel-pidfile channel))))






;;;
;;: eev keys mode
;;;

(defvar eev-mode-map nil)
(if eev-mode-map
    ()
(setq eev-mode-map (make-sparse-keymap))
(define-key eev-mode-map "\M-E" 'eek-eval-last-sexp)    ; extends     C-x C-e
(define-key eev-mode-map "\M-e" 'eek-eval-sexp-eol)     ; extends C-e C-x C-e
(define-key eev-mode-map "\M-k" 'kill-this-buffer)      ; convenience
(define-key eev-mode-map "\M-K" 'bury-buffer)           ; convenience
(define-key eev-mode-map [f3]   'eeb-default)
(define-key eev-mode-map [f9]   'eechannel-do-this-line)
(define-key eev-mode-map [f12]  'eesteps-do-step)
(define-key eev-mode-map "\M-G" 'eegud-show-gud-buffer)	; hack, in eev-insert.el

(define-key eev-mode-map "\M-h\M-d" 'find-debpkg-links)    ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-f" 'find-efunction-links) ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-i" 'find-einfo-links)     ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-k" 'find-ekey-links)      ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-m" 'find-manpage-links)   ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-v" 'find-evariable-links) ; in eev-insert.el
(define-key eev-mode-map "\M-hf"    'find-file-links)      ; in eev-insert.el
(define-key eev-mode-map "\M-hm"    'find-last-manpage-links) ;in eev-insert.el
(define-key eev-mode-map "\M-h\M-y" 'eemklinks-yank-pos-spec) ;in eev-insert.el
(define-key eev-mode-map "\M-h2"    'eemklinks-duplicate-this-line) ; idem
(define-key eev-mode-map "\M-h\M-2" 'eemklinks-duplicate-this-line) ; idem
(define-key eev-mode-map "\M-I"     'ee-ill)               ; in eev-insert.el

(define-key eev-mode-map "\M-h\M-c" 'describe-char)
(define-key eev-mode-map "\M-h\M-t" 'find-etpat)
(define-key eev-mode-map "\M-ht"    'find-etpat0)
(define-key eev-mode-map "\M-h\M-s" 'find-efacedescr)
)

(define-minor-mode eev-mode
  "Makes the keymap `eev-mode-map' active."
  :init-value t :global t)




;;;
;;: invading the global namespace
;;;

(defun ee-invade-global-keymap ())
(defun ee-invade-global-namespace ()
  (interactive)
  (defalias 'to   'ee-to)
  ;; (defalias 'back 'ee-back)		; oops - I removed `back'
  (defalias 'inn  'ee-inn)
  (defalias 'inns 'ee-inns)
  (defalias 'dff  'ee-dff)
  (defalias 'dfa  'ee-dfa)
  (defalias 'ill  'ee-ill))

(defun ee-invade-global-menu-bar ())	; I removed the menu code


;;;
;;; aliases for compatibility with previous versions
;;;

(defalias 'ee-substitute-in-file-name 'ee-expand)
(defalias 'find-availablegrep 'find-grep-available)
(defalias 'find-progoutput 'find-sh)

(defalias 'highlight-temporarily         'eeflash)
(defalias 'highlight-temporarily-by-spec 'eeflash+)



;;; The functions below are obsolete and semi-broken, don't use them!
;;; The functions that called them now call `find-wottb-call'.
;;
;; (defmacro ee-same-window (samewindowbuffername &rest body)
;;   `(let ((ee-old-pop-up-windows pop-up-windows))
;;      (setq pop-up-windows nil)          ; "let pop-up-windows" doesn't work
;;      (prog1 (progn . ,body)
;;        (setq pop-up-windows ee-old-pop-up-windows))))
;;
;; (defmacro ee-same-window-contagious (samewindowbuffername &rest body)
;;   "Obsolete, don't use. Replaced by `find-wottb-call'."
;;   `(let ((pop-up-windows ee-pop-up-windows)) . ,body)) ; hack, not working

;; `ee-ill' needs another name.
;; (find-efunction 'ee-ill)
;; (global-set-key "\M-I" 'ee-ill)

;; (find-elnode "Active Keymaps")
;; overriding-local-map
;; (current-local-map)
 


;; Local Variables:
;; mode:              outline-minor
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-anchor-format:  "defun %s "
;; ee-comment-prefix: ";;"
;; outline-regexp:    ";+:[;:]*"
;; no-byte-compile:   t
;; End:
