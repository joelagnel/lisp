;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-mouse.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-mouse.el,v 1.3 2003/04/11 22:02:05 rgrjr Exp $

;;; Unlike most other ilisp source files, ilisp-mouse is meant to be loadable by
;;; itself, in case you want to click M-left on a C definition name or URL, for
;;; which you don't need any of the Lisp support machinery.  It also works with
;;; Franz Inc's "eli" interface to ACL via emacs.
;;;
;;; To use this independently of ilisp, ensure that this directory is on your
;;; search path, and put the following in your .emacs file:
;;;
;;;	(require 'ilisp-mouse)
;;;	;; Need to nuke the 'down' event binding, or the 'up' gets swallowed.
;;;	(global-set-key [M-down-mouse-1] nil)
;;;	(global-set-key [M-mouse-1] 'ilisp-mouse-edit-thing)
;;;
;;; Meta-Left (aka Alt-Left aka M-mouse-1) is the traditional binding, but be
;;; aware that many window managers intercept this gesture to iconify the
;;; window, or some other stupid thing.  If you find this is the case, you can
;;; either tell the WM not to do that, or pick another mouse gesture.  (You
;;; should also be aware that Emacs by default uses M-mouse-1 to manipulate the
;;; secondary selection.)
;;;
;;; When this file is loaded without ilisp (or eli), of course, it can't use the
;;; Lisp to find Common Lisp definitions.  However, starting a Lisp session via
;;; ilisp will automagically restore this functionality.
;;;

;; tags package fns are used by ilisp-edit-thing and ilisp-thing-around-point.
;; [I'd like to condition (require 'tags) on whether we actually need it, but
;; the funcall in ilisp-edit-thing is problematic.  -- rgr, 12-Apr-94.]
(cond ((string-match "^Lucid" (emacs-version))
	;; In lemacs, the "tags" feature is provided by the "etags" file.
	(or (memq 'tags features)
	    (load "etags")))
      ;; End [arguable] of braindeath.  -- rgr, 26-Oct-94.
      ((string-match emacs-version "^18\\.") (require 'tags))
      (t (require 'etags)))

;;;; Variables and macros.

(defvar ilisp-mouse-use-ange-ftp-p t
  "*Whether to use ange-ftp to open 'FILE:' and 'FTP:' URLs in emacs
when you mouse on them; the default is 'yes'.")

(defvar ilisp-url-regexp
	"^<url:\\|^https?:\\|^gopher:\\|^telnet:\\|^wais:\\|^s?news:\\|^mailto:"
  "*Matches URL's to be passed on to the browse-url machinery.
Set this to nil to prevent \\[ilisp-mouse-edit-thing] from attempting to
interpret URLs.  Note that this doesn't match ftp: or file: URL's, so
they if you let this match ftp: or file: URL's, then you'll wind up
looking at them with Netscape, or whatever.")

(defmacro with-lisp-syntax (&rest body)
  "Helper for functions (mostly mouse commands) that want Lisp syntax in
arbitrary buffers."
  ;; But keep TeX-mode syntax, so that clicking c-Middle on "{" gets the whole
  ;; environment.  -- rgr, 27-Sep-94.
  (let ((tmst (if (string-match emacs-version "^18\\.")
		  'TeX-mode-syntax-table
		  'tex-mode-syntax-table)))
    (` (let ((old-syntax-table (syntax-table)))
	 (unwind-protect
	      (progn
		(if (not (and (boundp (quote (, tmst)))
			      (eq old-syntax-table
				  (symbol-value (quote (, tmst))))))
		    ;; (not (memq major-mode '(TeX-mode LaTeX-mode)))
		    (set-syntax-table
		     (or lisp-mode-syntax-table
			 ;; lmst not defined until lisp-mode executed
			 emacs-lisp-mode-syntax-table)))
		(,@ body))
	   (set-syntax-table old-syntax-table))))))

;;;; Code.

(defun ilisp-mouse-snarf-sexp-after-point (&optional end)
  ;; Common idiom.
  (while (looking-at "\\s'")
    (forward-char 1))
  (buffer-substring-no-properties (point)
				  (or end
				      (progn
					(forward-sexp 1)
					(point)))))

(defun ilisp-thing-around-point ()
  "Finds an interesting editable thing around point.  This includes file
names, URLs, emacs lisp and Common Lisp definition names, and
identifiers in other languages that may be findable via\\[find-tag].
Recognizes ange-ftp and Lispm pathname syntax and expands pathnames so
that relativity works, but leaves URLs alone.  Uses find-tag-default
and/or ffap-file-at-point but hacks the syntax table, since the default
text table doesn't like \".\", \"~\", and other constituent chars."
  (let* ((ffap-guess nil)
	 (thing (with-lisp-syntax
		  ;; Special cases for clicking on an S-expression, which could
		  ;; be Common Lisp definition names.
		  (cond ((eq (char-after) ?\()
			  (save-excursion
			    (ilisp-mouse-snarf-sexp-after-point)))
			((eq (char-after) ?\))
			  (save-excursion
			    (forward-char 1)
			    (let ((end (point)))
			      (forward-sexp -1)
			      (ilisp-mouse-snarf-sexp-after-point end))))
			((and (fboundp 'ffap-guesser)
			      ;; this will return an existing file name or URL,
			      ;; or nil.  remember what we got, so we don't
			      ;; second-guess the guesser.
			      (setq ffap-guess (ffap-guesser))))
			(t (find-tag-default))))))
    (cond ((or (not (stringp thing))
	       (eq (aref thing 0) ?\())
	    ;; Thing is sometimes null; this happens (e.g.) in empty buffers.
	    thing)
	  (ffap-guess)
	  ((and (or (file-name-absolute-p thing)
		    (= (aref thing 0) ?.))
		(file-exists-p thing))
	    ;; The old test just looked at the first character; this is lots
	    ;; more expensive, but much more versatile.  It's equivalent to the
	    ;; much cleverer find-file-at-point algorithm.  We still check the
	    ;; first char anyway, because file-exists-p can be too expensive for
	    ;; ange-ftp pathnames.  And we must expand the file name here, in
	    ;; order to handle relative pathnames correctly.  -- rgr, 12-Sep-96.
	    (expand-file-name thing))
	  ((and ilisp-url-regexp
		(string-match ilisp-url-regexp thing))
	    ;; Probably a URL; don't try to mung it.  [better still, go back and
	    ;; look again, since lisp syntax drops the #tag syntax.  -- rgr,
	    ;; 12-Sep-96.]  [and *doesn't* do <url:thing> right, since this is
	    ;; allowed to contain whitespace, which is supposed to be
	    ;; eliminated.  -- rgr, 3-Jan-00.]
	    (require 'browse-url)
	    (let ((better-thing (browse-url-url-at-point)))
	      (if (or (> (length better-thing) (length thing))
		      (string-match "^<url" thing))
		  better-thing
		  thing)))
	  ((and ilisp-mouse-use-ange-ftp-p
		(string-match "\\`\\(ftp\\|file\\)://\\([^:/]+\\):?\\(/.*\\)"
			      thing))
	    ;; Convert URL-style ftp: or file: references to ange-ftp syntax.
	    ;; Taken from ffap-fixup-url and ffap-host-to-path fns.  -- rgr,
	    ;; 22-Mar-95.
	    (require 'ange-ftp)
	    (let ((host (match-string 2 thing))
		  (rest (match-string 3 thing)))
	      (cond ((equal host "localhost") rest)
		    ((string-match "@" host)
		      (concat "/" host ":" rest))
		    (t
		      (concat "/" (or ange-ftp-default-user "anonymous")
			      "@" host ":" rest)))))
	  ((string-match "^\\([-a-zA-Z0-9._]+:\\)[~/.]" thing)
	    ;; Looks like Lispm host:pathname syntax; the "~", "/", or "." means
	    ;; it's probably not a package symbol.  We rely on the fact that
	    ;; Lispm syntax is a prefix of ange-ftp /user@host:pathname syntax.
	    ;; [but downcase the host name because they are case-sensitive to
	    ;; ange-ftp.  -- rgr, 1-Feb-95.]
	    (expand-file-name
	      (concat "/" (user-login-name) "@"
		      (downcase (substring thing 0 (match-end 1)))
		      (substring thing (match-end 1)))))
	  (t
	    thing))))

(defvar lisp-definition-finders
	'((edit-definitions-lisp ilisp-buffer)
	  (fi:lisp-find-definition fi::lep-open-connection-p))
  "This is a search list of (definition-finder tester).  We pick the
first entry for which the definition-finder symbol is defined in emacs
\(autoloading counts\), and either has a null tester function, or the
tester function returns non-nil.")

(defun ilisp-edit-function-spec (thing &optional prefer-lisp-p)
  ;; Given a string that is possibly a Lisp definition name, decide whether to
  ;; use find-tag or something more Lisp-competent such as edit-definitions-lisp
  ;; or fi:lisp-find-definition to find it.  (Really we should try to find
  ;; whatever the current lisp-mode uses.  But ilisp and LEP are too different
  ;; to make this clean & general.)  [haven't figured out all of the package
  ;; issues for finding things in non-Lisp buffers.  -- rgr, 19-May-00.]
  (let ((edit-fn (key-binding "\M-.")))
    (if (or (null edit-fn)		;; shoudn't happen.
	    (and (eq edit-fn 'find-tag)
		 (or prefer-lisp-p
		     ;; could be "(method icc:draw-part (icc::scratch-dot t))",
		     ;; or something like that, which fi:lisp-find-definition
		     ;; and edit-definitions-lisp know how to deal with.
		     (and (string-match "[():]" thing)
			  ;; eliminate "fi:random-elisp-fn" case.
			  (let ((symbol (intern-soft thing)))
			    (not (and symbol
				      (or (boundp symbol)
					  (fboundp symbol)))))))))
	;; search for an inferior lisp definition finder if given a symbol that
	;; is obviously Common Lisp, regardless of what the mode M-. might be.
	(let ((tail lisp-definition-finders))
	  (while tail
	    (let* ((entry (car tail))
		   (fn (if (consp entry) (car entry) entry))
		   (tester (if (consp entry) (car (cdr entry)) nil)))
	      (if (and (fboundp fn)
		       (or (null tester)
			   (condition-case ignore (funcall tester)
			     (error nil))))
		  (setq edit-fn fn
			tail nil)
		  (setq tail (cdr tail)))))))
    ;; (message "Using %S to edit %S." edit-fn thing)
    ;; assume find-tag compatibility.
    (funcall edit-fn thing)))

;;;###autoload
(defun ilisp-edit-thing (thing)
  "Like the Symbolics ed function: figures out how to edit thing generically."
  ;; The rules are different for GNU emacs, though, since emacs doesn't handle
  ;; #P, etc.
  '(message "Got %S" thing)
  (cond ((null thing) nil)
	((symbolp thing)
	  ;; emacs function/variable
	  (find-tag (symbol-name thing)))
	((or (not (stringp thing)) (equal thing ""))
	  (error "Don't know how to edit %S." thing))
	((or (file-name-absolute-p thing)
	     ;; Weak pathname heuristic.
	     (= (aref thing 0) ?.))
	  ;; Like find-file, but invokes dired if thing has wildcards.  Note
	  ;; that find-file is smart enough to enter dired if given a directory.
	  (if (string-match "^$\\|[[*?]" (file-name-nondirectory thing))
	      (dired thing)
	      (find-file thing)))
	((and ilisp-url-regexp
	      (string-match ilisp-url-regexp thing))
	  (require 'browse-url)
	  (funcall browse-url-browser-function thing))
	(t
	  (ilisp-edit-function-spec thing))))

;;; Mouse events.

;; [Oops; this is specific to fsf, v19 and later . . .  -- rgr, 8-Apr-03.]

;;;###autoload
(defun ilisp-mouse-edit-thing (event)
  "Find the source files for the thing under the mouse.
If it looks like a pathname, then do find-file or dired on it.
If it looks like a definition name, then do M-.
Whatever it is, it is found in the current window, regardless of where
you click.  'Looks like a pathname' means it starts with '.', '/', or
'~', or (as in a Lispm pathname) has a host: prefix followed by one of
these three characters.  Lispm pathnames are converted to ange-ftp
pathnames, which generally works, though only for Unix syntax."
  (interactive "e")
  (ilisp-edit-thing (save-excursion
		      (save-window-excursion
			(mouse-set-point event)
			(ilisp-thing-around-point)))))

(provide 'ilisp-mouse)

;; some test data:
;;    http://rgrjr.dyndns.org/linux/howto.html
;;    http://bmerc-www.bu.edu/
;;    mailto:rogers@rgrjr.dyndns.org
;;    ftp://huxley.bu.edu/~rogers/queue
;;    ./ilisp-mouse.el
;;    /etc/passwd
;;    file:///etc/passwd [but ffap doesn't rewrite this as a local pathname] 
;;    file://localhost/etc/passwd

;;; end of file -- ilisp-mouse.el --
