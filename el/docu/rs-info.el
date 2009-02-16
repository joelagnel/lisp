;;; Saved through ges-version 0.3.3dev at 2003-12-06 12:59
;;; ;;; ;;; From: Reiner Steib <4.uce.03.r.s@nurfuerspam.de>
;;; ;;; ;;; Subject: rs-info.el -- Some info related functions
;;; ;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; ;;; Followup-To: gnu.emacs.help
;;; ;;; ;;; Cc: jeff.rancier@softechnics.com (Jeffery B. Rancier)
;;; ;;; ;;; Date: Mon, 17 Nov 2003 21:58:40 +0100
;;; ;;; ;;; Organization: Dept. of Theoretical Physics, University of Ulm
;;; ;;; ;;; Reply-To: reiner.steib@gmx.de
;;; ;;; ;;; Mail-Copies-To: nobody

;;; ;;; [1. text/plain]

;;; ;;; Hi,

;;; ;;; `rs-info.el' provides some info related functions:

;;; ;;; - Insert proper[1] references to info pages in various formats using
;;; ;;;   `rs-info-insert-current-node':

;;; ;;;   · `emacs' (Emacs style): (info "(file)node")
;;; ;;;   · `emacs-press': as `emacs' plus a note about `C-x C-e'
;;; ;;;   · `defcustom': A ":link" for `defcustom'
;;; ;;;   · `gnus' (the Gnus home brewed style): <info://file/node> (deprecated)
;;; ;;;   · `gnome' (GNOME style): <info:foo#bar_baz>
;;; ;;;   · `kde' (KDE style): <info:(foo)bar baz>
;;; ;;;   · `konquerer' (Konquerer style): <info:/foo/bar baz>

;;; ;;;   [1] If inserted in this way, the references are buttonized in Gnus
;;; ;;;   5.10, see (info "(gnus)Article Buttons") and (info "(gnus)Article
;;; ;;;   Button Levels").

;;; ;;; - `rs-boxquote-info': Addition to `boxquote.el' for boxes containing
;;; ;;;   citations from an info node.  Example:

;;; ;;; ,----[ (info "(emacs)Keymaps") ]
;;; ;;; |    As a user, you can redefine any key; but it is usually best to
;;; ;;; | stick to key sequences that consist of `C-c' followed by a letter.
;;; ;;; `----

;;; ;;; - `rs-info-reload': Reload current info node retaining the position.

;;; ;;; the current version is always available on
;;; ;;; <URL:http://theotp1.physik.uni-ulm.de/~ste/comp/emacs/misc/rs-info.el>.

;;; ;;; Bye, Reiner.

;;; ;;; --8<---------------cut here---------------start------------->8---
;;; [2. application/emacs-lisp; rs-info.el]

;; rs-info.el -- Some info related functions
;; $Id: rs-info.el,v 1.6 2003/11/17 20:53:04 ste Exp $

;; Author: Reiner Steib <Reiner.Steib@gmx.de>
;; Keywords: info
;; X-URL: http://theotp1.physik.uni-ulm.de/~ste/comp/emacs/misc/rs-info.el

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; Some functions related to info mode in Emacs.

;;; Installation:

;; Put `rs-info.el' in a directory that's in your `load-path',
;; e.g. ~/lisp:
;;
;; (add-to-list 'load-path "~/lisp")
;;
;; Put the following autoloads your your init file:
;;
;; (autoload 'rs-info-insert-current-node "rs-info"
;;   "Insert reference to current Info node using STYPE in buffer." t nil)
;; (autoload 'rs-boxquote-info "rs-info"
;;   "Yank text (from an info node), box it and use current info node as title."
;;   t nil)
;; (autoload 'rs-info-reload "rs-info" "Reload current info node." t nil)

;;; Known problems:

;; XEmacs 21.4.12 inf-loops at (replace-in-string "xe" "\\(x\\)?" "").

;;; History:

;; revision 1.5: Note on XEmacs.

;;; Code:

(autoload 'boxquote-yank "boxquote")

(defun rs-replace-in-string (string regexp newtext)
  "In STRING, replace all matches for REGEXP with NEWTEXT.
Hack to get a common function for all Emacsen.  Note that Oort Gnus has
`gnus-replace-in-string', but we don't want to load Gnus."
  (cond
   ;; Gnus is loaded.
   ((fboundp 'gnus-replace-in-string)
    (gnus-replace-in-string string regexp newtext))
   ;; XEmacs
   ((fboundp 'replace-in-string)
    (replace-in-string string regexp newtext))
   ;; Emacs
   ((fboundp 'replace-regexp-in-string)
    (replace-regexp-in-string regexp newtext string))
   ;; Emacs < 21
   (t
    (require 'dired)
    (dired-replace-in-string regexp newtext string))))

;;;###autoload
(defvar rs-info-goto-node-string "info" ;; "Info-goto-node"
  ;; Stefan Monnier suggested to use `info' instead of `Info-goto-node', but
  ;; this doesn't work for XEmacs.  OTOH, I've submitted a patch for XEmacs,
  ;; so "info" should be okay (included in XEmacs >= 21.5.12).
  "String to insert with `rs-info-insert-current-node'.")

;;;###autoload
(defvar rs-info-prefered-style 'emacs
  "Prefered style for info URLs.")

;;;###autoload
(defun rs-info-insert-current-node (style &optional noinsert)
  "Insert reference to current Info node using STYLE in buffer.

If NOINSERT (the prefix), return the string instead.

Possible styles (must be a symbol):
- `emacs' (Emacs style): \(info \"\(file\)node\"\)
- `emacs-press': as `emacs' plus a note about `C-x C-e'
- `defcustom': A \":link\" for `defcustom'
- `gnus' (the Gnus home brewed style): <info://file/node> (deprecated)
- `gnome' (GNOME style): <info:foo#bar_baz>
- `kde' (KDE style): <info:(foo)bar baz>
- `konquerer' (Konquerer style): <info:/foo/bar baz>

When used interactivly, the default is taken from the varibale
`rs-info-prefered-style'. In `emacs-lisp-mode' the default is `defcustom'.

For `emacs' and `emacs-press' style see the variable
`rs-info-goto-node-string'."
  ;; `rs-replace-in-string' may be replaced by `gnus-replace-in-string' or
  ;; `replace-regexp-in-string' (Emacs) or `replace-in-string' (XEmacs).
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read
	    "Info URL style: "
	    (mapcar (lambda (method) (list (format "%s" method)))
		    '("emacs"  "emacs-press" "defcustom" "gnus"
		      "gnome" "kde" "konquerer"))
	    nil t;; PREDICATE REQUIRE-MATCH
	    (if (eq major-mode 'emacs-lisp-mode)
		"defcustom"
	      (symbol-name rs-info-prefered-style))))
	 current-prefix-arg))
  (when (eq "" style)
    (setq style rs-info-prefered-style))
  ;; Because of the above use of `completing-read', we also accept strings as
  ;; STYLE (undocumented feature ;-))
  (unless (symbolp style)
    (setq style (intern style)))
  (let ((buffer (buffer-name))
	(ret ""))
    ;; The next lines were orginally borrowed from Karl Pflaesterer's code in
    ;; <m3eli7tgp0.fsf@hamster.pflaesterer.de>.
    (set-buffer "*info*")
    (let* ((node Info-current-node)
	   (node+ (rs-replace-in-string node " " "+"))
	   (node_ (rs-replace-in-string node " " "_"))
	   (fileurl (file-name-nondirectory Info-current-file))
	   ;; XEmacs doesn't strip extensions in `Info-current-file':
	   (fileurl (rs-replace-in-string
		     fileurl "\\(\\.info\\)?\\(\\.gz\\)?$" "")))
      (setq ret
	    (cond
	     ((eq style 'defcustom)
	      (concat ":link '(custom-manual \"(" fileurl ")" node "\")"))
	     ((eq style 'gnome)
	      (concat "<info:" fileurl "#" node_ ">"))
	     ((eq style 'kde)
	      (concat "<info:(" fileurl ")" node ">"))
	     ((eq style 'konquerer)
	      (concat "<info:/" fileurl "/" node ">"))
	     ((eq style 'gnus)
	      (concat "<info://" fileurl "/" node+ ">"))
	     ((or (eq style 'emacs) (eq style 'emacs-press))
	      (concat "(" rs-info-goto-node-string
		      " \"(" fileurl ")" node "\")"
		      (if (eq style 'emacs-press)
			  "; <== Press C-x C-e here!"
			"")))
	     (t
	      (error "Style `%s' is not valid" style))))
      (if noinsert
	  ret
	(set-buffer buffer)
	(if (not (eq style 'defcustom))
	    (insert ret)
	  (beginning-of-line)
	  (insert ret)
	  (lisp-indent-line)
	  (newline))))))

;;;###autoload
(defun rs-boxquote-info ()
  "Yank text (from an info node), box it and use current info node as title."
  (interactive)
  (boxquote-yank)
  (boxquote-title
   (save-excursion (rs-info-insert-current-node 'emacs 'string))))

;;;###autoload
(defun rs-info-reload ()
  "Reload current info node."
  (interactive)
  (let ((file Info-current-file)
	(node Info-current-node)
	(point (point)))
    (kill-buffer "*info*")
    (Info-find-node file node)
    (goto-char point)))

;;; provide ourself

(provide 'rs-info)

;;; rs-info.el ends here
;;; [3. text/plain]

;;; --8<---------------cut here---------------end--------------->8---

;;; Followup-To: gnu.emacs.help
;;; ;;; -- 
;;; ;;;        ,,,
;;; ;;;       (o o)
;;; ;;; ---ooO-(_)-Ooo--- PGP key available via WWW   http://rsteib.home.pages.de/

