;;; Saved through ges-version 0.3.3dev at 2004-05-14 12:22
;;; ;;; From: Michael Schierl <schierlm-usenet@gmx.de>
;;; ;;; Subject: gnus-cvslog 0.1 -- Fancy highlighting of CVS log mailings in Gnus
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Followup-To: gnu.emacs.gnus
;;; ;;; Date: Sat, 03 Apr 2004 19:39:26 +0200
;;; ;;; Reply-To: schierlm@gmx.de

;;; [1. text/plain]

;;; (Automatically generated with ges-post.el, version $Revision: 0.6 $)

;;; [2. application/emacs-lisp; gnus-cvslog.el]

;;; gnus-cvslog.el --- Fancy highlighting of CVS log mailings in Gnus

;; Copyright (C) 2004 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Created: 27 March 2004
;; Keywords: Gnus, cvslog, syncmail
;; Version: 0.1

(defconst gnus-cvslog-version "0.1"
  "Version of Gnus CVSLOG.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Gnus' message highlighting is quite useful for "human generated"
;; messages, but can look quite ugly for computer generated
;; messages. An example for this are CVS log messages which are
;; usually sent to a CVS mailing list (which can - of course - be read
;; by Gnus). These messages contain a log message and zero or more
;; diffs showing what files have changed how. In these diffs, it can
;; be quite annoying to have highlighting of e.g. _emphasized_
;; words. Plus you cannot really see what has changed -- diff-mode can
;; do the highlighting much better.

;; To use gnus-cvslog, first (auto)load this file (e.g. by putting

;; (require 'gnus-cvslog)

;; into your gnus.el).  Then you can either enable highlighting for
;; all messages by

;; (add-hook 'gnus-article-prepare-hook 'gnus-cvslog-highlight)

;; or only for some groups by

;; (add-hook 'gnus-article-prepare-hook 'gnus-cvslog-highlight-maybe)

;; in the latter case, you have to use group parameters or similar
;; approaches to set `gnus-cvslog-enabled' depending on the current
;; group. However, this is not really necessary, since only messages
;; starting with `gnus-cvslog-start-line' will be treated at all.

;;; History:

;; 0.1   initial version

;;; Code:

(defvar gnus-cvslog-start-line "Update of \\(/.*\\)")
(defvar gnus-cvslog-diff-header "Index: \\(.*\\)\n======+$")
(defvar gnus-cvslog-added-header "--- NEW FILE: \\(.*\\) ---$")
(defvar gnus-cvslog-deleted-header "--- \\(.*\\) DELETED ---$")

(defvar gnus-cvslog-header-face 'bold)
(defvar gnus-cvslog-filename-face 'font-lock-function-name-face)
(defvar gnus-cvslog-log-face 'font-lock-comment-face)

(defvar gnus-cvslog-enabled nil)
(add-to-list 'gnus-newsgroup-variables 'gnus-cvslog-enabled)

;;;###autoload
(defun gnus-cvslog-highlight-maybe ()
  "Highlight CVS log message iff `gnus-cvslog-enabled' is non-nil.
You can use this to toggle CVS log message highlightings via group
parameters."
  (if gnus-cvslog-enabled
      (gnus-cvslog-highlight)))

;;;###autoload
(defun gnus-cvslog-highlight ()
  "Highlight CVS log message.
This is the main entry point of this file."
  (save-excursion
      (set-buffer gnus-article-buffer)
      ;; FIXME: clean up the subject line? Would be nice to replace
      ;; the line wraps so that they are only *between* files.
      (article-goto-body)
      (when (looking-at gnus-cvslog-start-line)
	(let ((inhibit-read-only t))
	  (save-restriction
	    (put-text-property (match-beginning 0) (match-end 0) 'face
			       gnus-cvslog-header-face)
	    (put-text-property (match-beginning 1) (match-end 1) 'face
			       gnus-cvslog-filename-face)
	    (forward-line 1)
	    (narrow-to-region (point)
			      (save-excursion (gnus-cvslog-next-header)))
	    (gnus-cvslog-highlight-head))
	  (gnus-cvslog-highlight-parts)
	  ))))

(defun gnus-cvslog-remove-highlights (beg end)
  "Remove all Gnus-made highlightings from region.
Region lasts from BEG to END.  This includes both text properties and
overlays."
  (mapc 'delete-overlay (overlays-in beg end))
  (remove-text-properties beg end '(face)))



(defun gnus-cvslog-next-header ()
  "Find the next CVS log part header after point.
If none is there, return `point-max'."
  (if (re-search-forward (concat "^\\(" gnus-cvslog-diff-header
				 "\\|"  gnus-cvslog-added-header
				 "\\|"  gnus-cvslog-deleted-header
				 "\\)")
			 nil t)
      (match-beginning 0)
    (point-max)))


(defun gnus-cvslog-highlight-head ()
  "Highlight the \"head\" part of a CVS log message."
  (while (not (looking-at "Log Message:"))
    (cond
     ((looking-at "[ \t]") ; filename entry
      (skip-chars-forward " \t")
      (put-text-property (point) (point-at-eol) 'face
			 gnus-cvslog-filename-face))
     ((looking-at "[^ ]* Files:")
      (put-text-property (point) (point-at-eol) 'face
			 gnus-cvslog-header-face)))
    (forward-line 1))
  (put-text-property (point) (point-at-eol) 'face
		     gnus-cvslog-header-face)
  (forward-line 1)
  (gnus-cvslog-remove-highlights (point) (point-max))
  (put-text-property (point) (point-max) 'face
		     gnus-cvslog-log-face)
  (while (not (eobp))
    (save-restriction
      (narrow-to-region (point) (point-at-eol))
      (fill-paragraph nil)
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (delete-region (point) (point-max)))
    (forward-line 1))
  (forward-line 1))

(defun gnus-cvslog-highlight-parts ()
  "Highlight all CVS log parts.
Diff parts are highlighted like the command `diff-mode' would, other
parts just get a header in boldface.  Point has to be before the first
part."
  (while (not (eobp))
    (let ((pstart (point)) pend buf buf2 p1 p2 ptype)
      (forward-line 1)
      ;; get next part
      (setq pend (gnus-cvslog-next-header))
      ;; clean up first
      (gnus-cvslog-remove-highlights pstart pend)
      ;; highlight header
      (goto-char pstart)
      (cond
       ((looking-at gnus-cvslog-diff-header)
	(setq ptype t))
       ((looking-at gnus-cvslog-deleted-header)
	nil)
       ((looking-at gnus-cvslog-added-header)
	nil)
       (t (error "What is here? %S" (point))))
      (put-text-property (match-beginning 0) (match-end 0) 'face
			 gnus-cvslog-header-face)
      (if (match-beginning 1)
	  (put-text-property (match-beginning 1) (match-end 1) 'face
			     gnus-cvslog-filename-face))
      (goto-char (match-end 0))
      (when ptype
	(forward-line 1)
	(setq buf (current-buffer)
	      p1 (point)
	      p2 pend)
	(with-temp-buffer
	  (insert-buffer-substring buf p1 p2)
	  (diff-mode)
	  (font-lock-fontify-buffer)
	  (message nil)
	  (setq buf2 (current-buffer)
		p1 (point-min)
		p2 (point-max))
	  (with-current-buffer buf
	    (delete-region (point) pend)
	    (insert-buffer-substring buf2 p1 p2))))
    (goto-char pend))))

(provide 'gnus-cvslog)

;;; gnus-cvslog.el ends here

