;;;; skip-initial-comments.el -- skip initial comments when loading a file
;;; Time-stamp: <2005-01-18 19:13:55 jcgs>
;;; started Wed Nov 24 2004

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'skip-initial-comments)

;;;###autoload
(defun skip-initial-comments ()
  "Move forward over comments, if at the top of the file.
Interactively, move to the first non-comment in the file, unconditionally.
The idea is to put this on find-file-hooks.
It's a crude implementation at present, that won't work for multi-line comments
without a comment marker at the start of each line of comment."
  (interactive)
  (if (interactive-p) (goto-char (point-min)))
  ;; compare with zero rather than point-min, as not making sense for narrowed buffers
  (if (= (point) 1)
      ;; only works for comment syntax with every line of comment
      ;; marked as such -- OK for Lisp but not for C
      (while (and (not (eobp))
		  (or (looking-at "^\\s-*\\s<")
		      (looking-at "^ *$")))
	(forward-line 1))))

(add-hook 'find-file-hooks 'skip-initial-comments 'append)

;;; end of skip-initial-comments.el
