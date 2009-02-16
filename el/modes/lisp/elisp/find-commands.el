;;;; find-commands.el -- find functions matching a pattern, that have interactive definitions
;;; Time-stamp: <2005-02-04 18:21:40 jcgs>

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

(provide 'find-commands)

;;;###autoload
(defun find-commands-matching (pattern)
  "Find functions with an interactive definition, whose names match PATTERN."
  (interactive "sFind commands matching pattern: ")
  (let ((commands nil))
    (mapatoms (lambda (atom)
		(if (and (commandp atom)
			 (string-match pattern (symbol-name atom)))
		    (setq commands (cons atom commands)))))
    (if (interactive-p)
	(with-output-to-temp-buffer (format "*Commands matching %s*" pattern)
	  (let ((rest commands))
	    (while rest
	      (princ (symbol-name (car rest)))
	      (princ "\n")
	      (setq rest (cdr rest))))))
    commands))

;;;###autoload
(defun insert-voice-commands-matching (pattern)
  "Prepare a voice command list for PATTERN commands."
  (interactive "sPrepare voice command list for commands matching: ")
  (insert "(setq vr-" pattern "-commands '(\n")
  (mapcar (lambda (command)
	    (let* ((name (symbol-name command))
		   (end (string-match pattern name)))
		  (insert " (\"" (subst-char-in-string ?- ?  (substring name (match-end 0))) "\" . " name ")\n")))
	  (find-commands-matching pattern))
  (insert "))\n")
  )


;;; end of find-commands.el
