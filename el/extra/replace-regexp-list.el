;;; Time-stamp: <2005-07-06 18:25:24 jcgs>

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


(provide 'replace-regexp-list)

;;;###autoload
(defun apply-replace-regexp-alist (edit-list &optional fixedcase literal)
  "Apply EDIT-LIST to the current buffer, and return the total count of changes.
Each edit is a regexp and a replacement for that regexp.
The edits are applied in order, to the whole buffer; that is, the buffer is
edited throughout with the first one, then with the next one, and so on.
See documentation of replace-match for optional arguments."
  (let ((count 0))
    (save-excursion
      (while edit-list
	(goto-char (point-min))
	(let* ((edit (car edit-list))
	       (regexp (car edit))
	       (replacement (cdr edit)))
	  (while (re-search-forward regexp nil t)
	    (replace-match replacement fixedcase literal)
	    (setq count (1+ count))))
	(setq edit-list (cdr edit-list))))
    count))

;;;###autoload
(defun apply-replace-regexp-alist-repeatedly (edit-list &optional fixedcase literal)
  "Like apply-replace-regexp-alist but keeps doing it until no further changes occur.
It is not guaranteed that this will ever terminate."
  (while (not (zerop (apply-replace-regexp-alist edit-list fixedcase literal)))))


;;; I'm not sure who wrote this; I don't think I did. JCGS
;;;###autoload
(defun replace-with-eval-result (regexp form)
  "Replace occurrences of REGEXP with the result of (eval FORM).
While FORM is evaluated, the variable \\& is bound to the whole
matched text, and the variables \\1, \\2 etc bound to the
corresponding one of the strings matched by bracketed expressions in
the regexp. The whole matched text is deleted from the buffer before
the form is evaluated. Thus, it is quite like replace-regexp, with
binding names being the same as the corresponding substitution markers
in the replacement string of replace-regexp."
  (interactive "sReplace regexp:
xReplace regexp %s with result of form: ")
  (while (re-search-forward regexp (point-max) t)
    (let ((m-d (match-data))
	  (matched-strings nil))
      (while m-d
	(setq matched-strings (cons (buffer-substring (car m-d) (cadr m-d))
				    matched-strings)
	      m-d (cddr m-d)))
      (progv
	  '(\\& \\1 \\2 \\3 \\4 \\5 \\6 \\7 \\8 \\9)
	  (nreverse matched-strings)
	(delete-region (match-beginning 0)
		       (match-end 0))
	(let ((result (save-excursion
			(save-window-excursion
			  (eval form)))))
	  (if (not (stringp result))
	      (setq result (prin1-to-string result 'no-escape)))
	  (insert result))))))

;;; end of replace-regexp-list.el
