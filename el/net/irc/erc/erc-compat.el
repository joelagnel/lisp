;;; erc-compat.el --- ERC compatibility code for XEmacs

;; Copyright (C) 2002,2003 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?EmacsIRCClient

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mostly defines stuff that cannot be worked around easily.

;;; Code:

(defconst erc-compat-version "$Revision: 1.16.2.4 $"
  "ERC compat revision.")

;; erc-define-minor-mode: the easy-mmode-define-minor-mode available
;; in XEmacs' easy-mmode.el does not have the BODY argument.  This
;; code has to work, even if somebody has defaliased
;; easy-mmode-define-minor-mode to define-minor-mode.  The code runs a
;; test first, and if define-minor-mode works, it uninterns all the
;; symbols created, so nothing should be left behind.

;;;###autoload (autoload 'erc-define-minor-mode "erc-compat")
(condition-case nil
    (progn
      (define-minor-mode erc-compat-test "Testing `define-minor-mode'." nil nil nil (ignore))
      (mapc 'unintern (apropos-internal "^erc-compat-test"))
      (defalias 'erc-define-minor-mode 'define-minor-mode)
      (put 'erc-define-minor-mode 'edebug-form-spec 'define-minor-mode))
  (error 
   (defmacro erc-define-minor-mode (mode doc &optional init-value lighter
                                    keymap &rest body)
     "Define a minor mode like in Emacs."
     ;; Deal with at least /some/ keywords.
     ;; the rest don't seem to be as important.
     (let (keyw globalp group)
       (while (keywordp (setq keyw (car body)))
         (setq body (cdr body))
         (case keyw
           (:global (setq globalp (pop body)))
           (:group (setq group (pop body)))
           (t (pop body))))
       `(progn
          (if ,group
              (defcustom ,mode ,init-value
                "Non-nil if the corresponding mode is enabled."
                :group ,group
                :type 'boolean)
              (defvar ,mode ,init-value
                "Non-nil if the corresponding mode is enabled."))
          (unless ,globalp
            (make-variable-buffer-local ',mode))
          (defun ,mode (&optional arg)
            ,doc
            (interactive)
            (setq ,mode (if arg
                            (> (prefix-numeric-value arg) 0)
                            (not ,mode)))
            ,@body
            ,mode)
          (add-minor-mode ,mode ,lighter ,keymap))))
   (put 'erc-define-minor-mode 'edebug-form-spec
	'(&define name stringp
		  [&optional sexp sexp &or consp symbolp]
		  [&rest
		   [keywordp sexp]]
		  def-body))
   ))

;; MULE: decode-coding-string and encode-coding-string -- note that
;; XEmacs' functions do not have the NOCOPY argument.

;; latin-1 is only available as iso-8859-1 on XEmacs.  Since that
;; works for both, we will use that.

(condition-case nil
    ;; Try 3 arguments
    (progn
      (decode-coding-string "a" 'iso-8859-1 t)
      (defun erc-decode-coding-string (s coding-system)
	"Decode S using CODING-SYSTEM."
	(decode-coding-string s coding-system t)))
  (error
   (condition-case nil
       ;; Try 2 arguments
       (progn
	 (decode-coding-string "a" 'iso-8859-1)
	 (defun erc-decode-coding-string (s coding-system)
	   "Decode S using CODING-SYSTEM."
	   (decode-coding-string s coding-system)))
     (error
      ;; Default
      (defun erc-decode-coding-string (s &rest ignore)
	"Return S."
	s)))))

(condition-case nil
    ;; Try 3 arguments
    (progn
      (encode-coding-string "a" 'iso-8859-1 t)
      (defun erc-encode-coding-string (s coding-system)
	"Encode S using CODING-SYSTEM.
Return the same string, if the encoding operation is trivial.
See `erc-encoding-coding-alist'."
	(encode-coding-string s coding-system t)))
  (error
   (condition-case nil
       ;; Try 2 arguments
       (progn
	 (encode-coding-string "a" 'iso-8859-1)
	 (defun erc-encode-coding-string (s coding-system)
	   "Encode S using CODING-SYSTEM.
See `erc-encoding-coding-alist'."
	   (encode-coding-string s coding-system)))
     (error
      ;; Default
      (defun erc-encode-coding-string (s &rest ignore)
	"Return S unchanged."
	s)))))

;;; fields: XEmacs seems to lack them completely
(if (not (fboundp 'field-end))
    (defun field-end (pos &optional ignored)
      (save-excursion
        (let ((field (get-text-property pos 'field)))
          (goto-char pos)
          (while (and field
                      (eq field (get-text-property (point) 'field)))
            (forward-char))
          (point)))))

;;; XEmacs does not have `view-mode-enter', but its `view-mode' has a
;;; similar argument list.  And we need this in erc-match.el.

;; Emacs view-mode-enter: (view-mode-enter &optional RETURN-TO
;; EXIT-ACTION)

;; XEmacs view-mode: (view-mode &optional PREV-BUFFER EXIT-ACTION
;; CLEAN-BS)

;; But note Emacs view-mode: (view-mode &optional ARG)

(when (and (fboundp 'view-mode)
	   (not (fboundp 'view-mode-enter)))
  (defalias 'view-mode-enter 'view-mode))

;;; XEmacs has `replace-in-string', Emacs has `replace-regexp-in-string':

(cond ((fboundp 'replace-regexp-in-string)
       (defalias 'erc-replace-regexp-in-string 'replace-regexp-in-string))
      ((fboundp 'replace-in-string)
       (defun erc-replace-regexp-in-string (regexp rep string &optional fixedcase literal)
         (replace-in-string string regexp rep literal))))
;;; Done!

;; XEmacs has a string representation of the build time. Really!
(setq erc-emacs-build-time
      (if (stringp emacs-build-time)
          (date-to-time emacs-build-time)
          emacs-build-time))

;; XEmacs' `replace-match' does not replace matching subexpressions in strings.
(defun erc-replace-match-subexpression-in-string
  (newtext string match subexp start &optional fixedcase literal)
  "Replace the subexpression SUBEXP of the last match in STRING with NEWTEXT.
MATCH is the text which matched the subexpression (see `match-string').
START is the beginning position of the last match (see `match-beginning').
See `replace-match' for explanations of FIXEDCASE and LITERAL."
  (cond ((featurep 'xemacs)
         (string-match match string start)
         (replace-match newtext fixedcase literal string))
        (t (replace-match newtext fixedcase literal string subexp))))

;; If a version of Emacs or XEmacs does not have gnus or tramp, they
;; will not have the format-spec library.  We deal with this by
;; providing copies of its functions if the library is not available.
(condition-case nil
    (require 'format-spec)
  (error
   (defun format-spec (format specification)
     "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"bash %u %k\",
while SPECIFICATION is an alist mapping from format spec characters
to values."
     (with-temp-buffer
       (insert format)
       (goto-char (point-min))
       (while (search-forward "%" nil t)
         (cond
          ;; Quoted percent sign.
          ((eq (char-after) ?%)
           (delete-char 1))
          ;; Valid format spec.
          ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
           (let* ((num (match-string 1))
                  (spec (string-to-char (match-string 2)))
                  (val (cdr (assq spec specification))))
             (delete-region (1- (match-beginning 0)) (match-end 0))
             (unless val
               (error "Invalid format character: %s" spec))
             (insert (format (concat "%" num "s") val))))
          ;; Signal an error on bogus format strings.
          (t
           (error "Invalid format string"))))
       (buffer-string)))

   (defun format-spec-make (&rest pairs)
     "Return an alist suitable for use in `format-spec' based on PAIRS.
PAIRS is a list where every other element is a character and a value,
starting with a character."
     (let (alist)
       (while pairs
         (unless (cdr pairs)
           (error "Invalid list of pairs"))
         (push (cons (car pairs) (cadr pairs)) alist)
         (setq pairs (cddr pairs)))
       (nreverse alist)))))

;; Emacs has `cancel-timer', but XEmacs uses `delete-itimer'.
(defun erc-cancel-timer (timer)
  (cond ((fboundp 'cancel-timer)
         (cancel-timer timer))
        ((fboundp 'delete-itimer)
         (delete-itimer timer))
        (t
         (error "Cannot find `cancel-timer' variant"))))

(provide 'erc-compat)

;;; erc-compat.el ends here
