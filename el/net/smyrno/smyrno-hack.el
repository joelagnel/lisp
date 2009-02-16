;;; smyrno-hack.el --- support for hacking on Smyrno

;; Copyright (C) 2002  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(defvar smyrno-debug-p (get-buffer-create "*smyrno-debug-log*"))

(defun smyrno-hack-log (prompt &rest args)
  (when smyrno-debug-p
    (with-current-buffer smyrno-debug-p
      (goto-char (point-max))
      (insert prompt ": " (apply 'format args) "\n"))))

(defun xe-log (&rest args)
  (apply 'smyrno-hack-log "XE" args))

(defun xs-log (&rest args)
  (apply 'smyrno-hack-log "XS" args))

(defun xmpp-log (&rest args)
  (apply 'smyrno-hack-log "XM" args))

;; Let us indent sexps that represent XML nicely.
;; Warning: this is really, really annoying when you're not
;; mucking with Smyrno.

(mapc (lambda (symbol)
        (put symbol 'lisp-indent-function 1))
      '(message presence iq item query jabber-message-bind
        jabber-presence-bind jabber-iq-bind))

;; Make smyrno code prettier. Yay fruit salad.
(and (fboundp 'font-lock-add-keywords)
     (font-lock-add-keywords
      'emacs-lisp-mode
      '(("(\\(jabber-\\([a-z]+\\)-bind\\)\\>"
         (1 font-lock-keyword-face)))))

;; Straight-up pilfered from chess/lispdoc.el. johnw rules.
(unless (fboundp 'update-lispdoc-tags)
  (defun update-lispdoc-tags ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@c lispfun \\(.+\\)" nil t)
        (let ((name (match-string 1)) begin end)
          (message "Update lispdoc for function '%s'" name)
          (if (re-search-forward (concat "^@defun " name) nil t)
              (setq begin (match-beginning 0)))
          (if (re-search-forward "^@end defun" nil t)
              (setq end (match-end 0)))
          (if (and begin end)
              (delete-region begin end))
          (let* ((sym (or (intern-soft name)
                          (signal 'wrong-type-argument
                                  (list 'functionp name))))
                 (data (let ((func (symbol-function sym)))
                         (while (symbolp func)
                           (setq func (symbol-function func)))
                         func))
                 (args (pp-to-string (if (listp data)
                                         (cadr data)
                                       (aref data 0))))
                 (doc (documentation sym)))
            (if (or (null doc) (= (length doc) 0))
                (message "warning: no documentation available for '%s'" name)
              (unless (and begin end)
                (insert ?\n ?\n))
              (insert (format "@defun %s %s\n" name
                              (substring args 1 (- (length args) 2))))
              (setq begin (point))
              (insert doc ?\n)
              (save-restriction
                (narrow-to-region begin (point))
                (goto-char (point-min))
                (let ((case-fold-search nil))
                  (while (re-search-forward "[A-Z][A-Z-]+" nil t)
                    (replace-match (format "@var{%s}"
                                           (downcase (match-string 0))) t t)))
                (goto-char (point-max)))
              (insert "@end defun"))))))))

;; Basically pilfered from chess-maint.el.
(defun smyrno-generate-texinfo-file ()
  "Generate a new smyrno-final.texi from smyrno.texi."
  (interactive)
  (require 'texinfo)
  (with-temp-buffer
    (let ((default-directory "~/elisp/smyrno/"))
      (insert-file-contents-literally "smyrno.texi")
      (texinfo-mode)
      (require 'smyrno)
      (texinfo-insert-node-lines (point-min) (point-max) t)
      (texinfo-every-node-update)
      (texinfo-all-menus-update t)
      (texinfo-every-node-update)
      (texinfo-all-menus-update t)
      (update-lispdoc-tags)
      (write-file "smyrno-final.texi"))))

(provide 'smyrno-hack)
;;; smyrno-hack.el ends here
