;;; clhs.el -- access the Common Lisp HyperSpec (CLHS)

;; Copyright (C) 2002   Free Software Foundation, Inc.

;; Author: Sam Steingold <address@bogus.example.com>
;; Maintainer: FSF
;; Keywords: lisp

;; This file is a part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Kent Pitman and the Harlequin Group (later Xanalys) have made the
;; text of American National Standard for Information Technology --
;; Programming Language -- Common Lisp, ANSI X3.226-1994 available on
;; the WWW, in the form of the Common Lisp HyperSpec.  This package
;; makes it convenient to peruse this documentation from within Emacs.

;; This is inspired by the Erik Naggum's version of 1997.

;;; Code:

(eval-when-compile (require 'cl)) ; push
(require 'browse-url)
(require 'thingatpt)
(require 'url)

(defcustom common-lisp-hyperspec-root "e:/lisp/docs/HyperSpec";
  "*The root of the Common Lisp HyperSpec URL.
If you copy the HyperSpec to your local system, set this variable to
something like \"file:/usr/local/doc/HyperSpec/\"."
  :group 'lisp
  :type 'string
  :version "21.3")

(defvar clhs-history nil
  "History of symbols looked up in the Common Lisp HyperSpec.")

(defvar clhs-symbols nil)

(defun clhs-table-buffer (&optional root)
  (unless root (setq root common-lisp-hyperspec-root))
  (if (string-match "^file:/" root)
      (with-temp-buffer
        (insert-file-contents-literally
         (let* ((d (concat (substring root 5) "/Data/"))
                (f (concat d "Map_Sym.txt")))
           (if (file-exists-p f) f
             (setq f (concat d "Symbol-Table.text"))
             (if (file-exists-p f) f
               (error "no symbol table at ~s" root))))
         nil nil nil t)
        (goto-char 0)
        (current-buffer))
    (let* ((d (concat root "/Data/"))
           (f (concat d "Map_Sym.txt")))
      (set-buffer (url-retrieve-synchronously f))
      (goto-char 0)
      (unless (looking-at "^HTTP/.*200 OK$")
        (kill-buffer (current-buffer))
        (setq f (concat d "Symbol-Table.text"))
        (set-buffer (url-retrieve-synchronously f))
        (goto-char 0)
        (unless (looking-at "^HTTP/.*200 OK$")
          (kill-buffer (current-buffer))
          (error "no symbol table at ~s" root)))
      ;; skip to the first symbol
      (search-forward "\n\n")
      (current-buffer))))

(defun clhs-read-symbols ()
  "read `clhs-symbols' from the current position in the current buffer"
  (while (not (eobp))
    (puthash (buffer-substring-no-properties ; symbol
              (line-beginning-position) (line-end-position))
             (progn (forward-line 1) ; file name
                    (buffer-substring-no-properties ; strip "../"
                     (+ 3 (line-beginning-position)) (line-end-position)))
             clhs-symbols)
    (forward-line 1)))

(defun clhs-symbols ()
  "Get `clhs-symbols' from `common-lisp-hyperspec-root'."
  (if (and clhs-symbols (not (= 0 (hash-table-count clhs-symbols))))
      clhs-symbols
    (with-current-buffer (clhs-table-buffer)
      (unless clhs-symbols
        (setq clhs-symbols (make-hash-table :test 'equal :size 1031)))
      (clhs-read-symbols)
      (kill-buffer (current-buffer))
      clhs-symbols)))

(defun hash-table-complete (string table how)
  (let ((res nil) (st (upcase string)) (len (length string)))
    (maphash (lambda (key val)
               (when (and (<= len (length key))
                          (string= st (substring key 0 len)))
                 (push key res)))
             table)
    (if how res                 ; `all-completions'
      (if (cdr res) (try-completion st (mapcar #'list res))
        (if (string= st (car res)) t (car res))))))

;;;###autoload
(defun common-lisp-hyperspec (symbol-name)
  "Browse the Common Lisp HyperSpec documentation for SYMBOL-NAME.
Finds the HyperSpec at `common-lisp-hyperspec-root'."
  (interactive (list (let ((sym (thing-at-point 'symbol)))
                       (completing-read
                        "Look-up symbol in the Common Lisp HyperSpec: "
                        #'hash-table-complete (clhs-symbols)
                        t sym 'clhs-history))))
  (browse-url (concat common-lisp-hyperspec-root
                      (gethash (upcase symbol-name) (clhs-symbols)))))

(provide 'clhs)