;;; lp-elisp.el --- Literate Programming in Emacs Lisp -*- mode: emacs-lisp -*-

;; Copyright (C) 1997 -- 2002 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 2 or,
;; at your option, any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; I'm sure you already have many copies of the GPL on your machine.
;; If you're using GNU Emacs, try typing  `C-h C-c' to bring it up.
;; If you're using XEmacs,  `C-h C-l' does this.

;;; Commentary:

;; At one time, my .emacs file was a literate program made with this
;; poor-man's literate programming system for Emacs Lisp. It uses
;; \davidw's  `two-mode-mode.el' to allow us to edit the files
;; using both \LaTeX\ and  `emacs-lisp' major modes.
;;
;; I've since switched to using \texttt `noweb', and I recommend that you do
;; the same.
;;
;; If you have the elisp but don't have the LaTeX source, you can find it
;; here:
;;
;;         \selfhref `http://edward.oconnor.cx/elisp/lp-elisp.tex'

;;; History:

;;; Code:

(require 'two-mode-mode)

(defun literate-emacs-lisp-mode ()
  "Treat the current buffer as a literate Emacs Lisp program."
  (interactive)
  (setq default-mode    '("LaTeX" latex-mode)
        second-modes     '(("Emacs-Lisp"
                            ;; We break up the values so as to not have strange
                            ;; behavior when editing this function.
                            (concat "\\begin{" "elisp") ; }
                            (concat "\\end{" "elisp")  ; }
                            emacs-lisp-mode)))
  (two-mode-mode))

(defun lel-dotemacs-comment-filter (line)
  "Filter LINE for inclusion as a comment in an elisp file."
  (let ((newline line))
    (mapc (lambda (match)
            (while (string-match (car match) newline)
              (setq newline (replace-match (cdr match) t t newline))))
          '(("{19}\\(\\\\\\)?"                          . " 19")
            ("{20}\\(\\\\\\)?"                          . " 20")
            ("{21}\\(\\\\\\)?"                          . " 21")
            ("{21.3}\\(\\\\\\)?"                        . " 21.3")
            ("\\\\Emacs\\(\\\\\\)?"                     . "GNU Emacs")
            ("\\\\Meadow\\(\\\\\\)?"                    . "Meadow")
            ("\\\\dotemacs\\(\\\\\\)?"                  . ".emacs")
            ("\\\\verb"                                 . "")
            ("|"                                        . "")
            ("{"                                        . " `")
            ("}\\(\\\\\\)?"                             . "'")
            ("\\\\el\\(var\\|fun\\|package\\|buffer\\)" . "")
            ("\\\\el\\(key\\|prog\\|file\\|mode\\)"     . "")
            ("\\\\XEmacs\\(\\\\\\)?"                    . "XEmacs")))
    newline))

(defun lel-generate-elisp-file (prefix docstring &optional el-name)
  "Generate PREFIX.el (or EL-NAME) from TeX in PREFIX.tex."
  (save-window-excursion
    (save-excursion
      (let* ((tex-file-name (concat prefix ".tex"))
             (elisp-file-name (or el-name (concat prefix ".el")))
             (inbuf (or (get-file-buffer tex-file-name)
                        (find-file tex-file-name)))
             outbuf
             (elisp (concat (format ";;; %s --- %s "
                                    elisp-file-name docstring)
                            "-*- mode: emacs-lisp -*-\n"))
             (in-elisp nil)
             (in-comment nil)
             (comment-indent-amount 0))

        ;; Generate the new elisp file. Note that there are
        ;; probably many more efficient ways of doing this.
        (with-temp-file elisp-file-name
          (setq outbuf (current-buffer))
          (set-buffer inbuf)
          (goto-char (point-min))
          (message (concat (format "Extracting elisp from %s "
                                   tex-file-name)
                           "(this may take a while)..."))
          (while (not (>= (point) (point-max)))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (cond ((and in-elisp
                          (string-match
                           (concat "^\\\\en" "d{elisp[}E]")
                           line))
                     (setq elisp (concat elisp "\n"))
                     (setq in-elisp nil))

                    ((and (not in-elisp)
                          (string-match
                           (concat "^\\\\begi" "n{elisp[}E]")
                           line))
                     (setq in-elisp t))

                    ((and in-comment
                          (string-match
                           "^\\\\end{elcomment}$" ; $
                           line))
                     (setq elisp (concat elisp "\n"))
                     (setq in-comment nil))

                    ((and (not in-comment)
                          (string-match
                           "^\\\\begin{elcomment}{\\([0-9]\\)+}$" ; $
                           line))
                     (setq in-comment t
                           comment-indent-amount
                           (string-to-number
                            (substring line
                                       (match-beginning 1)
                                       (match-end 1)))))

                    (in-comment
                     (setq elisp (concat elisp
                                         "\n"
                                         (make-string
                                          comment-indent-amount
                                          ?\ )
                                         ";; "
                                         (lel-dotemacs-comment-filter line))))

                    (in-elisp
                     (setq elisp (concat elisp "\n" line))))

              (forward-line 1)))
          (set-buffer outbuf)
          (insert elisp "\n")
          (delete-trailing-whitespace))))))

(provide 'lp-elisp)

;;; lp-elisp.el ends here

