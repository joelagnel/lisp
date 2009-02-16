;;; bm-man-cmplt.el --- completion of Unix manual page spec. using apropos(1)

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01...@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01...@nifty.ne.jp>
;; $Revision: 1.3 $
;; Keywords: local, help

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Read man page spec. string (e.g. "ls(1)") from the minibuffer with
;; completion using Unix command apropos(1).

;; Requirement
;; I tested this library with FSF Emacs 20.6.2 and XEmacs 21.1.8.
;;
;; This library needs Unix command apropos(1) which can perform shell style
;; wildcard search of Unix manual page names and their short descriptions.
;;
;; If your apropos(1) have this capability, you should be able to do
;; something like the following in a shell session;
;;
;;     ~$ apropos -w 'ls*'  # list every man page whose name or discription
;;                          # starts with 'ls'
;;     ls (1)               - list directory contents
;;     lseek (2)            - reposition read/write file offset
;;     lstat (2)            - get file status
;;     ....
;;

;; Usage (as a programmed completion function for `completing-read')
;;
;; Give `bm-man-cmplt' to `completing-read' as the second argument COLLECTION.
;; See "(elisp)Completion".
;;
;; Example
;; #1 Complete with all manual sections.
;;      (completing-read "man page: " 'bm-man-cmplt)
;;
;;      ;; read user input using minibuffer with completion
;;
;;      => "ls(1)"
;;
;;
;; #2 Complete with only '3pm' (Perl modules) section.
;;      (completing-read "perl lib: " 'bm-man-cmplt
;;                       (lambda (cell) ; predicate
;;                         (string-match "(3pm)\\'" (car cell))))
;;
;;      ;; read user input using minibuffer with completion
;;
;;      => "Net::Cmd(3pm)"
;;                                        

;;; Code:

(defvar bm-man-cmplt-apropos
  "apropos"
  "*Command which list manual page names and their short descriptions.")

(defvar bm-man-cmplt-apropos-wildcard-search-option
  "-w"
  "*Shell style wildcard search option for `bm-man-cmplt-apropos'.")

(defvar bm-man-cmplt-apropos-other-args
  nil
  "*List of arguments to `bm-man-apropos' other than `bm-man-cmplt-apropos-wildcard-search-option'.")

(defvar bm-man-cmplt-apropos-name-sec-regexp
  "^\\([^ ]+\\) +\\(([^)]+)\\)"
  "*Regular expression recognizing name and section part of apropos(1)'s output.")

(defun bm-man-cmplt (string predicate flag)
  "Programmed completion function of man page spec. for `completing-read'.
see \"(elisp)Programmed Completion\"."
  (cond
   ((null flag)
    (try-completion string (bm-man-cmplt-get-alist string) predicate))
   ((eq flag t)
    (all-completions string (bm-man-cmplt-get-alist string) predicate))
   ((eq flag 'lambda)
    ;; `t' if STRING is an exact match for some possibility, otherwise `nil'
    (let ((cell (assoc string (bm-man-cmplt-get-alist string))))
      (if (and cell
               (or (null predicate)
                   (funcall predicate cell)))
          t nil)))
   (t   (error "bm-man-cmplt: Invalid FLAG `%s'" (prin1-to-string flag)))))

(defun bm-man-cmplt-get-alist (man-spec)
  "Return alist of *possible* completions of MAN-SPEC."
  (let* ((name (progn (string-match "\\`[^(]*" man-spec) ; always match
                      (match-string 0 man-spec)))
         (quoted-name (funcall (if completion-ignore-case
                                   'bm-man-cmplt-get-ignore-case-arg
                                 'identity)
                               (if (zerop (length name))
                                   ""   ; avoid (shell-quote-argument "")=>"''"
                                 (shell-quote-argument name))))
         alist)
    (with-temp-buffer
      (apply 'call-process
             bm-man-cmplt-apropos
             nil                        ; < /dev/null
             (current-buffer)           ; > current temporary buffer
             nil                        ; no redisplay
             (append bm-man-cmplt-apropos-other-args
                     (list bm-man-cmplt-apropos-wildcard-search-option
                           (concat quoted-name "*"))))

      (goto-char (point-min))
      (while (re-search-forward bm-man-cmplt-apropos-name-sec-regexp nil t)
        (setq alist (cons (list (concat (match-string 1) ; page name e.g. "ls"
                                        (match-string 2))) ; section e.g. "(1)"
                          alist))))
    alist))

(defun bm-man-cmplt-get-ignore-case-arg (quoted-arg)
  "Return a copy of QUOTED-ARG with letters changed to match both cases.
e.g. (bm-man-cmplt-get-ignore-case-arg \"ls\") => \"[Ll][Ss]\"."
  (let ((start 0)
        result)
    (save-match-data
      (while (string-match "[a-zA-Z]" quoted-arg start)
        (setq result (concat
                      result
                      (substring quoted-arg start (match-beginning 0))
                      "["
                      (upcase (match-string 0 quoted-arg))
                      (downcase (match-string 0 quoted-arg))
                      "]")
              start (match-end 0)))
      (setq result (concat result (substring quoted-arg start))))
    result))



 (defun bm-man (man-spec)
  "Read Unix manual page name with completion and display the page."
  (interactive (list (bm-man-read)))
  (if (featurep 'xemacs) (manual-entry man-spec)
    ;; workaround (FSF Emacs's man.el fails with "[(1)")
    (string-match "\\([^(]+\\)(?\\([^)]*\\)" man-spec)
    (man (format "%s %s"
                 (match-string 2 man-spec)
                 (match-string 1 man-spec)))))

(defun bm-man-read ()
  (let* ((sec-regexp (format "(%s[^)]*)\\'"
                             (number-to-string
                              (prefix-numeric-value current-prefix-arg))))
         (predicate  (when current-prefix-arg
                       (lambda (cell)
                         (string-match sec-regexp (car cell))))))
    (completing-read "Manual entry: " 'bm-man-cmplt predicate))) 

(provide 'bm-man-cmplt)

;;; bm-man-cmplt.el ends here 