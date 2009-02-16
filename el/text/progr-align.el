;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; progr-align.el --- Allign stuff via regexpressions found in the buffer

;; Copyright (C) 2000-2001 by Stefan Reichoer

;; Emacs Lisp Archive Entry
;; Filename: progr-align.el
;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Version: 0.5

;; progr-align.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; progr-align.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; To use progr-align put the following line in your .emacs:
;; (require 'progr-align)
;; (global-set-key [(meta a)] 'progr-align)

;; progr-align aligns your text in nice columns specified by regular expressions.
;; The regular expressions are part of your written text/program

;; One writes a row containing #palign and then the "pattern" of the
;; rows. The "fields" are separated by commas, which contain a regular
;; expression describing the beginning of that "field", (which most of the
;; time will be a delimiter) and then a number that sets the length of
;; the field. Then one can merrily type the row without necessarily
;; aligning the delimiters, and then M-a in some row will align it
;; according to the pattern.

;; The following examples should demonstrate the usage of this package

;#palign  ";" 2, "[0-9]" 4, "&" 2, "\w+" 18, "&"2, "\w+" 24, "\\\\" 1
; 1.  & Cat Stevens       & Morning has broken      \\
; 2.  & Madonna           & Music                   \\

;  %%#palign ";" 5, "\w+" 13, "&" 9, "&" 9, "&" 9, "&" 13, "&" 9, "&" 13, "\\\\"
;  \begin{tabular}[h]{lllllll}
;    \toprule
;    KeyString    & 1      & +      & 5      & /          & 2      & RET        \\
;    KeyNumber    & 93     & 106    & 97     & 95         & 98     & 108        \\
;    ScanCode     & 69     & 79     & 73     & E04A       & 72     & E05A       \\
;    \midrule
;    Result       & 69F069 & 79F079 & 73F073 & E04AF0E04A & 72F072 & E05AF0E05A \\
;    \bottomrule
;  \end{tabular}

;#palign ";" 2, "bit" 4, "[0-9a-f\\.]+" 5, ":" 2, "\sw+" 15, ":" 2, "[0-9]" 2, "\"" 5
;# CFG0, Adr 0xF8
; bit F8.0 : PC_En          : 1 "Enable I2C Communication Module"
; bit F8.1 : SPI_En         : 0 "Enable SPI Communication Module"

;; The latest version of progr-align.el can be found at:
;;   http://www.xsteve.at/prg/emacs/progr-align.el

;; Thanks to Rafael Villarroel <rvf@hotpop.com> for documentation improvements

;; Comments / suggestions welcome!

;;; Code:
;(defvar progr-align-parse-header "^[ \t]*%#palign")
(defvar progr-align-parse-header "#palign")

;;; End of user settings

(defun progr-align-get-field-alignement (regexp)
  (let ((beg (point))
        (end (progn (end-of-line) (point))))
    (goto-char beg)
    (when (re-search-forward regexp end t)
      (goto-char (match-beginning 0))
      (current-column))))

(defun progr-align-get-line-alignement(rules)
  (save-excursion
    (beginning-of-line)
    (let* ((l rules)
           (new-rules)
           (column 0)
           (prev-column)
           (regexp)
           (prev-regexp)
           (width 0))
      (while l
        (setq prev-regexp regexp)
        (setq prev-column column)
        (setq regexp (caar l))
        (setq column (progr-align-get-field-alignement regexp))
        (unless column
          (message "%s not found" regexp)
          (setq l nil)
          (setq new-rules nil))
        (when column
          (setq width (- column prev-column))
          (when prev-regexp
            (setq new-rules (append new-rules (list (list prev-regexp width)))))
          (setq l (cdr l))))
      (when column
        (setq new-rules (append new-rules (list (list regexp -1)))))
      new-rules)))

(defun progr-align-field (regexp pos)
  (let ((beg (point))
        (end (line-end-position))
        (indent-offset 0))
    ;(goto-char beg)
    (if (re-search-forward regexp end t)
        (progn
          (goto-char (match-beginning 0))
          (setq indent-offset (- pos (current-column)))
          ;;(message "indent: %d" indent-offset)
          (unless (eq indent-offset 0)
            (message "realigning (%d): %s" indent-offset regexp)
            (delete-horizontal-space)
            (indent-to pos))
          (goto-char (+ (match-end 0) indent-offset))))))

(defun progr-align-line(rules &optional column)
  (save-excursion
     (unless column
       (back-to-indentation)
       (setq column (current-column)))
    (beginning-of-line)
    (let* ((l rules)
           (regexp "")
           (width 0)
           (num 0)
           (debug nil))
      (while l
        (setq regexp (caar l))
        (setq width (cadar l))
        (setq num (+ num 1))
        ;;(message "%s %d %d" regexp width column)
        (cond ((stringp regexp)
               (progr-align-field regexp column)
               ;(message "%d" (current-column))
               (when debug (insert (number-to-string num)))
               (setq column (+ column width)))
              ((eq regexp 'column)
               (beginning-of-line)
               (setq column width)))
        (setq l (cdr l))))))

(defun progr-align-line-match(rules)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((l rules)
           (regexp "")
           (width 0)
           (num 0)
           (matches t))
      (while l
        (setq regexp (caar l))
        (setq width (cadar l))
        (setq num (+ num 1))
        ;;(message "%s %d %d" regexp width column)
        (cond ((stringp regexp)
               (if (re-search-forward regexp (line-end-position) t)
                   (goto-char (match-end 0))
                 (setq matches nil)))
              ((eq regexp 'column)
               (beginning-of-line)
               (setq column width)))
        (setq l (cdr l)))
      matches)))


;08.04.2000
(defun progr-align-parse-rule()
  (save-excursion
    (let* ((beg (progn (re-search-backward progr-align-parse-header (point-min)) (match-end 0)))
           (end (line-end-position))
           (regexp)
           (width)
           (rules)
           (regexp-start-pos)
           (regexp-end-pos))
      (skip-chars-forward " \t")
      (when (looking-at "#\\([0-9]+\\)")
        (setq rules (append rules (list (list 'column (string-to-number (buffer-substring (match-beginning 1) (match-end 1)))))))
        (goto-char (match-end 0)))
      (while (re-search-forward "\"\\([^\"]*\\)\"" end t)
        (setq regexp-start-pos (match-beginning 1))
        (setq regexp-end-pos (match-end 1))
        ;; expand string if it contains \"
        (unless (string= "\\\\" (buffer-substring-no-properties (- regexp-end-pos 2) regexp-end-pos))
                (while (string= "\\" (buffer-substring-no-properties
                                      (- regexp-end-pos 1) regexp-end-pos))
                  (re-search-forward "\"" end t)
                  (setq regexp-end-pos (- (match-end 0) 1))))
        (setq regexp (buffer-substring-no-properties regexp-start-pos regexp-end-pos))
        (re-search-forward "[0-9]+" end t)
        (setq width (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))
        (setq rules (append rules (list (list regexp width)))))
      ;;(message "%S" rules)
      rules)))

(defun progr-align()
  ""
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((rules (progr-align-parse-rule))
          (end (point))
          (lastpos))
      (when rules
        (when mark-active
          ;(setq end (region-end))
          (setq end (- (region-end) 1))
          (goto-char (region-beginning)))
        (while (<= (point) end)
          (if (progr-align-line-match rules)
              (progr-align-line rules)
            (message "no match: %s" (buffer-substring (line-beginning-position) (line-end-position))))
          (when (eq lastpos (point)) (setq end 0))
          (setq lastpos (point))
          (forward-line 1)
          (beginning-of-line))))))

(provide 'progr-align)

;; arch-tag: cba7708a-b75e-4079-b20f-a0b69f9993c0