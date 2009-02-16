;;; html-tt.el --- Template Toolkit for html-helper-mode
;;; $Id: html-tt.el 40 2002-05-03 02:54:12Z yoshiki $
;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Yoshiki KURIHARA <yoshiki@clouder.jp>
;; Keywords: tools, languages

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

;; INSTALL:
;;  1.Put this file to a directory in load-path.
;;  2.Add the following line to your ~/.emacs:
;;	(require 'html-tt)
;;
;;  Setting for html-helper-mode with html-tt:
;;
;;      (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;;      (setq auto-mode-alist
;;            (cons
;;             '("\\.html$" . html-helper-mode) auto-mode-alist))
;;      (require 'html-tt)
;; 	(add-hook 'html-helper-mode-hook 'html-tt-load-hook)
;;
;;  Setting values:
;;
;;      ;; change sequence face
;;      (make-face 'my-sequence-face)
;;      (set-face-foreground 'my-sequence-face "blue")
;;      (set-face-background 'my-sequence-face "bisque")
;;      (setq html-tt-sequence-face 'my-sequence-face)
;;      ;; or
;;      (setq html-tt-sequence-face 'bold)
;;      (setq html-tt-sequence-face 'italic)
;;      (setq html-tt-sequence-face 'underline)
;;
;;      ;; change sequence for insert
;;      (setq html-tt-sequence-start "[% ")
;;      (setq html-tt-sequence-end " %]")
;;
;; Commentary:
;;
;;      Mar 28, 2002 - Project Start
;;                     add html-tt-insert-sequence
;;                     add html-tt-insert-directive
;;      May 01, 2002 - Fixed
;;                     add html-tt-load-hook
;;                     support font-lock
;;      May 03, 2002 - Update
;;                     use tempo.el
;;                     add some insertion
;;                

;; Code:

(provide 'html-tt)
(require 'html-helper-mode)
(require 'tempo)
(require 'font-lock)

(defvar html-tt-directive-alist
  '(
    ("IF")
    ("UNLESS")
    ("ELSIF")
    ("ELSE")
    ("FOREACH")
    ("WHILE")
    ("FILTER")
    ("GET")
    ("CALL")
    ("MACRO")
    ("SET")
    ("DEFAULT")
    ("INSERT")
    ("INCLUDE")
    ("BLOCK")
    ("END")
    ("PROCESS")
    ("WRAPPER")
    ("SWITCH")
    ("CASE")
    ("USE")
    ("PERL")
    ("RAWPERL")
    ("TRY")
    ("THROW")
    ("FINAL")
    ("CATCH")
    ("NEXT")
    ("LAST")
    ("RETURN")
    ("STOP")
    ("CLEAR")
    ("META")
    ("TAGS")
    )
  "Template Toolkit Directives alist for complation.")

;; start sequence
(defvar html-tt-sequence-start "[% "
  "Template Toolkit start sequence for html-tt-insert-sequence.")

;; end sequence
(defvar html-tt-sequence-end " %]"
  "Template Toolkit end sequence for html-tt-insert-sequence.")

;; insert sequence
(defun html-tt-insert-sequence ()
  "Insert Template Toolkit sequence."
  (interactive)
  (insert (concat html-tt-sequence-start html-tt-sequence-end)))

;; insert sequence with directive
(defun html-tt-insert-directive ()
  "Insert Template Toolkit sequence with directive."
  (interactive)
  (setq directive (completing-read "Directive: " html-tt-directive-alist))
  (and directive
       (insert (concat html-tt-sequence-start directive html-tt-sequence-end))))
;; set default sequence-face
(make-face 'sequence-face)
(set-face-foreground 'sequence-face "midnightblue")
(setq html-tt-sequence-face 'sequence-face)

;; set keywords
(defvar html-tt-font-lock-keywords
  (list
   '("\\[\%[_ \t\n\r]*[^\%]+\%\\]" 0 html-tt-sequence-face t)
   '("\\[\% END \%\\]" 0 html-tt-sequence-face t)))

;; tempo-template
(require 'tempo)
(tempo-define-template
 "html-tt-insert-sequence"
 '(html-tt-sequence-start
   (p "Value: ")
   html-tt-sequence-end))

(tempo-define-template
 "html-tt-insert-directive"
 '(html-tt-sequence-start
   (p "Directive: ")
   html-tt-sequence-end))

(tempo-define-template
 "html-tt-insert-if"
 '(html-tt-sequence-start
   "IF " (p "Condition: ")
   html-tt-sequence-end
   p
   html-tt-sequence-start
   "END"
   html-tt-sequence-end))

(tempo-define-template
 "html-tt-insert-elsif"
 '(html-tt-sequence-start
   "ELSIF " (p "Condition: ")
   html-tt-sequence-end))

(tempo-define-template
 "html-tt-insert-else"
 '(html-tt-sequence-start
   "ELSE"
   html-tt-sequence-end))

(tempo-define-template
 "html-tt-insert-foreach"
 '(html-tt-sequence-start
   "FOREACH " (p "Condition: ")
   html-tt-sequence-end n p n
   html-tt-sequence-start
   "END"
   html-tt-sequence-end))

(tempo-define-template
 "html-tt-insert-while"
 '(html-tt-sequence-start
   "WHILE " (p "Condition: ")
   html-tt-sequence-end n p n
   html-tt-sequence-start
   "END"
   html-tt-sequence-end))

(tempo-define-template
 "html-tt-insert-switch"
 '(html-tt-sequence-start
   "SWITCH " (p "Name: ")
   html-tt-sequence-end n >
   html-tt-sequence-start
   "CASE " (p "Case: ")
   html-tt-sequence-end n p n
   html-tt-sequence-start
   "END"
   html-tt-sequence-end))

(tempo-define-template
 "html-tt-insert-include"
 '(html-tt-sequence-start
   "INCLUDE \"" (p "File: ") "\""
   html-tt-sequence-end))

;; load-hook
(defun html-tt-load-hook ()
  (interactive)
  ;; define key bind
  ;(define-key html-helper-mode-map "\C-cs"
  ;  'html-tt-insert-sequence)
  (define-key html-helper-mode-map "\C-cs"
    'tempo-template-html-tt-insert-sequence)
  (define-key html-helper-mode-map "\C-cd"
    'html-tt-insert-directive)
  (define-key html-helper-mode-map "\C-cn"
    'tempo-template-html-tt-insert-directive)
  (define-key html-helper-mode-map "\C-ci"
    'tempo-template-html-tt-insert-if)
  (define-key html-helper-mode-map "\C-cl"
    'tempo-template-html-tt-insert-elsif)
  (define-key html-helper-mode-map "\C-ce"
    'tempo-template-html-tt-insert-else)
  (define-key html-helper-mode-map "\C-cf"
    'tempo-template-html-tt-insert-foreach)
  (define-key html-helper-mode-map "\C-cw"
    'tempo-template-html-tt-insert-while)
  (define-key html-helper-mode-map "\C-cm"
    'tempo-template-html-tt-insert-switch)
  (define-key html-helper-mode-map "\C-cn"
    'tempo-template-html-tt-insert-include)

  ;; add hilit-set-mode-pattern, if use hilit19.
  (if (featurep 'hilit19)
      (hilit-add-pattern "\\[%" "%\\]" 'midnightblue 'html-helper-mode)
    )

  ;; set font-lock
  (make-local-variable 'font-lock-defaults)
  (setq html-tt-font-lock-keywords
	(append html-helper-font-lock-keywords html-tt-font-lock-keywords))
  (setq font-lock-defaults '(html-tt-font-lock-keywords t t))
  )

;;; html-tt.el ends here
