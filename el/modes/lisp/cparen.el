;;; cparen.el --- coloured parentheses in Lisp-derived modes

;; Copyright (C) 2001 Riku Saikkonen

;; Author: Riku Saikkonen <Riku.Saikkonen@hut.fi>
;; Version: 1.0
;; Keywords: faces lisp

;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:
;;;
;;; This package makes font-lock more colourful by colouring
;;; parentheses in Lisp-derived modes. In Lisp mode, the package
;;; simply makes all parentheses grey (`cparen-normal-paren-face'). In
;;; Scheme mode, however, parentheses are coloured according to their
;;; syntactic meaning. Try it and see if you like it.
;;;
;;; This package was motivated by the common complaint that Scheme has
;;; too many different uses for parentheses. I think it will be most
;;; useful for those who are new to Scheme, though I've also found it
;;; somewhat useful myself. The package recognises all syntactic forms
;;; defined in the Scheme standard R5RS.
;;;
;;; Activate the package by loading it and running M-x
;;; cparen-activate. Please note that there is currently no simple way
;;; to deactivate the package once it has been activated (except for
;;; restarting Emacs). If you want the package enabled by default,
;;; place cparen.el somewhere in your `load-path' and put the
;;; following lines in your ~/.emacs:
;;;   (require 'cparen)
;;;   (cparen-activate)
;;;
;;; Here's some example Scheme code that you can try to highlight
;;; (copy it to a buffer in Scheme mode and uncomment it). This is
;;; quite contrived code that displays all the colours; you should
;;; also try highlighting some normal Scheme code.
;;;
;;; (define (foo)
;;;   (let ((a 1) (b 2))
;;;     (define (foo a b)
;;;       (let* ((a 2) (b 3) (c 4))
;;;         ((foo 4)
;;;          (bar))
;;;         ((lambda (x y z) (+ x y z)) 1 2 3)
;;;         foo)
;;;       (and fds (let))
;;;       (if 1 (quote 2) '(3 3 #(a b c) 4) 4))
;;;     (cond ((= a 1) 2)
;;;           ((true? b) (+ a 2))
;;;           (else 4))
;;;     (begin (test) 3)
;;;     (set! x 2)
;;;     (case (* 5 2)
;;;       ((1 2 3) 'wrong)
;;;       ((10) 'right))
;;;     (do ((vec (make-vector 5))
;;;          (i 0 (+ i 1)))
;;;         ((= i 5) vec)
;;;       (vector-set! vec i i))
;;;     (let loop ((i 0) (j 1)) (loop 20))
;;;     5))
;;;
;;; (define-syntax begin
;;;   (syntax-rules ()
;;;     ((begin exp ...)
;;;      ((lambda () exp ...)))))
;;;
;;; The default colours (specified in the deffaces below) may look
;;; somewhat better with the following set of font-lock colours (which
;;; are the ones I use):
;;;
;;; (custom-set-faces
;;;  '(font-lock-string-face ((((class color) (background light)) (:foreground "IndianRed4"))))
;;;  '(font-lock-keyword-face ((((class color) (background light)) (:bold t :foreground "ForestGreen"))))
;;;  '(font-lock-constant-face ((((class color) (background light)) (:foreground "RoyalBlue"))))
;;;  '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Blue"))))
;;;  '(font-lock-function-name-face ((((class color) (background light)) (:bold t :foreground "Blue"))))
;;;  '(font-lock-builtin-face ((((class color) (background light)) (:bold t :foreground "RoyalBlue")))))

;;; Change Log:
;;; $Id: cparen.el,v 1.7 2001/03/27 20:54:54 rjs Exp $
;;;
;;; Version 1.0 (March 27th, 2001)
;;;  * Initial release.

;;; Todo:
;;;  * colouring support for Emacs Lisp and Common Lisp
;;;  * a function to deactivate the package (or a cparen minor mode;
;;;    the problem is that there is no simple complement to the
;;;    function `font-lock-add-keywords')
;;;  * `cparen-activate' doesn't affect the colouring of existing
;;;    buffers (workaround: use M-x normal-mode on them)
;;;  * user-customisable syntax; for example, make it easy for the
;;;    user to add a new let-like form

;;; Code:

(require 'font-lock)

;;;; Customisable faces

(defgroup cparen-highlighting-faces nil
  "Faces for parentheses in cparen mode."
  :group 'font-lock)

(defface cparen-around-define-face
  '((t (:bold t :foreground "Blue")))
  "Face used around a define or define-syntax expression."
  :group 'cparen-highlighting-faces)

(defface cparen-around-lambda-face
  '((t (:foreground "LightSeaGreen")))
  "Face used around a lambda expression."
  :group 'cparen-highlighting-faces)

(defface cparen-around-quote-face
  '((t (:foreground "SaddleBrown")))
  "Face used around a quote or quasiquote expression."
  :group 'cparen-highlighting-faces)

(defface cparen-around-vector-face
  '((t (:foreground "chocolate")))
  "Face used around vector constants."
  :group 'cparen-highlighting-faces)

(defface cparen-around-letdo-face
  '((t (:bold t :foreground "LightSeaGreen")))
  "Face used around a let or do expression."
  :group 'cparen-highlighting-faces)

(defface cparen-binding-list-face
  '((t (:bold t :foreground "ForestGreen")))
  "Face surrounding lists of bindings."
  :group 'cparen-highlighting-faces)

(defface cparen-binding-face
  '((t (:foreground "ForestGreen")))
  "Face surrounding individual bindings."
  :group 'cparen-highlighting-faces)

(defface cparen-around-conditional-face
  '((t (:bold t :foreground "RoyalBlue")))
  "Face used around a conditional expression (if/cond/case)."
  :group 'cparen-highlighting-faces)

(defface cparen-conditional-clause-face
  '((t (:foreground "RoyalBlue")))
  "Face surrounding clauses in conditionals."
  :group 'cparen-highlighting-faces)

(defface cparen-around-begin-face
  '((t (:foreground "maroon")))
  "Face used around a begin expression."
  :group 'cparen-highlighting-faces)

(defface cparen-around-set!-face
  '((t (:foreground "OrangeRed")))
  "Face used around a set! expression."
  :group 'cparen-highlighting-faces)

(defface cparen-around-andor-face
  '((t (:bold t :foreground "maroon")))
  "Face used around an \"and\" or \"or\" expression."
  :group 'cparen-highlighting-faces)

(defface cparen-around-syntax-rules-face
  '((t (:foreground "Magenta")))
  "Face used around a syntax-rules expression."
  :group 'cparen-highlighting-faces)

(defface cparen-normal-paren-face
  '((t (:foreground "grey50")))
  "Face for normal parentheses in Lisp code."
  :group 'cparen-highlighting-faces)

;;;; Variables

(defvar cparen-font-lock-keywords
  '(("(" (0 (cparen-opening-paren-face)))
    (")" (0 (cparen-closing-paren-face))))
  "Font lock keywords to enable cparen mode.
`cparen-activate' adds these using `font-lock-add-keywords'
for Scheme mode.")

(defvar cparen-mini-font-lock-keywords
  '(("[()]" 0 'cparen-normal-paren-face))
  "Font lock keywords to enable paren highlight.
`cparen-activate' adds these using `font-lock-add-keywords'
for Lisp mode.
By default, they highlight all parentheses with the face
`cparen-normal-paren-face'.")

;;;; Functions

(defsubst cparen-get-face-internal ()
  "Internal cparen face finder.
Finds the face of an opening parenthesis before point, moving point.
Should be wrapped in `condition-case' and `save-excursion'. Please use
`cparen-opening-paren-face' instead of this, if you can."
  (or (if (looking-at "\\(let\\|do\\>\\)") 'cparen-around-letdo-face)
      (if (looking-at "begin\\>") 'cparen-around-begin-face)
      (if (looking-at "\\(cond\\>\\|if\\>\\|case\\>\\)")
          'cparen-around-conditional-face)
      (if (looking-at "\\(and\\>\\|or\\>\\)") 'cparen-around-andor-face)
      (if (looking-at "set!\\>") 'cparen-around-set!-face)
      (if (looking-at "\\(quote\\>\\|quasiquote\\>\\)")
          'cparen-around-quote-face)
      (if (looking-at "define") 'cparen-around-define-face)
      (if (looking-at "lambda\\>") 'cparen-around-lambda-face)
      (if (looking-at "syntax-rules\\>") 'cparen-around-syntax-rules-face)
      (condition-case nil
          (or (save-excursion
                (backward-char 2)
                (if (looking-at "[`']") 'cparen-around-quote-face
                  (if (looking-at "#") 'cparen-around-vector-face)))
              (progn
                (backward-up-list 1)
                (backward-sexp 1)
                (or (if (looking-at "\\(let\\|lambda\\>\\|define\\|do\\>\\)")
                        'cparen-binding-list-face)
                    (if (looking-at "\\(case\\>\\|syntax-rules\\>\\)")
                        'cparen-normal-paren-face)
                    (let ((atparen (looking-at "[([{]")))
                      (backward-sexp 1)
                      (if (or (looking-at "do\\>")
                              (and (not atparen) (looking-at "let")))
                          'cparen-binding-list-face)))))
        (error nil))
      (progn
        (backward-up-list 1)
        (forward-char 1)
        (if (looking-at "\\(cond\\>\\|case\\>\\)")
            'cparen-conditional-clause-face))
      (if (looking-at "syntax-rules\\>") 'cparen-conditional-clause-face)
      (progn
        (backward-up-list 1)
        (backward-sexp 1)
        (if (looking-at "\\(let\\|do\\>\\)") 'cparen-binding-face))
      (let ((atparen (looking-at "[([{]")))
        (backward-sexp 1)
        (if (and (not atparen) (looking-at "let"))
            'cparen-binding-face))
      'cparen-normal-paren-face))

(defun cparen-opening-paren-face ()
  "Find the face of an opening parenthesis before point.
Uses `cparen-get-face-internal'."
  (condition-case nil
      (save-excursion
        (cparen-get-face-internal))
    (error 'cparen-normal-paren-face)))

(defun cparen-closing-paren-face ()
  "Find the face of a closing parenthesis before point.
Uses `cparen-get-face-internal'."
  (condition-case nil
      (save-excursion
        (backward-list 1)               ; move to corresponding
        (forward-char 1)                ; opening paren
        (cparen-get-face-internal))
    (error 'cparen-normal-paren-face)))

;;;; Installation

(defun cparen-activate ()
  "Activate coloured parentheses in Lisp modes.
You should also enable `font-lock-mode'.
Please note that there is currently no way to disable cparen, except
by turning `font-lock-mode' off completely.
Also, this function affects only buffers created after it was run;
use \\[normal-mode] after this to enable the colours in an existing
buffer."
  (interactive)
  (mapcar (lambda (mode)
            (font-lock-add-keywords mode cparen-font-lock-keywords))
          '(scheme-mode inferior-scheme-mode))
  (mapcar (lambda (mode)
            (font-lock-add-keywords mode cparen-mini-font-lock-keywords))
          '(emacs-lisp-mode lisp-mode lisp-interaction-mode
                            inferior-lisp-mode)))

(provide 'cparen)

;;; cparen.el ends here
