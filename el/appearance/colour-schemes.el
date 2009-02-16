;;; colour-schemes.el -- try other groups of colours
;;; Time-stamp: <2005-01-18 19:11:12 jcgs>

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

(require 'cl)

(defstruct colour-scheme
  background
  foreground
  cursor)

(makunbound 'colour-schemes)

(defvar colour-schemes
  `(("Vellum" . ,(make-colour-scheme :background "Wheat"
				     :foreground "Brown"
				     :cursor "Red"))
    ("Green-screen" . ,(make-colour-scheme :background "Black"
					   :foreground "Green"
					   :cursor "Red"))
    ("Amber-screen" . ,(make-colour-scheme :background "Black"
					   :foreground "Orange"
					   :cursor "Blue"))
    ("Paper" . ,(make-colour-scheme :background "White"
				    :foreground "Black"
				    :cursor "Blue"))
    ("Wedgewood" . ,(make-colour-scheme :background "DarkTurquoise"
				    :foreground "White"
				    :cursor "Blue"))
    )
  "Some colour schemes, as alist by name")

(defvar colour-scheme-history-hack nil
  "A variable for tmm/handsfree-style choosing of colour schemes")

;;;###autoload
(defun set-colour-scheme (colour-scheme)
  "Switch to COLOUR-SCHEME."
  (interactive
   (let ((completion-ignore-case t))
     (setq colour-scheme-history-hack (mapcar 'car colour-schemes))
     (list
      (completing-read "Colour scheme: "
		       colour-schemes
		       nil ; predicate
		       t ; require-match
		       nil ; initial-input
		       'colour-scheme-history-hack ; history var
		       ))))
  (let ((this-colour-scheme (cdr (assoc colour-scheme colour-schemes))))
    (set-face-foreground
     'default
     (colour-scheme-foreground this-colour-scheme))
    (set-background-color
     (colour-scheme-background this-colour-scheme))
    (set-cursor-color
     (colour-scheme-cursor this-colour-scheme))))

;; (set-colour-scheme "Vellum")

;;; end of colour-schemes.el
