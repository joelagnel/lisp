;;; emacs-name.el --- emacs acronym expansions

;; Copyright (C) 1993, 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions, games
;; Created: 1993-12-02

;; $Id: emacs-name.el,v 1.3 2001/08/31 11:30:43 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

;; These have been contributed by people all over the network
;; (see the file etc/JOKES or emacs.names in the Emacs 19 distribution).
(defconst emacs-name-vector
  ["Each Mail A Continued Surprise"
   "Each Manual's Audience is Completely Stupified"
   "Easily Maintained with the Assistance of Chemical Solutions"
   "Easily Mangles, Aborts, Crashes and Stupifies"
   "Eating Memory And Cycle-Sucking"
   "Editing MACroS"
   "Edwardian Manifestation of All Colonial Sins"
   "Eenie-Meenie-Miney-Mo- Macros Are Completely Slow"
   "Egregious Managers Actively Court Stallman"
   "Eight Megabytes And Constantly Swapping"
   "Elephantine Memory Absolutely Considered Sine que non"
   "Eleven-thousand Monkeys Asynchronously Crank out these Slogans"
   "Elsewhere Maybe All Commands are Simple"
   "Elsewhere Maybe Alternative Civilizations Survive"
   "Elvis Masterminds All Computer Software"
   "Emacs Macht Alle Computer Schoen"
   "Emacs Made Almost Completely Screwed"
   "Emacs Makefiles Annihilate C-Shells"
   "Emacs Makers Are Crazy Sickos"
   "Emacs Makes A Computer Slow"
   "Emacs Makes All Computing Simple"
   "Emacs Makes no Allowances Considering its Stiff price"
   "Emacs Manuals Always Cause Senility"
   "Emacs Manuals Are Cryptic and Surreal"
   "Emacs Masquerades As Comfortable Shell"
   "Emacs May Alienate Clients and Supporters"
   "Emacs May Allow Customised Screwups"
   "Emacs May Annihilate Command Structures"
   "Emacs Means A Crappy Screen"
   "Emacs Monitors Aliens Cruising Skyward"
   "Emacs: My Alternative Computer Story"
   "Embarrassed Manual-Writer Accused of Communist Subversion"
   "Embarrassingly Mundane Advertising Cuts Sales"
   "Emetic Macros Assault Core and Segmentation"
   "Encourages Manic Actions Causing Suicide"
   "Energetic Merchants Always Cultivate Sales"
   "Epileptic MLisp Aggravates Compiler Seizures"
   "Equine Mammals Are Considerably Smaller"
   "Eradication of Memory Accomplished with Complete Simplicity"
   "Erasing Minds Allows Complete Submission"
   "Escape Meta Alt Control Shift"
   "Esoteric Malleability Always Considered Silly"
   "Even My Aunt Crashes the System"
   "Even a Master of Arts Comes Simpler"
   "Evenings, Mornings, And a Couple of Saturdays"
   "Eventually Munches All Computer Storage"
   "Ever Made A Control-key Setup?"
   "Every Male Adolescent Craves Sex"
   "Every Mode Accelerates Creation of Software"
   "Every Mode Acknowledges Customized Strokes"
   "Every Moron Assumes CCA is Superior"
   "Everyday Material Almost Compiled Successfully"
   "Excavating Mayan Architecture Comes Simpler"
   "Excellent Manuals Are Clearly Suppressed"
   "Exceptionally Mediocre Algorithm for Computer Scientists"
   "Exceptionally Mediocre Autocratic Control System"
   "Experience the Mildest Ad Campaign ever Seen"
   "Extended Macros Are Considered Superfluous"
   "Extensibility and Modifiability Aggravate Confirmed Simpletons"
   "Extraneous Macros And Commands Stink"
   "Generally Not Used (Except by Middle Aged Computer Scientists)"]
  "EMACS acronym expansions.")


(defmacro emacs-name-random (n)
  (if (and (numberp n)
           (string-lessp emacs-version "19"))
      (list '% '(abs (random)) n)
    (list 'random n)))

;;;###autoload
(defun emacs-name ()
  "Return a random expansion for the EMACS acronym as a string.
If called interactively, display the text in the minibuffer or in a
separate temporary buffer (if the output is too long for the minibuffer)."
  (interactive)
  (let ((name (aref emacs-name-vector
                    (emacs-name-random (length emacs-name-vector)))))
    (cond ((interactive-p)
           (let ((data (match-data))
                 (bufname "*Emacs Name*"))
             (if (string-match "\n" name)
                 (with-output-to-temp-buffer bufname (princ name))
               (delete-windows-on (get-buffer-create bufname))
               (message "%s" name))
             (store-match-data data))))
    name))

;;;###autoload
(defun emacs-name-set-frame-title (&optional frame)
  "Set the title of FRAME to a random expansion of the EMACS acronym.
If frame is not specified, use the currently-selected frame."
  (interactive)
  (let ((title (emacs-name)))
    (modify-frame-parameters (or frame (selected-frame))
                             (mapcar (function (lambda (key)
                                                 (cons key title)))
                                     '(name title icon-name)))))

(defun read-emacs-name (prompt &optional require-match)
  "Read an Emacs name from the minibuffer with completion.
Prompt with PROMPT.
If optional second arg is non-nil, require input to match a completion."
  (read-cookie prompt
               emacs-name-file
	       nil nil
	       require-match))

(provide 'emacs-name)

;;; emacs-name.el ends here
