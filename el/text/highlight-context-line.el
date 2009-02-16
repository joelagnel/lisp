;;; highlight-context-line.el --- Highlight last visible line when scrolling
;; $Id: highlight-context-line.el,v 1.5 2003/01/27 21:42:28 ska Exp $
;; Copyright (C) 2002-2003 by Stefan Kamphausen, Claus Brunzema
;; Authors:
;;           Stefan Kamphausen <mail@skamphausen.de>
;;           Claus Brunzema    <mail@cbrunzema.de>
;; Keywords: services, user
(defvar highlight-context-version "1.5"
  "Version number of highlight-context-line")
;; This file is not part of XEmacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


;;; Commentary:
;; HighlightContextLine on the Web:
;; Main page:
;; http://www.skamphausen.de/software/skamacs/highlight-context.html
;; German intro:
;; http://www.skamphausen.de/xemacs/lisp/highlight-context.html

;; This piece of code highlights the last visible line when scrolling
;; full windows. It was inspired by the postscript viewer gv which
;; does a similar thing using a line across the screen
;; It honours the variable next-screen-context-lines.

;;; TODO:
;; - it would be nice to draw a horizontal line across the whole frame.
;;   The approach with using a face-background is just a workaround.
;; - The usual GNU Emacs compatibility stuff could be done here.

;;; ChangeLog
;; 1.5   January  2003
;;       - GNU Emacs compatibility due to request of
;;         Sami Salkosuo (http://members.fortunecity.com/salkosuo)
;;       - untabify file
;; 1.3   December 2002
;;       CVS, "official" webpage
;; Written in 2002, the year I finally started writing elisp code

;;; Code:
(require 'advice)

;;; User options ---------------------------------------------------------
(defgroup highlight-context-line nil
  "Highlight last visible line when scrolling."
  :tag "Highlight Context"
  :link '(url-link :tag "Home Page" 
                   "http://www.skamphausen.de/software/skamacs/")
  :link '(emacs-commentary-link
          :tag "Commentary in highlight-context-line.el" "highlight-context-line.el")
  :prefix "highlight-context-line-"
  :group 'basics)

(defcustom highlight-context-line-background "#bbbbbb"
  "*The background color to use to highlight the context line"
  :type 'string
  :group 'highlight-context-line)

(defvar highlight-context-line-face
  (copy-face 'default 'highlight-context-line-face)
  "Face for the highlighting of the context line.")

(defvar highlight-context-line-extent nil
  "Extent to use for highlighting.")

;; Compatibility
(if (featurep 'xemacs)
    (progn ;; XEmacs
      (defalias 'highlight-context-line-make-extent
        'make-extent)
      (defalias 'highlight-context-line-set-extent-property
        'set-extent-property)
      (defalias 'highlight-context-line-delete-extent
        'delete-extent)
      (defalias 'highlight-context-line-height
        'window-displayed-height))
  ;; GNU Emacs
  (defalias 'highlight-context-line-make-extent 'make-overlay)
  (defalias 'highlight-context-line-set-extent-property 'overlay-put)
  (defalias 'highlight-context-line-delete-extent 'delete-overlay)
  (defalias 'highlight-context-line-height 'window-height))

(when (not (fboundp 'point-at-bol))
    (progn
      (defun point-at-bol ()
        (save-excursion
          (beginning-of-line)
          (point)))
      (defun point-at-eol ()
        (save-excursion
          (end-of-line)
          (point)))))

;; Highlighting Code
(defun highlight-context-line-set-extent-face (extent face)
  (highlight-context-line-set-extent-property extent 'face face))

(defun highlight-context-line-do-highlight (direct)
  "Do the actual highlighting.
If DIRECT is positive we have been called from scroll-up, if negative
from scroll down."
  (highlight-context-line-unhighlight)
  (save-excursion
    (if (> direct 0)
        (move-to-window-line next-screen-context-lines)
      (move-to-window-line
       (- (highlight-context-line-height) next-screen-context-lines)))
    (setq highlight-context-line-extent
          (highlight-context-line-make-extent
           (point-at-bol)
           (if (not (eq (point-at-bol) (point-at-eol)))
               (point-at-eol)
             (forward-line (- direct))
             (point-at-eol))))
    (set-face-background highlight-context-line-face
                         highlight-context-line-background)
    (highlight-context-line-set-extent-face
     highlight-context-line-extent highlight-context-line-face)
    ;; prepare clean up
    (add-hook 'pre-command-hook
              'highlight-context-line-unhighlight)))

(defun highlight-context-line-unhighlight ()
  "Remove highlighting of the context line if any."
  (if highlight-context-line-extent
      (progn
        (highlight-context-line-delete-extent
         highlight-context-line-extent)
        (setq highlight-context-line-extent nil)
        (remove-hook 'pre-command-hook
                     'highlight-context-line-unhighlight))))

(defadvice scroll-up (around highlight-context-line-up activate)
  ad-do-it
  (if (not (car (ad-get-args 0)))
      (highlight-context-line-do-highlight 1)))

(defadvice scroll-down (around highlight-context-line-down activate)
    ad-do-it
    (if (not (car (ad-get-args 0)))
        (highlight-context-line-do-highlight -1)))

(provide 'highlight-context-line)
;; highlight-context-line.el ends here