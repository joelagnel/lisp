;;; highlight.el --- Simple highlighting commands.
;;
;; Filename: highlight.el
;; Description: Simple highlighting commands.
;; Author: David Brennan, brennan@hal.com, Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Copyright 1992, Dave Brennan
;; Created: Wed Oct 11 15:07:46 1995
;; Version: 21.0
;; Last-Updated: Fri Jan 13 15:01:10 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 500
;; URL: http://www.emacswiki.org/cgi-bin/wiki/highlight.el
;; Keywords: faces, help, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `faces', `faces+', `frame-fns', `misc-cmds',
;;   `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Simple highlighting commands.
;;
;;  Main new functions defined here:
;;
;;    `highlight', `highlight-regexp', `highlight-regexp-region',
;;    `highlight-region', `highlight-single-quotations',
;;    `mouse-face-each-line', `mouse-face-following-lines',
;;    `unhighlight-region'.
;;
;;  New user option (variable) defined here:
;;
;;    `max-highlight-w-o-warning'.
;;
;;  Other variable defined here: `highlight-last-regexp'.
;;
;;  Suggested binding:
;;
;;   (define-key ctl-x-map [(control ?y)] 'highlight)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2005/12/18 dadams
;;     Use minibuffer-prompt face.  Removed require of def-face-const.el.
;;     highlight-single-quotations: defsubst -> defun.
;; 2004/10/13 dadams
;;     Updated for Emacs 21: highlight-region: Bind
;;       inhibit-modification-hooks to non-nil to prevent Emacs 21
;;       font-lock from refontifying (removing highlighting)
;; 2004/10/12 dadams
;;     highlight-region: Use font-lock-after-fontify-buffer instead of
;;       lazy-lock-after-fontify-buffer.
;; 2004/03/16 dadams
;;     highlight-region: Prevent put-text-property from removing highlighting
;; 1996/04/26  dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/25  dadams
;;     1. Added highlight-single-quotations.
;;     2. highlight-regexp, highlight-regexp-region: Added new optional arg NTH.
;; 1996/04/25  dadams
;;     Added mouse-face-following-lines.
;; 1996/04/04  dadams
;;     1. highlight: Removed RAW-PREFIX, DISPLAY-MSGS args.  Made PREFIX optional.
;;        Set current-prefix-arg to nil so called fns don't use it as mouse-p.
;;     2. highlight-regexp, highlight-regexp-region: Added MOUSE-P arg.
;; 1996/02/27  dadams
;;     Added mouse-face-each-line.
;; 1996/02/26  dadams
;;     unhighlight-region: Added new arg MOUSE-P.
;; 1996/02/12  dadams
;;     highlight-region: Added optional arg MOUSE-P.
;; 1996/02/06  dadams
;;     Put variable-interactive property on appropriate user option vars.
;; 1996/02/01  dadams
;;     highlight: Just call subfunctions interactively.
;;     highlight-region, highlight-regexp, highlight-regexp-region: Use
;;       read-face-name
;; 1996/01/08  dadams
;;     highlight-regexp, highlight-regexp-region: message ->
;;       display-in-minibuffer.
;; 1995/11/09  dadams
;;     highlight-region: FACE arg is optional.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

(require 'frame-fns nil t) ;; (no error if not found): flash-ding
(require 'strings nil t) ;; (no error if not found): display-in-minibuffer
(require 'faces+ nil t) ;; (no error if not found): read-face-name
(require 'misc-cmds nil t) ;; (no error if not found): no-op

;;;;;;;;;;;;;;;;

;; This is defined in `faces.el', Emacs 22.  This definition is adapted to Emacs 20.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "Face for minibuffer prompts."
    :version "22.1" :group 'basic-faces))

;;;###autoload
(defvar max-highlight-w-o-warning 100000
  "*Max size of region to highlight without confirmation.")
(put 'max-highlight-w-o-warning 'variable-interactive
     "sMax number of chars in region to highlight without confirmation: ")

;;;###autoload
(defvar highlight-last-regexp nil "The last regexp highlighted.")


(defun highlight-single-quotations (&optional face)
  "Highlight single-quoted text (e.g commands and keys between `'s).
Optional arg FACE is the face (default: `highlight')."
  (interactive)
  (highlight-regexp "`\\([^']+\\)'" (or face 'highlight) nil nil 1))

;;;###autoload
(defun highlight (&optional prefix)
  "Highlight region, regexp (PREFIX +), or unhighlight region (PREFIX -).
PREFIX arg: 0,+ => highlight-regexp-region
              - => unhighlight-region
            nil => highlight-region"
  (interactive "P")
  (setq current-prefix-arg nil)         ; No mouse-p.
  (if prefix
      (if (natnump (prefix-numeric-value prefix))
          (call-interactively 'highlight-regexp-region)
        (save-excursion (call-interactively 'unhighlight-region)))
    (call-interactively 'highlight-region)
    (message "Highlighting region...done.  %s"
             (substitute-command-keys
              "`\\[negative-argument] \\[highlight]' to remove all \
highlighting in region."))))

;;;###autoload
(defun unhighlight-region (reg-start reg-end &optional where mouse-p)
  "Remove faces in region.
Required arguments:
 REG-START, REG-END: beginning and end of the region to unhighlight.
Optional 3rd argument WHERE:
 If a string, it is inserted in progress message.
 If otherwise non-nil, no progress message is displayed.
Optional 4th arg MOUSE-P non-nil => Use `mouse-face' property, not `face'.
Interactively, MOUSE-P is provided by the prefix arg."
  (interactive (list (region-beginning) (region-end) "in region "
                     current-prefix-arg))
  (setq where (or where ""))
  (when (stringp where) (message (format "Removing highlighting %s..." where)))
  (let ((read-only-p buffer-read-only)
        (modified-p (buffer-modified-p)))
    (setq buffer-read-only nil)
    (remove-text-properties reg-start reg-end
                            (if mouse-p '(mouse-face) '(face)))
    (setq buffer-read-only read-only-p)
    (set-buffer-modified-p modified-p))
  (when (stringp where)
    (message (format "Removing highlighting %s... done." where))))


;;; Function that does nothing and returns nil.  Any arguments are ignored.
;;; This is defined as a command in `misc-cmds.el'.
(unless (fboundp 'no-op) (defun no-op (&rest args)))

;;;###autoload
(defun highlight-region (start end &optional face mouse-p)
  "Highlight region between START and END with FACE (default: `highlight').
Optional arg MOUSE-P non-nil => Use `mouse-face' property, not `face'.
Interactively, MOUSE-P is provided by the prefix arg."
  (interactive
   (list (region-beginning) (region-end)
         (read-face-name "Use highlighting face: ")
         current-prefix-arg))
  (setq face (or face 'highlight))
  (let ((read-only buffer-read-only)
        (modified-p (buffer-modified-p))
        ;; Emacs 21: prevents font-lock from refontifying, removing highlighting
        (inhibit-modification-hooks t)
        ;; Otherwise, `put-text-property' calls this, which removes highlight.
        (font-lock-fontify-region-function 'no-op))
    (setq buffer-read-only nil)
    (put-text-property start end (if mouse-p 'mouse-face 'face) face)
    (setq buffer-read-only read-only)
    (set-buffer-modified-p modified-p))
  ;; Prevent font-lock from unhighlighting in Emacs 20
  (font-lock-after-fontify-buffer))

;;;###autoload
(defun highlight-regexp (regexp face &optional display-msgs mouse-p nth)
  "Highlight text after cursor that matches REGEXP, with face FACE.
Default face is `highlight'.
Optional 3rd arg DISPLAY-MSGS non-nil =>
         Display \"Highlighting...\" progress message.
Optional 4th arg MOUSE-P non-nil => `mouse-face' property, not `face'.
         Interactively, MOUSE-P is provided by the prefix arg.
Optional 5th arg NTH determines which regexp subgroup is highlighted.
         If NTH is nil or 0, the entire regexp is highlighted.
         Otherwise, the NTH regexp subgroup (\"\\\\(...\\\\)\"
         expression) is highlighted.  (Not available interactively.)"
  (interactive
   (list (read-string "Regexp to highlight after cursor: "
                      highlight-last-regexp)
         (read-face-name "Use highlighting face: ")
         'display-msgs
         current-prefix-arg))
  (let ((remove-msg (and display-msgs
                         (substitute-command-keys
                          "`\\[negative-argument] \\[highlight]' to remove \
all highlighting in region."))))
    (when display-msgs
      (if (fboundp 'display-in-minibuffer)
          (display-in-minibuffer 'new "Highlighting occurrences of `"
                                 (list 'minibuffer-prompt regexp)
                                 "' after cursor...")
        (message (concat "Highlighting occurrences of `" regexp
                         "' after cursor..."))))
    (highlight-regexp-region (point) (point-max) regexp face
                             (and display-msgs 'error-msgs-only)
                             mouse-p nth)
    (when display-msgs
      (if (fboundp 'display-in-minibuffer)
          (display-in-minibuffer 'more-event " done.  " remove-msg)
        (message (concat "Highlighting occurrences of `" regexp " done.  "
                         remove-msg)))))
  (setq highlight-last-regexp regexp))

;;;###autoload
(defun highlight-regexp-region (start end regexp face
                                      &optional display-msgs mouse-p nth)
  "Highlight regular expression REGEXP with FACE in region
from START to END.
Optional 5th arg DISPLAY-MSGS:
  t => Treat as interactive call in deciding to display all messages.
  non-nil & non-t => Display only error and warning messages.
Optional 6th arg MOUSE-P non-nil => `mouse-face' property, not `face'.
  Interactively, MOUSE-P is provided by the prefix arg.
Optional 7th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)"
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Regexp to highlight in region: " highlight-last-regexp)
         (read-face-name "Use highlighting face: ")
         t current-prefix-arg))         ; interactive-p => Display all msgs.
  (unless (stringp regexp)              ; Else re-search-forward gets an error
    (error "HIGHLIGHT-REGEXP-REGION: REGEXP arg is not a string: `%S'" regexp))
  (let ((reg-size (abs (- end start))))
    (when (and display-msgs
               (> reg-size max-highlight-w-o-warning)
               (not (progn
                      (and (fboundp 'flash-ding) ; In `frame-fns.el'
                           (flash-ding 'no-terminate-macros (selected-frame)))
                      (y-or-n-p (substitute-command-keys
                                 (format "Lots of highlighting slows \
things down.  Do you really want to highlight up to %d chars?  "
                                         reg-size))))))
      (error "OK, highlighting was cancelled")))
  (when (eq t display-msgs)
    (if (fboundp 'display-in-minibuffer)
        (display-in-minibuffer 'new "Highlighting occurrences of `"
                               (list 'minibuffer-prompt regexp) "' in region...")
      (message (concat "Highlighting occurrences of `" regexp
                       "' in region..."))))
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp end t)
      (highlight-region (match-beginning (or nth 0))
                        (match-end (or nth 0)) face)))
  (when (eq t display-msgs)
    (if (fboundp 'display-in-minibuffer)
        (display-in-minibuffer 'more-event " done.  " (substitute-command-keys
                                                       "`\\[negative-argument] \
\\[highlight]' to remove all highlighting in region."))
      (message (concat "Highlighting occurrences of `" regexp " done.  "
                       (substitute-command-keys
                        "`\\[negative-argument] \
\\[highlight]' to remove all highlighting in region.")))))
  (setq highlight-last-regexp regexp))

;;;###autoload
(defun mouse-face-following-lines ()
  "Put `mouse-face' on line of cursor and each following line."
  (let ((buffer-read-only nil))
    (save-excursion
      (while (not (eobp))
        (put-text-property (point) (progn (end-of-line) (point))
                           'mouse-face 'highlight)
        (forward-line 1)))))

;;;###autoload
(defun mouse-face-each-line ()
  "Put `mouse-face' on each line of buffer (restriction)."
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (put-text-property (point) (progn (end-of-line) (point))
                           'mouse-face 'highlight)
        (forward-line 1)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight.el ends here

