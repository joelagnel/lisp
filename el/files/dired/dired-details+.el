;;; dired-details+.el --- Enhancements to library `dired-details+.el'.
;; 
;; Filename: dired-details+.el
;; Description: Enhancements to library `dired-details+.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2006, Drew Adams, all rights reserved.
;; Created: Tue Dec 20 13:33:01 2005
;; Version: 
;; Last-Updated: Sun Aug 20 14:43:01 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 140
;; URL: http://www.emacswiki.org/cgi-bin/wiki/dired-details+.el
;; Keywords: dired, frames
;; Compatibility: GNU Emacs 20, GNU Emacs 22
;; 
;; Features that might be required by this library:
;;
;;   `autofit-frame', `avoid', `dired-details', `fit-frame',
;;   `frame-cmds', `frame-fns', `misc-fns', `strings', `thingatpt',
;;   `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This enhances the functionality of library `dired-details.el'.
;;
;;  1. It shrink-wraps Dired's frame whenever you show or hide
;;     details.  For this enhancement, you will need library
;;     `autofit-frame.el'.
;;
;;  2. It advises `dired-byte-compile', `dired-compress',
;;     `dired-create-files' and `dired-create-directory', so that
;;     whenever you create new files or directories or rename them the
;;     hide/show overlays are updated accordingly.
;;
;;  3. It adds user option `dired-details-propagate-flag' which, if
;;     non-nil, propagates the last state you chose to the next Dired
;;     buffer you open.
;;
;;  4. It binds both `)' and `(' to `dired-details-toggle'.
;;
;;  Perhaps #2 corresponds to this TO-DO item in `dired-details.el':
;;
;;    * add a hook for dired-add-file to hide new entries as necessary
;;
;;
;;  ***** NOTE: The following function defined in `dired-details.el'
;;              has been REDEFINED HERE:
;;
;;  `dired-details-activate' - If `dired-details-propagate-flag' is
;;                             non-nil, then use the last state.
;;
;;  ***** NOTE: The following functions defined in `dired-aux.el' have
;;              been REDEFINED HERE (advised only):
;;
;;  `dired-byte-compile', `dired-compress', `dired-create-files',
;;  `dired-create-directory' - Update overlays.
;;
;;
;;  I have submitted these enhancements to Rob Giardina, the author of
;;  `dired-details.el', for inclusion in that library.  When they (or
;;  similar) are added to that library, I'll remove this library.
;;
;;  Put this in your initialization file (~/.emacs):
;;
;;   (require 'dired-details+)
;;
;;  I also recommend customizing `dired-details-hidden-string' to use
;;  the value "" instead of the default "[...]" - less wasted space.
;;
;;  Note: This library also calls `dired-details-install', activating
;;  show/hide and binding keys `(' and `)'.
;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2006/02/02 dadams
;;     Bind both ) and ( to dired-details-toggle.
;; 2006/01/02 dadams
;;     Advised dired-byte-compile and dired-compress.
;; 2006/01/01 dadams
;;     Advised dired-create-directory.
;; 2005/12/30 dadams
;;     Advised dired-create-files.
;;     dired-details-(show|hide): Only fit frame if it's showing Dired.
;; 2005/12/26 dadams
;;     Updated groups.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;; Do this `defcustom' first, before we load `dired-details', so we
;;; don't pick up the `defcustom' there.  The default value here is
;;; the empty string, so the overlay doesn't give a false impression
;;; of the current column number.  This is important for frame fitting
;;; (see library `fit-frame.el', required by `autofit-frame.el').
(defcustom dired-details-hidden-string ""
  "*This string will be shown in place of file details and symbolic links."
  :group 'dired-details :group 'dired
  :type 'string)

(require 'dired-details nil t) ;; (no error if not found): dired-details-hide,
                               ;; dired-details-install, dired-details-show
(require 'autofit-frame nil t) ;; (no error if not found): fit-frame-if-one-window

;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcustom dired-details-propagate-flag t
  "Non-nil means next Dired buffer should be displayed the same.
The last `dired-details-state' value set is used by the next Dired
buffer created."
  :type 'boolean)

(defvar dired-details-last-state nil
  "Last `dired-details-state' value.
This is changed each time any Dired buffer's state changes.")




;;; REPLACE ORIGINAL in `dired-details.el'.
;;;
;;; Use last hide/show state, if `dired-details-propagate-flag'.
;;; 
(defun dired-details-activate ()
  "Set up dired-details in the current dired buffer.
Called by `dired-after-readin-hook' on initial display and when a
subdirectory is inserted (with `i').  The state is chosen as follows:
If the state is already established here, leave it alone.
If `dired-details-propagate-flag' is non-nil, then use the last state.
Otherwise, use the default state, as determined by
  `dired-details-initially-hide'."
  (cond (dired-details-state            ; State chosen in this buffer; respect it.
         (when (eq 'hidden dired-details-state)
           (dired-details-hide)))
        ((and dired-details-propagate-flag ; Inherit state from previous.
              dired-details-last-state)
         (when (eq 'hidden dired-details-last-state)
           (dired-details-hide)))
        (t
         ;;otherwise, use the default state
         (when dired-details-initially-hide
           (dired-details-hide)))))

;; The test (get-buffer-window (current-buffer)) is to make sure that
;; Dired is already displayed.  If not, the selected frame is not what
;; we want to fit.
(when (fboundp 'dired-details-show)
  (dired-details-install)
  ;; Override bindings in `dired-details-install'.
  (define-key dired-mode-map "(" 'dired-details-toggle)
  (define-key dired-mode-map ")" 'dired-details-toggle)
  (defadvice dired-details-show (after fit-dired-frame activate)
    "Save `dired-details-last-state'.  Fit Dired frame if `one-window-p'."
    (setq dired-details-last-state dired-details-state)
    (when (and (get-buffer-window (current-buffer))
               (fboundp 'fit-frame-if-one-window))
      (fit-frame-if-one-window)))

  (defadvice dired-details-hide (after fit-dired-frame activate)
    "Save `dired-details-last-state'.  Fit Dired frame if `one-window-p'."
    (setq dired-details-last-state dired-details-state)
    (when (and (get-buffer-window (current-buffer))
               (fboundp 'fit-frame-if-one-window))
      (fit-frame-if-one-window))))


;; `dired-create-files' is defined in `dired-aux.el'.
;;;###autoload
(defadvice dired-create-files (after dired-details-activate activate)
  "Set up Dired details."
  (dired-details-delete-overlays)
  (dired-details-activate))

;; `dired-create-directory' is defined in `dired-aux.el'.
;;;###autoload
(defadvice dired-create-directory (after dired-details-activate activate)
  "Set up Dired details."
  (dired-details-delete-overlays)
  (dired-details-activate))

;; `dired-byte-compile' is defined in `dired-aux.el'.
;;;###autoload
(defadvice dired-byte-compile (after dired-details-activate activate)
  "Set up Dired details."
  (dired-details-delete-overlays)
  (dired-details-activate))

;; `dired-compress' is defined in `dired-aux.el'.
;;;###autoload
(defadvice dired-compress (after dired-details-activate activate)
  "Set up Dired details."
  (dired-details-delete-overlays)
  (dired-details-activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired-details+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-details+.el ends here
