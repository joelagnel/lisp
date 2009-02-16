;;; zoom-frm.el --- Commands to zoom frame font size.
;;
;; Filename: zoom-frm.el
;; Description: Commands to zoom frame font size.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2006, Drew Adams, all rights reserved.
;; Created: Fri Jan 07 10:24:35 2005
;; Version: 20
;; Last-Updated: Fri Jan 13 15:17:48 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 157
;; URL: http://www.emacswiki.org/cgi-bin/wiki/zoom-frm.el
;; Keywords: frames, extensions, convenience
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-cmds', `frame-fns', `misc-fns', `strings',
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Commands to zoom frame font size.
;;
;;  A few commands are provided for zooming a frame, so that its font
;;  becomes larger or smaller.
;;
;;  Note on saving changes made with the commands defined here:
;;
;;    Some of the commands defined here change frame properties.  You
;;    can save any changes you have made, by using Customize.  To
;;    visit a Customize buffer of all unsaved changes you have made,
;;    use command `customize-customized'.
;;
;;    Frame parameter changes, such as background color, can be saved
;;    for future use by all frames or all frames of a certain kind.
;;    For that, you must change the frame parameters of the
;;    correponding frame-alist variable.
;;
;;    There is no single variable for saving changes to parameters of
;;    the current frame.  Instead, there are several different
;;    frame-alist variables, which you can use to define different
;;    kinds of frames.  These include: `default-frame-alist',
;;    `initial-frame-alist', and `special-display-frame-alist'.  The
;;    complete list of such frame alist variables is available using
;;    function `frame-alist-var-names', defined in library
;;    `frame-cmds.el'.
;;
;;    Example: Suppose you change the background color of a frame and
;;    want to make that the default background color for new frames in
;;    the future.  You will need to update the value of variable
;;    `default-frame-alist' to use the `background-color' parameter
;;    setting of the changed frame.
;;
;;    You can easily copy one or all parameter values from any given
;;    frame to any frame alist (such as `default-frame-alist'), by
;;    using the commands `set-frame-alist-parameter-from-frame' and
;;    `set-all-frame-alist-parameters-from-frame'.  Those commands are
;;    defined in library `frame-cmds.el'.
;;
;;
;;  Commands defined here:
;;
;;    `toggle-zoom-frame', `zoom-frm-in', `zoom-frm-out',
;;    `zoom-frm-unzoom'.
;;
;;
;;  User options (variables) defined here:
;;
;;    `frame-zoom-font-difference'.
;;
;;
;;  Put this in your init file (`~/.emacs'): (require 'zoom-frm)
;;
;;  Suggested key bindings:
;;
;;  (define-key global-map [S-mouse-1] 'zoom-frm-in)
;;  (define-key global-map [C-S-mouse-1] 'zoom-frm-out)
;;  ;; Get rid of `mouse-set-font':
;;  (define-key global-map [S-down-mouse-1] nil)
;;
;;
;;  See also these files for other frame commands:
;;
;;     `autofit-frame.el' - Automatically fit each frame to its
;;                          selected window.  Uses `fit-frame.el'.
;;
;;     `fit-frame.el'     - 1) Fit a frame to its selected window.
;;                          2) Incrementally resize a frame.
;;
;;     `doremi-frm.el'    - Incrementally adjust frame properties
;;                          using arrow keys and/or mouse wheel.
;;
;;     `frame-cmds.el'    - Miscellaneous frame and window commands.
;;
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/01/07 dadams
;;      Added :link for sending bug report.
;; 2006/01/06 dadams
;;     frame-zoom-font-difference: Changed :group to Frame-Commands. Added :link.
;; 2005/01/18 dadams
;;     Changed default value of frame-zoom-font-difference.
;;     Added Note on saving changes.
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

(require 'frame-cmds) ;; enlarge-font

;;;;;;;;;;;;;;;;;;;;;;;;




;;; USER OPTIONS (VARIABLES) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom frame-zoom-font-difference 1
  "*Number of points to change the frame font size when zooming
using commands `zoom-frm-in' and `zoom-frm-out'.
The absolute value of this must be less than the current font size,
since the new font size cannot be less than 1 point."
  :type 'integer :group 'Frame-Commands ; Defined in `frame-cmds.el'.
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
zoom-frm.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/zoom-frm.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/SetFonts#ChangingFontSize")
  :link '(emacs-commentary-link :tag "Commentary" "zoom-frm")
  )




;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun zoom-frm-in (&optional frame flip)
  "Zoom FRAME in by `frame-zoom-font-difference', making text larger.
If `frame-zoom-font-difference' is negative, make text smaller.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, then make text smaller.
This is equal but opposite to `zoom-frm-out'."
  (interactive (list (selected-frame) current-prefix-arg))
  (setq frame (or frame (selected-frame)))
  (let ((zoom-factor (frame-parameter frame 'zoomed))
        (increment (if flip (- frame-zoom-font-difference) frame-zoom-font-difference)))
    (unless zoom-factor (setq zoom-factor 0))
    (setq zoom-factor (+ zoom-factor increment))
    (enlarge-font increment frame)
    (modify-frame-parameters frame (list (cons 'zoomed zoom-factor)))))

;;;###autoload
(defun zoom-frm-out (&optional frame flip)
  "Zoom FRAME out by `frame-zoom-font-difference'.
If `frame-zoom-font-difference' is negative, make text larger.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, then make text larger.
This is equal but opposite to `zoom-frm-in'."
  (interactive (list (selected-frame) current-prefix-arg))
  (setq frame (or frame (selected-frame)))
  (let ((frame-zoom-font-difference (- frame-zoom-font-difference)))
    (zoom-frm-in frame flip)))

;;;###autoload
(defun zoom-frm-unzoom (&optional frame)
  "Cancel zoom of FRAME."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (let ((zoom-factor (frame-parameter frame 'zoomed)))
    (if (not zoom-factor)
        (error "Frame is not zoomed")
      (enlarge-font (- zoom-factor) frame)
      (modify-frame-parameters frame '((zoomed))))))

;;;###autoload
(defun toggle-zoom-frame (&optional frame)
  "Alternately zoom/unzoom FRAME by `frame-zoom-font-difference'."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (if (frame-parameter frame 'zoomed)
      (zoom-frm-unzoom frame)
    (zoom-frm-in frame)))



;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'zoom-frm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zoom-frm.el ends here
