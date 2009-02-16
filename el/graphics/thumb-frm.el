;;; thumb-frm.el --- Commands for thumbnail frames.
;;
;; Filename: thumb-frm.el
;; Description: Commands for thumbnail frames.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2006, Drew Adams, all rights reserved.
;; Created: Fri Dec 10 16:44:55 2004
;; Version: 21.0
;; Last-Updated: Sat Aug 12 00:43:54 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 1174
;; URL: http://www.emacswiki.org/cgi-bin/wiki/thumb-frm.el
;; Keywords: frame, icon
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `doremi', `doremi-frm', `eyedropper', `faces',
;;   `faces+', `frame-cmds', `frame-fns', `hexrgb', `misc-fns',
;;   `mwheel', `ring', `ring+', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Shrink frames to a thumbnail size and restore them again.
;;
;;  The main command here is `thumbify-other-frames', alias
;;  `fisheye'. It shrinks all frames except the selected frame to a
;;  thumbnail size. The thumbnail frames are stacked from top to
;;  bottom, left to right on your display. This provides a kind of
;;  "fisheye" view of the frames you are using. Command
;;  `dethumbify-all-frames' restores all thumbnails to full size.
;;
;;  You can also thumbify or dethumbify any individual frame, using
;;  commands `toggle-thumbnail-frame', `thumbify-frame', and
;;  `dethumbify-frame'. You might want to thumbify the frame of a
;;  progressive output buffer, for instance, just to keep an eye on
;;  the output as it is produced.
;;
;;  Command `stack-thumbnail-frames' neatly stacks all of the
;;  thumbnail frames along the display edge.  You can use it at any
;;  time, and it is called automatically by `fisheye'.  Which display
;;  edge to stack along (left, right, top, bottom), and which
;;  direction (up, down, to-left or to-right), is determined by
;;  `thumbfrm-stack-display-edge'.  The stacking order is determined
;;  by option `thumbfrm-sort-function'.  You can turn sorting on and
;;  off with command `toggle-sort-thumbnail-frame-stack'.
;;
;;  By default, this library provides thumbifying and dethumbifying as
;;  a *replacement* for iconifying and deiconifying.  Loading the
;;  library changes the standard commands `iconify-frame' and
;;  `iconify-or-deiconify-frame' so that they use thumbnails instead
;;  of icons whenever option `thumbify-instead-of-iconify-flag' is
;;  non-nil.  To prevent this thumbnail behavior, you can set
;;  `thumbify-instead-of-iconify-flag' to nil in your init file.
;;  Alternatively, you can deactivate (`ad-deactivate') the advice
;;  imposed here on these functions to give them back their original
;;  behavior.
;;
;;  The original behavior of commands `iconify-frame' and
;;  `iconify-or-deiconify-frame' is available using commands
;;  `really-iconify-frame' and `really-iconify-or-deiconify-frame'.
;;  In particular, these commands can be used to iconify, even if you
;;  bind [iconify-frame] in `special-event-map' (see below).
;;
;;  You can iconify or deiconify all thumbnail frames (that is, only
;;  the thumbnail frames), to get them out of the way and bring them
;;  back.  Use commands `iconify-thumbnail-frames' and
;;  `deiconify-thumbnail-frames' to do this.
;;
;;  Emacs built-in function `raise-frame' is redefined here to also
;;  dethumbify.  The original behavior of `raise-frame' is available
;;  in new function `only-raise-frame'.
;;
;;  You can cycle among the visible frames in two ways, applying
;;  `fisheye' to each in turn.  The first way is using commands
;;  `fisheye-previous-frame' and `fisheye-next-frame' (which you can
;;  bind to, for instance, `C-M-prior' and `C-m-next').  The second
;;  way is using command `doremi-thumbnail-frames' and the arrow keys
;;  or mouse wheel.
;;
;;  A more comprehensive, lower-level way of substituting thumbifying
;;  for iconifying is to do the following in your init file:
;;
;;    (define-key special-event-map [iconify-frame]
;;                'thumbify-frame-upon-event)
;;
;;  In effect, this thumbifies any frame as soon as it is iconified,
;;  no matter how it was iconified.  In particular, this will let you
;;  use the window-manager "minimize" frame button (usually at the
;;  upper left or right frame corner) to thumbify.
;;
;;  If you do that, be aware that `thumbify-instead-of-iconify-flag'
;;  will no longer have any effect: Emacs will *always* thumbify
;;  instead of iconify (except for functions `really-iconify-*frame',
;;  which are designed to counter this).  If you try this behavior and
;;  then wish to cancel it, to once again allow iconification, use
;;  this code:
;;
;;  In Emacs 20 or prior:
;;
;;    (define-key special-event-map [iconify-frame] 'ignore-event)
;;
;;  In Emacs 21 or later:
;;
;;    (define-key special-event-map [iconify-frame] 'ignore)
;;
;;
;;  Other user options (variables) not mentioned above are these:
;;
;;    `frame-thumbnail-font-difference' - Sets size of thumb frames.
;;    `rename-frame-when-thumbify-flag' - Rename frame to buffer.
;;    `thumb-frm-frame-parameters' - Sets thumb frame parameters.
;;    `thumbfrm-stack-display-edge' - Sets display edge for stacking.
;;    `window-mgr-title-bar-pixel-width' - Thickness of title bar.
;;
;;
;;  WARNING:
;;
;;    Thumbnail frames are *FULLY FUNCTIONAL*.  In particular, their
;;    buffers are *NOT* read-only in any way.  You can edit their
;;    buffers normally, even if you can't see what you're doing
;;    :-).  You can also scroll and search their buffers, of course.
;;
;;
;;  Functions defined here:
;;
;;    `cull-thumbnail-frames', `deiconify-thumbnail-frames',
;;    `dethumbify-all-frames', `dethumbify-frame',
;;    `doremi-thumbnail-frames', `fisheye', `fisheye-next-frame',
;;    `fisheye-previous-frame', `iconify-thumbnail-frames',
;;    `only-raise-frame', `next-stack-position',
;;    `really-iconify-frame', `really-iconify-or-deiconify-frame',
;;    `stack-thumbnail-frames', `thumbfrm-sort-by-name',
;;    `thumbfrm-sort-by-window-id', `thumbify-frame',
;;    `thumbnail-frame-p' `thumbify-frame-upon-event',
;;    `thumbify-other-frames', `toggle-sort-thumbnail-frame-stack',
;;    `toggle-thumbnail-frame'.
;;
;;
;;  User options (variables) defined here:
;;
;;    `frame-thumbnail-font-difference',
;;    `rename-frame-when-thumbify-flag'
;;    `thumbify-instead-of-iconify-flag', `thumbfrm-frame-parameters',
;;    `thumbfrm-sort-function', `thumbfrm-stack-display-edge',
;;    `window-mgr-title-bar-pixel-width'.
;;
;;
;;  Internal variable defined here:
;;
;;    `thumbnail-frames', `thumbfrm-last-row-show',
;;    `thumbfrm-last-sort-function'.
;;
;;
;;  ***** NOTE: The following EMACS functions have been REDEFINED HERE:
;;
;;  `iconify-frame' - Thumbify if `thumbify-instead-of-iconify-flag'.
;;  `iconify-or-deiconify-frame' - Similar to `iconify-frame', plus
;;                                 dethumbify if already a thumbnail.
;;  `raise-frame' - Dethumbify also, if a thumbnail.
;;
;;
;;  Put this in your init file (`~/.emacs'): (require 'thumb-frm)
;;
;;  Suggested key bindings:
;;
;;   (global-set-key [(shift mouse-3)]        'toggle-thumbnail-frame)
;;   (global-set-key [(shift control mouse-3)]'thumbify-other-frames)
;;   (global-set-key [(shift control ?z)]     'thumbify-other-frames)
;;   (global-set-key [(shift control prior)]  'fisheye-previous-frame)
;;   (global-set-key [(shift control next)]   'fisheye-next-frame)
;;   (global-set-key [(control meta ?z)]
;;                   'really-iconify-or-deiconify-frame)
;;
;;   ;; Make the window-manager "minimize" button thumbify instead.
;;   (define-key special-event-map [iconify-frame]
;;               'thumbify-frame-upon-event)
;;
;;   ;; Add `doremi-thumbnail-frames' to the Do Re Mi commands -
;;   ;; see library `doremi-frm.el'.
;;   (unless (fboundp 'doremi-prefix)
;;     (defalias 'doremi-prefix (make-sparse-keymap))
;;     (defvar doremi-map (symbol-function 'doremi-prefix)
;;       "Keymap for Do Re Mi commands."))
;;   (define-key global-map "\C-xt"  'doremi-prefix)
;;   (define-key global-map "\C-xte" 'doremi-thumbnail-frames) ; "Eye"
;;
;;   Keep in mind also that if `thumbify-instead-of-iconify-flag' is
;;   non-nil, keys bound to (de-)iconifying commands, such as `C-z',
;;   will instead (de-)thumbify.
;;
;;  See also these libraries for other frame commands:
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
;;     `zoom-frm.el'      - Zoom a frame, so that its font becomes
;;                          larger or smaller.
;;
;;  Acknowledgements (thanks):
;;    Michael Cadilhac [Michael.Cadilhac-@t-lrde.epita.fr] for a tip
;;      on using `discard-input' to effectively nullify
;;      `special-event-map' bindings (used in
;;      `really-iconify-[or-deiconify]frame').
;;
;;  TO DO?:
;;
;;     Make thumbnail frames read-only, to prevent inadvertent
;;     changes.  How to do so? Could make all buffers in frame's
;;     buffer-list r-o, but that would affect the buffer on
;;     non-thumbnail frames too.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/08/07 dadams
;;      dethumbify-frame: raise-frame and give it focus.  Reported Emacs bug:
;;                        sends front Windows app to the bottom of the stack.
;; 2006/01/07 dadams
;;      Added :link for sending bug report.
;; 2006/01/06 dadams
;;      Added :link.
;; 2005/12/03 dadams
;;     Changed suggested bindings.
;; 2005/07/25 dadams
;;     Added :prefix to defgroup.
;; 2005/06/26 dadams
;;     raise-frame: Cleaned up doc string.
;; 2005/05/15 dadams
;;     Renamed: minibuffer-frame to 1on1-minibuffer-frame.
;; 2005/05/09 dadams
;;     Added: thumbnail-frame-p.
;; 2005/05/06 dadams
;;     Added: thumbfrm-next-stack-xoffset and thumbfrm-next-stack-yoffset.  Use them in
;;            thumbify-frame and stack-thumbnail-frames to 1) return thumbified frame to
;;            its stacked position and 2) ensure that thumbifying uses next stack position.
;;     Added: thumbify-frame-upon-event.
;; 2005/04/19 dadams
;;     Protected fset 'only-raise-frame with fboundp.
;; 2005/01/26 dadams
;;     Added: fisheye-next-frame, fisheye-previous-frame.
;;     thumbfrm-stack-display-edge: use right+down as default value.
;; 2005/01/08 dadams
;;     Renamed doremi-grow-font to enlarge-font.  It is now defined in frame-cmds.el.
;; 2005/01/04 dadams
;;     Added rename-frame-when-thumbify-flag; use it in thumbify-frame.
;; 2004/12/25 dadams
;;     stack-thumbnail-frames: Allow any display edge in any direction.
;;     Added: next-stack-position, thumbfrm-frame-parameters,
;;            thumbfrm-last-row-show, thumbfrm-sort-by-window-id.
;;     thumbify-frame: Use thumbfrm-frame-parameters.
;; 2004/12/24 dadams
;;     Added: thumbfrm-stack-display-edge.
;; 2004/12/23 dadams
;;     stack-thumbnail-frames: Raise each frame, so title bars show.
;;         Sort frames.  Account for window-mgr title bar.  Use copy-sequence,
;;         so no side effects.
;;     Use defcustom for user vars.  Added defgroup thumbfrm.
;;     Added: thumbfrm-sort-function, thumbfrm-last-sort-function,
;;            thumbfrm-sort-by-name, toggle-sort-thumbnail-frame-stack,
;;            window-mgr-title-bar-pixel-width.
;;     cull-thumbnail-frames: Remove invisible and iconified frames also.
;; 2004/12/21 dadams
;;     frame-thumbnail-font-difference: Changed defvar to defcustom.
;;     Added: iconify-frame, iconify-or-deiconify-frame, really-iconify-frame,
;;        really-iconify-or-deiconify-frame, thumbify-instead-of-iconify-flag.
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

(require 'doremi-frm) ;; doremi
(require 'frame-cmds) ;; enlarge-font, rename-non-minibuffer-frame
(eval-when-compile (require 'cl)) ;; delete-if-not, nset-difference
                                  ;; (plus, for Emacs 20: dolist
                                  ;;  and, for Emacs <20: when, unless)

;;;;;;;;;;;;;;;;;;;;;



;;; USER OPTIONS ;;;;;;;;;;;;;;;;

(defgroup Thumbnail-Frames nil
  "Commands for thumbnail frames"
  :prefix "thumbfrm-" :group 'frames :group 'convenience
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
thumb-frm.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/thumb-frm.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/FisheyeWithThumbs")
  :link '(emacs-commentary-link :tag "Commentary" "thumb-frm")
  )

(defcustom frame-thumbnail-font-difference 10
  "*Number of points for `thumbify-frame' to decrease the frame font.
This must be less than the current font size, since the new font size
cannot be less than 1 point.

Note that this can be a negative integer, in which case thumbifying
actually increases the font and frame size, instead of decreasing them."
  :type 'integer :group 'Thumbnail-Frames)

(defcustom thumbify-instead-of-iconify-flag t
  "*Non-nil means thumbify frames instead of iconifying them."
  :type 'boolean :group 'Thumbnail-Frames)

(defcustom rename-frame-when-thumbify-flag t
  "*Non-nil means frames are renamed when thumbified.
The new name is the name of the current buffer."
  :type 'boolean :group 'Thumbnail-Frames)

(defcustom thumbfrm-stack-display-edge 'right+down
  "*Display edge to stack thumbnail frames along.
Possible values are symbols named EDGE+DIRECTION,
where EDGE is one of `left', `right', `top', and `bottom',
and DIRECTION is one of `up', `down', `to-left', and `to-right'.

For example, value `right+down' means to arrange thumbnail frames
along the right edge from top to bottom."
  :type
  '(choice
    (const
     :tag "Arrange thumbnail frames along display left, downward" left+down)
    (const
     :tag "Arrange thumbnail frames along display left, upward" left+up)
    (const
     :tag "Arrange thumbnail frames along display right, downward" right+down)
    (const
     :tag "Arrange thumbnail frames along display right, upward" right+up)
    (const
     :tag "Arrange thumbnail frames along display top, toward the right" top+to-right)
    (const
     :tag "Arrange thumbnail frames along display top, toward the left" top+to-left)
    (const
     :tag "Arrange thumbnail frames along display bottom, toward the right" bottom+to-right)
    (const
     :tag "Arrange thumbnail frames along display bottom, toward the left" bottom+to-left))
  :group 'Thumbnail-Frames)

(defcustom thumbfrm-frame-parameters
  (if (< emacs-major-version 21)
      '((menu-bar-lines . 0) (tool-bar-lines . 0))
    ;; Emacs 21 does not shrink scroll bars when font shrinks.
    '((menu-bar-lines . 0) (tool-bar-lines . 0)
      (vertical-scroll-bars) (horizontal-scroll-bars)))
  "Frame parameters of thumbnail frames.
Use this to remove things like the menu-bar from thumbnail frames."
  :type '(repeat (cons symbol sexp))
  :group 'Thumbnail-Frames)

(defcustom thumbfrm-sort-function 'thumbfrm-sort-by-name
  "*Function to use for sorting the stacked thumbnail frames.
If nil, then no sorting is done.
Set this to `thumbfrm-sort-by-name' for alphabetical order.

The function should take two frame specifications as arguments, where
a frame spec has the form of an item in list `thumbnail-frames'.  It
should return non-nil if the frame of the first spec comes before
that of the second.  See, for example, `thumbfrm-sort-by-name' and
`thumbfrm-sort-by-window-id'.

Use `toggle-sort-thumbnail-frame-stack' to turn sorting on and off."
  :type '(choice (const :tag "No sorting" nil)
		 (const :tag "Sort by name" thumbfrm-sort-by-name)
		 (const :tag "Sort by name" thumbfrm-sort-by-window-id)
		 (function :tag "Another function"))
  :group 'Thumbnail-Frames)

(unless (boundp 'window-mgr-title-bar-pixel-width) ; Defined in `frame-cmds.el'.
  (defcustom window-mgr-title-bar-pixel-width 30
    "*Width of frame title bar provided by window manager, in pixels."
    :type 'integer :group 'Thumbnail-Frames)) ; Normally, the :group is `Frame-Commands'.




;;; INTERNAL VARIABLES ;;;;;;;;;;

(defvar thumbnail-frames nil
  "An alist of frames currently displayed as thumbnails.
Each entry is of the form (FRAME . FRAME-PARAMETERS), which
records the `frame-parameters' of FRAME before it was turned into a
thumbnail.")

(defvar non-thumbnail-frames nil
  "An alist of frames currently not displayed as thumbnails.
Each entry is of the form (FRAME . FRAME-PARAMETERS), which
records the `frame-parameters' of FRAME when it was a thumbnail.")

(defvar thumbfrm-last-sort-function nil
  "Last non-nil value for `thumbfrm-sort-function' during this session.")

(defvar thumbfrm-last-row-show 0.7
  "Minimum amount showing of frames in last row (or column).
Set to 1.0 to see whole frames.
A smaller value uses display real estate better.")

(defvar thumbfrm-next-stack-xoffset nil
  "X offset for position to stack next thumbnail frame.")

(defvar thumbfrm-next-stack-yoffset nil
  "Y offset for position to stack next thumbnail frame.")

;; Just so we don't need to test boundp each time.
(defvar 1on1-minibuffer-frame nil)



;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;



;; REPLACES ORIGINAL (built-in).
;; Thumbifies if `thumbify-instead-of-iconify-flag'.
;;
(defadvice iconify-frame (around thumbify-replace-iconify activate)
  "Thumbify FRAME if `thumbify-instead-of-iconify-flag'; else iconify.
To iconify frame in spite of this flag, use `really-iconify-frame'."
  (if thumbify-instead-of-iconify-flag
      (thumbify-frame frame)
    ad-do-it))



;; REPLACES ORIGINAL.
;; Thumbifies/dethumbifies if `thumbify-instead-of-iconify-flag'.
;;
(defadvice iconify-or-deiconify-frame
  (around thumbify-replace-iconify activate)
  "Thumbify frame if `thumbify-instead-of-iconify-flag'; else iconify.
To iconify selected frame in spite of this flag, use
`really-iconify-or-deiconify-frame'."
  (if thumbify-instead-of-iconify-flag
      (toggle-thumbnail-frame)
    ad-do-it))

;;;###autoload
(defun thumbify-frame-upon-event (event)
  "Thumbify frame upon event EVENT.
To make the window-manager \"minimize\" button thumbify instead, bind
\[iconify-frame] to this command, as follows:
  (define-key special-event-map [iconify-frame]
              'thumbify-frame-upon-event)
That will effectively replace iconification by thumbification
everywhere, except for `really-iconify-frame' and
`really-iconify-or-deiconify-frame'."
  (interactive "e")
  (select-frame (posn-window (event-start event)))
  (make-frame-visible)
  (toggle-thumbnail-frame))

;; Thanks to Michael Cadilhac [Michael.Cadilhac-@t-lrde.epita.fr]
;; for a tip on using `discard-input' in a situation like this.
;;;###autoload
(defun really-iconify-frame (&optional frame)
  "Iconify FRAME, even if `thumbify-instead-of-iconify-flag' is non-nil."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (let ((thumbify-instead-of-iconify-flag nil))
    ;; Ensure we iconify, even if [iconify-frame] is bound in `special-event-map'.
    (iconify-frame frame)
    (while (not (input-pending-p)) (sit-for 0))
    (discard-input)))

;; Thanks to Michael Cadilhac [Michael.Cadilhac-@t-lrde.epita.fr]
;; for a tip on using `discard-input' in a situation like this.
;;;###autoload
(defun really-iconify-or-deiconify-frame ()
  "Iconify or deiconify frame, even if `thumbify-instead-of-iconify-flag'
is non-nil."
  (interactive)
  (let ((thumbify-instead-of-iconify-flag nil))
    (iconify-or-deiconify-frame)
    ;; Ensure we iconify, even if [iconify-frame] is bound in `special-event-map'.
    (while (not (input-pending-p)) (sit-for 0))
    (discard-input)))

;;;###autoload
(defun thumbify-frame (&optional frame)
  "Create a thumbnail version of FRAME (default: selected frame).
Variable `thumbfrm-frame-parameters' is used to determine
which frame parameters (such as menu-bar) to remove."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (let* ((fr+params (assoc frame non-thumbnail-frames))
         (params (cdr fr+params)))
    (when rename-frame-when-thumbify-flag (rename-non-minibuffer-frame))
    (unless (assoc frame thumbnail-frames) ; No-op if already a thumbnail.
      (add-to-list 'thumbnail-frames (cons frame (frame-parameters frame)))
      (setq non-thumbnail-frames (delq fr+params non-thumbnail-frames))
      (enlarge-font (- frame-thumbnail-font-difference) frame) ; Defined in `frame-cmds.el'.
      (when params (modify-frame-parameters frame params))
      (when thumbfrm-next-stack-xoffset
        (set-frame-position frame thumbfrm-next-stack-xoffset thumbfrm-next-stack-yoffset)
        (setq thumbfrm-next-stack-xoffset nil
              thumbfrm-next-stack-xoffset nil))
      (modify-frame-parameters frame thumbfrm-frame-parameters))))

;;;###autoload
(defun dethumbify-frame (&optional frame)
  "Restore thumbnail FRAME to original size (default: selected frame)."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (let* ((fr+params (assoc frame thumbnail-frames))
         (params (cdr fr+params)))
    (when fr+params                     ; No-op if not a thumbnail.
      (add-to-list 'non-thumbnail-frames (cons frame (frame-parameters frame)))
      (setq thumbnail-frames (delq fr+params thumbnail-frames))
      (enlarge-font frame-thumbnail-font-difference frame) ; Defined in `frame-cmds.el'.
      (modify-frame-parameters frame params)
      (select-frame-set-input-focus frame)
      (only-raise-frame frame))))

;;;###autoload
(defsubst thumbnail-frame-p (&optional frame)
  "Return non-nil if FRAME is a thumbnail."
  (interactive)
  (assoc (or frame (selected-frame)) thumbnail-frames))



(or (fboundp 'only-raise-frame)
    (fset 'only-raise-frame (symbol-function 'raise-frame)))

;; REPLACES ORIGINAL (built-in):
;; Also dethumbifies frame.
;;
(defun raise-frame (&optional frame)
  "Bring FRAME to the front, so it occludes any frames it overlaps.
If FRAME is invisible, make it visible.
If FRAME is a thumbnail frame (see `thumb-frm.el'), dethumbify it also.
If you don't specify a frame, the selected frame is used.

If Emacs is displaying on an ordinary terminal or other device that
does not support multiple overlapping frames, then do nothing."
  (unless frame (setq frame (selected-frame)))
  (only-raise-frame frame)
  (dethumbify-frame frame))

;;;###autoload
(defun toggle-thumbnail-frame (&optional frame)
  "If FRAME is a thumbnail, restore it; else thumbify it.
FRAME defaults to the selected frame."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (if (assoc frame thumbnail-frames)
      (dethumbify-frame frame)
    (thumbify-frame frame)))

(defalias 'fisheye 'thumbify-other-frames)

;;;###autoload
(defun thumbify-other-frames (&optional frame)
  "Thumbify all visible non-minibuffer frames except FRAME.
Dethumbify FRAME, if it is a thumbnail frame.
FRAME is the selected frame, by default."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (dethumbify-frame frame)
  (let ((other-frames (visible-frame-list)))
    (setq other-frames (delq frame other-frames))
    (setq other-frames (delq 1on1-minibuffer-frame other-frames))
    (dolist (fr (nset-difference other-frames (mapcar 'car thumbnail-frames)))
      (thumbify-frame fr))
    (stack-thumbnail-frames))
  frame)                                ; Return frame.

;;;###autoload
(defun fisheye-previous-frame ()
  "Call `fisheye' on the previous frame.
Thumbify all visible non-minibuffer frames except the previous frame.
Dethumbify the previous frame (if it is a thumbnail frame)."
  (interactive)
  (select-frame (thumbify-other-frames (other-frame -1)))
  (message "%s" (get-frame-name)))

;;;###autoload
(defun fisheye-next-frame ()
  "Call `fisheye' on the next frame.
Thumbify all visible non-minibuffer frames except the next frame.
Dethumbify the next frame (if it is a thumbnail frame)."
  (interactive)
  (select-frame (thumbify-other-frames (other-frame 1)))
  (message "%s" (get-frame-name)))

;;;###autoload
(defun dethumbify-all-frames ()
  "Dethumbify all visible frames
restoring them to their states before they were thumbified."
  (interactive)
  (dolist (fr (mapcar 'car (cull-thumbnail-frames))) (dethumbify-frame fr))
  (setq thumbnail-frames nil))          ; Make sure.


;; New row (or column) offset is based on the size of the previous
;; frame, not the current frame, which is not really correct.  This is
;; easier to do, and works OK most of the time.  It is not ideal if
;; frame sizes vary a great deal.
;;
(defun stack-thumbnail-frames ()
  "Stack thumbnail frames along edge of display
according to the direction of `thumbfrm-stack-display-edge'."
  (interactive)
  (let* ((display-width (x-display-pixel-width))
         (display-height (if (boundp '1on1-minibuffer-frame)
                             (cdr (assq 'top (frame-parameters 1on1-minibuffer-frame)))
                           (x-display-pixel-height)))
         (xstart (if (memq thumbfrm-stack-display-edge
                           '(right+down right+up top+to-left bottom+to-left))
                     display-width
                   0))
         (ystart (if (memq thumbfrm-stack-display-edge
                           '(bottom+to-right bottom+to-left left+up right+up))
                     display-height
                   0))
         (xoffset xstart)
         (yoffset ystart)
         (thumb-frs (copy-sequence (cull-thumbnail-frames)))
         last-fr)
    (when thumbfrm-sort-function
      (setq thumb-frs (sort thumb-frs thumbfrm-sort-function)))

    ;; These loops are similar, more or less symmetric.
    (case thumbfrm-stack-display-edge
      (left+down
       (dolist (fr (mapcar 'car thumb-frs))
         (set-frame-position fr xoffset yoffset)
         (let ((next-position
                (next-stack-position
                 yoffset ystart (- display-height
                                   window-mgr-title-bar-pixel-width
                                   ;;(frame-pixel-height fr)
                                   )
                 #'+ (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width) #'>
                 xoffset xstart (- display-width (frame-pixel-width fr))
                 #'+ (frame-pixel-width fr) #'>)))
           (setq yoffset (cdr next-position) xoffset (car next-position)))
         (only-raise-frame fr))
       (when thumb-frs
         (setq thumbfrm-next-stack-xoffset xoffset)
         (setq thumbfrm-next-stack-yoffset yoffset)))
      (left+up
       (dolist (fr (mapcar 'car thumb-frs))
         (setq last-fr fr)
         (set-frame-position
          fr
          xoffset
          (max 0 (- yoffset window-mgr-title-bar-pixel-width (frame-pixel-height fr))))
         (let ((next-position
                (next-stack-position
                 yoffset ystart (truncate (* (frame-pixel-height fr) thumbfrm-last-row-show))
                 #'- (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width) #'<
                 xoffset xstart (- display-width (frame-pixel-width fr))
                 #'+ (frame-pixel-width fr) #'>)))
           (setq yoffset (cdr next-position) xoffset (car next-position))))
       (dolist (fr (mapcar 'car (nreverse thumb-frs)))
         (only-raise-frame fr))
       (when thumb-frs
         (setq thumbfrm-next-stack-xoffset xoffset)
         (setq thumbfrm-next-stack-yoffset (max 0 (- yoffset window-mgr-title-bar-pixel-width
                                                     (frame-pixel-height last-fr))))))
      (right+down
       (dolist (fr (mapcar 'car thumb-frs))
         (setq last-fr fr)
         (set-frame-position fr (- xoffset (frame-pixel-width fr)) yoffset)
         (let ((next-position
                (next-stack-position
                 yoffset ystart (- display-height
                                   window-mgr-title-bar-pixel-width
                                   ;;;(frame-pixel-height fr)
                                   )
                 #'+ (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width) #'>
                 xoffset xstart 0
                 #'- (frame-pixel-width fr) #'<)))
           (setq yoffset (cdr next-position) xoffset (car next-position)))
         (only-raise-frame fr))
       (when thumb-frs
         (setq thumbfrm-next-stack-xoffset (- xoffset (frame-pixel-width last-fr)))
         (setq thumbfrm-next-stack-yoffset yoffset)))
      (right+up
       (dolist (fr (mapcar 'car thumb-frs))
         (setq last-fr fr)
         (set-frame-position
          fr
          (max 0 (- xoffset (frame-pixel-width fr)))
          (max 0 (- yoffset window-mgr-title-bar-pixel-width (frame-pixel-height fr))))
         (let ((next-position
                (next-stack-position
                 yoffset ystart (truncate (* (frame-pixel-height fr) thumbfrm-last-row-show))
                 #'- (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width) #'<
                 xoffset xstart 0
                 #'- (frame-pixel-width fr) #'<)))
           (setq yoffset (cdr next-position) xoffset (car next-position))))
       (dolist (fr (mapcar 'car (nreverse thumb-frs)))
         (only-raise-frame fr))
       (when thumb-frs
         (setq thumbfrm-next-stack-xoffset (max 0 (- xoffset (frame-pixel-width last-fr))))
         (setq thumbfrm-next-stack-yoffset (max 0 (- yoffset window-mgr-title-bar-pixel-width
                                                     (frame-pixel-height last-fr))))))
      (top+to-right
       (dolist (fr (mapcar 'car thumb-frs))
         (set-frame-position fr xoffset yoffset)
         (let ((next-position
                (next-stack-position
                 xoffset xstart (- display-width (frame-pixel-width fr))
                 #'+ (frame-pixel-width fr) #'>
                 yoffset ystart (- display-height
                                   window-mgr-title-bar-pixel-width
                                   (frame-pixel-height fr))
                 #'+ (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width) #'>)))
           (setq yoffset (car next-position) xoffset (cdr next-position)))
         (only-raise-frame fr))
       (when thumb-frs
         (setq thumbfrm-next-stack-xoffset xoffset)
         (setq thumbfrm-next-stack-yoffset yoffset)))
      (top+to-left
       (dolist (fr (mapcar 'car thumb-frs))
         (setq last-fr fr)
         (set-frame-position fr (max 0 (- xoffset (frame-pixel-width fr))) yoffset)
         (let ((next-position
                (next-stack-position
                 xoffset xstart (truncate (* (frame-pixel-width fr) thumbfrm-last-row-show))
                 #'- (frame-pixel-width fr) #'<
                 yoffset ystart (- display-height
                                   window-mgr-title-bar-pixel-width
                                   (frame-pixel-height fr))
                 #'+ (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width) #'>)))
           (setq yoffset (car next-position) xoffset (cdr next-position)))
         (only-raise-frame fr))
       (when thumb-frs
         (setq thumbfrm-next-stack-xoffset (max 0 (- xoffset (frame-pixel-width last-fr))))
         (setq thumbfrm-next-stack-yoffset yoffset)))
      (bottom+to-right
       (dolist (fr (mapcar 'car thumb-frs))
         (setq last-fr fr)
         (set-frame-position
          fr
          xoffset
          (max 0 (- yoffset window-mgr-title-bar-pixel-width (frame-pixel-height fr))))
         (let ((next-position
                (next-stack-position
                 xoffset xstart (- display-width (frame-pixel-width fr))
                 #'+ (frame-pixel-width fr) #'>
                 yoffset ystart (- display-height
                                   window-mgr-title-bar-pixel-width
                                   (frame-pixel-height fr))
                 #'- (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width) #'<)))
           (setq yoffset (car next-position) xoffset (cdr next-position))))
       (dolist (fr (mapcar 'car (nreverse thumb-frs)))
         (only-raise-frame fr))
       (when thumb-frs
         (setq thumbfrm-next-stack-xoffset xoffset)
         (setq thumbfrm-next-stack-yoffset (max 0 (- yoffset window-mgr-title-bar-pixel-width
                                                     (frame-pixel-height last-fr))))))
      (bottom+to-left
       (dolist (fr (mapcar 'car thumb-frs))
         (setq last-fr fr)
         (set-frame-position
          fr
          (max 0 (- xoffset (frame-pixel-width fr)))
          (max 0 (- yoffset window-mgr-title-bar-pixel-width (frame-pixel-height fr))))
         (let ((next-position
                (next-stack-position
                 xoffset xstart (* (truncate (frame-pixel-width fr) thumbfrm-last-row-show))
                 #'- (frame-pixel-width fr) #'<
                 yoffset ystart (- display-height
                                   window-mgr-title-bar-pixel-width
                                   (frame-pixel-height fr))
                 #'- (+ (frame-pixel-height fr) window-mgr-title-bar-pixel-width) #'<)))
           (setq yoffset (car next-position) xoffset (cdr next-position))))
       (dolist (fr (mapcar 'car (nreverse thumb-frs)))
         (only-raise-frame fr))
       (when thumb-frs
         (setq thumbfrm-next-stack-xoffset (max 0 (- xoffset (frame-pixel-width last-fr))))
         (setq thumbfrm-next-stack-yoffset (max 0 (- yoffset window-mgr-title-bar-pixel-width
                                                     (frame-pixel-height last-fr)))))))))

;; Helper function for `stack-thumbnail-frames'.
(defun next-stack-position
  (major-position major-start major-limit major-+/- major-increment major-</>
   minor-position minor-start minor-limit minor-+/- minor-increment minor-</>)
  "Return new position for next-stacked frame.
MAJOR* are the settings for the major direction of movement (x or y).
MINOR* are the settings for the minor direction of movement (x or y).
New position is returned as a cons: (MINOR-POSITION . MAJOR-POSITION).

*-POSITION, as input, are the positions of the current frame.
*-START are the starting positions for the first row (or column).
*-LIMIT are the maximum positions on the display.
*-+/- are functions `+' or `-', used to increment or decrement.
*-</> are functions `<' or `>', used to compare."
  (setq major-position (funcall major-+/- major-position major-increment))
  (when (funcall major-</> major-position major-limit)
    (setq major-position major-start)
    (setq minor-position (funcall minor-+/- minor-position minor-increment))
    (when (funcall minor-</> minor-position minor-limit)
      (setq minor-position minor-start)))
  (cons minor-position major-position))

;;;###autoload
(defun toggle-sort-thumbnail-frame-stack (force-p)
  "Toggle stacking thumbnail frames between sorting and not.
Non-nil prefix FORCE-P => Sort iff FORCE-P >= 0."
  (interactive "P")
  (cond (thumbfrm-sort-function
         (setq thumbfrm-last-sort-function thumbfrm-sort-function) ; Save it.
         (when (or (null force-p) (<= (prefix-numeric-value force-p) 0))
           (setq thumbfrm-sort-function nil))) ; Don't sort.
        ((or (null force-p) (> (prefix-numeric-value force-p) 0)) ; Ask to sort
         (if thumbfrm-last-sort-function ; Sort using saved sort fn.
             (setq thumbfrm-sort-function thumbfrm-last-sort-function)
           (error "You first need to set `thumbfrm-sort-function'"))))
  (if thumbfrm-sort-function
      (message "Stacking of thumbnail frames is now sorted using `%s'."
               thumbfrm-sort-function)
    (message "Stacking of thumbnail frames is longer sorted.")))

;;;###autoload
(defun iconify-thumbnail-frames ()
  "Iconify all thumbnail frames."
  (interactive)
  (let ((thumbify-instead-of-iconify-flag nil))
    (mapcar (lambda (fr-spec) (iconify-frame (car fr-spec)))
            (cull-thumbnail-frames))))

;;;###autoload
(defun deiconify-thumbnail-frames ()
  "Deiconify all thumbnail frames."
  (interactive)
  (mapcar (lambda (fr-spec) (make-frame-visible (car fr-spec)))
          (cull-thumbnail-frames)))

(defun cull-thumbnail-frames ()
  "Remove dead, invisible, and iconified frames from `thumbnail-frames'."
  (setq thumbnail-frames
        (delete-if-not
         (lambda (fr+params) (and (frame-live-p (car fr+params))
                                  (eq t (frame-visible-p (car fr+params)))))
         thumbnail-frames)))

;;;###autoload
(defun doremi-thumbnail-frames ()
  "Successively cycle through frames with fisheye."
  (interactive)
  (let ((other-frames (visible-frame-list)))
    (setq other-frames (delq 1on1-minibuffer-frame other-frames))
    (doremi (lambda (fr) (thumbify-other-frames fr) fr)
            (selected-frame)
            nil                         ; ignored
            nil                         ; ignored
            other-frames)))

(defun thumbfrm-sort-by-name (framespec1 framespec2)
  "Alphabetical comparison of names of frames in argument frame specs.
FRAMESPEC1 and FRAMESPEC2 are the frame specs.
Return non-nil if name of first frame comes before that of second."
  (string-lessp (cdr (assq 'name (cdr framespec1)))
                (cdr (assq 'name (cdr framespec2)))))

(defun thumbfrm-sort-by-window-id (framespec1 framespec2)
  "Comparison of `window-id' parameters in argument frame specs.
FRAMESPEC1 and FRAMESPEC2 are the frame specs.
Return non-nil if `window-id' parameter of first frame comes before
that of second.  The `window-id' can be used as a substitute for time
of window creation."
  (string-lessp (cdr (assq 'window-id (cdr framespec1)))
                (cdr (assq 'window-id (cdr framespec2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'thumb-frm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thumb-frm.el ends here
