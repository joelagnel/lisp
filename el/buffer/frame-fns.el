;;; frame-fns.el --- frame manipulation commands

;; Copyright (C) 1992, 93, 94, 95, 96, 97, 99, 00, 05, 2006 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: frame-fns.el,v 1.22 2006/07/03 05:40:19 friedman Exp $

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
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(require 'list-fns)

;;; Do not add autoload cookies for these macros; any package which uses
;;; them should `require' this file explicitly.

(defmacro save-frame-excursion (&rest body)
  "Execute BODY, saving and restoring the selected frame."
  (let ((orig-frame (make-symbol "orig-frame")))
    `(let ((,orig-frame (selected-frame)))
       (unwind-protect
           (progn ,@body)
         (when (frame-live-p ,orig-frame)
           (select-frame ,orig-frame))))))

(put 'save-frame-excursion 'lisp-indent-function 0)

;; To be able to step into forms under edebug, eval the following.
;(def-edebug-spec save-frame-excursion t)

;; In emacsen where both window-system and tty frames can exist in the same
;; process, some initializations may be dependent on what kind of frame
;; is in use; it's not enough to check the window system type once at startup.
(defmacro for-frame-type (type name &rest body)
"For frames of type TYPE, name a routine NAME which executes BODY.
The forms are inserted onto the hook `after-make-frame-functions'.

The BODY forms will only be evaluated for frames matching TYPE: immediately
for each frame existing at definition time, and subsequently for each new
frame at creation time.  The forms are evaluated with the relevant frame
currently selected; the first window on the frame is also selected, so that
popups and minibuffer messages will show up in the right frame.

TYPE may be a symbol or list of symbols:
  * when TYPE is `nil' or `tty', the forms will only be evaluated
    in console/tty frames.
  * when TYPE is t or `window-system', the forms will be evaluated
    in any frame on a bitmapped display (i.e. not console/tty frames).
  * when TYPE is a list of symbols, the forms will be evaluated in any
    frame whose value of the frame-local variable `window-system' is a
    member of the list.
  * When TYPE is a list of symbols beginning with `not', the forms will be
    evaluated in any frame whose value of the frame-local variable
    `window-system' is *not* a member of the list.

NAME can be any symbol, for reference when redefining a body of forms to be
evaluated.  Therefore unless you intentionally want to replace a previous
set of forms that were labeled with that tag, do not reuse symbol names."
  (let ((fn-name (intern (concat "for-frame-type:"
                                 (if (symbolp name)
                                     (symbol-name name)
                                   name))))
        (frame-sym (make-symbol "frame"))

        (predicate
         (cond ((memq type '(nil tty))
                '(not window-system))
               ((memq type '(t window-system))
                'window-system)
               ((symbolp type)
                (list 'eq 'window-system (list 'quote type)))
               ((not (consp type))
                (signal 'wrong-type-argument
                        (list 'symbol-or-cons-p 'type
                              (cons :type type)
                              (cons :name name)
                              (cons :body body))))
               ;; To exclude ttys, put `nil' in the list.
               ;; The special case (not . x) uses eq instead of memq;
               ;; but (not x) will still use memq.
               ((eq (car type) 'not)
                `(not (,(if (consp (cdr type)) 'memq 'eq)
                       window-system (quote ,(cdr type)))))
               (t `(memq window-system (quote ,type))))))

    `(progn
       (defun ,fn-name (&optional ,frame-sym)
         (cond (,frame-sym) ; non-nil
               ;; emacs 19 ran after-make-frame-hook with no args,
               ;; but the new frame is let-bound to `nframe'.
               ((boundp 'nframe) (setq ,frame-sym nframe)))
         (save-frame-excursion
           (select-frame ,frame-sym)
           (save-window-excursion
             ;; select first window on frame so that `message' etc. use a
             ;; reasonable minibuffer window, and `display-buffer' etc.
             ;; create windows on the right frame.
             (select-window (frame-first-window ,frame-sym))
             (when ,predicate ,@body))))

       ;; Make hook run for all subsequently-created frames.
       ;; Emacs 19 did not have after-make-frame-functions.
       (if (boundp 'after-make-frame-functions)
           (add-hook 'after-make-frame-functions (quote ,fn-name))
         (add-hook 'after-make-frame-hook (quote ,fn-name)))

       ;; Call new hook for all existing frames
       (let ((,frame-sym (frame-list)))
         (while ,frame-sym
           (,fn-name (car ,frame-sym))
           (setq ,frame-sym (cdr ,frame-sym)))))))

(put 'for-frame-type 'lisp-indent-function 2)

(defmacro for-window-system-frames (name &rest body)
  "NAME a BODY of expressions to be evaluated at frame creation time.
These expressions will only be evaluated for window system frames.
See `for-frame-type' for more details."
  `(for-frame-type window-system ,name ,@body))

(put 'for-window-system-frames 'lisp-indent-function 1)

(defmacro for-tty-frames (name &rest body)
  "NAME a BODY of expressions to be evaluated at frame creation time.
These expressions will only be evaluated for console/tty frames.
See `for-frame-type' for more details."
  `(for-frame-type tty ,name ,@body))

(put 'for-tty-frames 'lisp-indent-function 1)


;;;###autoload
(defvar x-display-completions (make-vector 7 0)
  "Completion table for `x-display-complete'.")

;;;###autoload
(defvar x-display-history nil
  "History of X server display names.")

(defun x-display-complete (string predicate action)
  (if action
      (all-completions string x-display-completions predicate)
    (try-completion string x-display-completions predicate)))

;;;###autoload
(defun x-display-add-completion (&rest displays)
  (interactive (list (completing-read "Add display completion: "
                                      'x-display-complete
                                      nil nil nil 'x-display-history)))
  (while displays
    (intern (car displays) x-display-completions)
    (setq displays (cdr displays))))

(defun x-display-remove-completion (&rest displays)
  (interactive (list (completing-read "Remove display completion: "
                                      'x-display-complete
                                      nil t nil 'x-display-history)))
  (while displays
    (unintern (car displays) x-display-completions)
    (setq displays (cdr displays))))

;;;###autoload
(defun x-display-completing-read (prompt &optional initial)
  (let ((disp (completing-read prompt 'x-display-complete
                               nil nil initial 'x-display-history)))
    (x-display-add-completion disp)
    disp))


;; There needs to be a better way to get this information.
(defun frame-xwininfo (&optional frame)
  "Return an alist of X window attributes for frame FRAME.
These attributes are generated from the output of the `xwininfo' command,
  since some values (e.g. the absolute location of frames including window
  manager decorations) cannot be determined from emacs primitives.

If FRAME is not specified, the selected frame is the default."
  (with-temp-buffer
    (let ((id (if x-no-window-manager 'outer-window-id 'parent-id)))
      (call-process "xwininfo" nil t nil
                    "-display" (frame-parameter frame 'display)
                    "-id"      (format "%s" (frame-parameter frame id))))
    (let ((case-fold-search t)
          (data nil)
          re start val)
      (mapc (lambda (param)
              (goto-char (point-min))
              (setq re (concat "^[ \t]*" (cadr param) "[ \t]*"))
              (when (re-search-forward re nil t)
                (setq start (match-end 0))
                (end-of-line)
                (save-restriction
                  (narrow-to-region start (point))
                  (goto-char (point-min))
                  (setq val (buffer-substring start
                             (if (re-search-forward "[ \t]+$" nil t)
                                 (match-beginning 0)
                               (point-max)))))

                (if (string-match "^[0-9]+$" val)
                    (setq val (string-to-int val)))

                (setq data (cons (cons (car param) val) data))))
        '((abs-x             "Absolute upper-left X:")
          (abs-y             "Absolute upper-left Y:")
          (rel-x             "Relative upper-left X:")
          (rel-y             "Relative upper-left Y:")
          (width             "Width:")
          (height            "Height:")
          (depth             "Depth:")
          (visual            "Visual Class:")
          (border-width      "Border width:")
          (class             "Class:")
          (colormap          "Colormap:")
          (bit-gravity       "Bit Gravity State:")
          (window-gravity    "Window Gravity State:")
          (backing-store     "Backing Store State:")
          (save-under        "Save Under State:")
          (map-state         "Map State:")
          (override-redirect "Override Redirect State:")
          (corners           "Corners:")
          (geometry          "-geometry")))
      (nreverse data))))

;; Unlike set-frame-position (or even x-set-frame-geometry below), this
;; function takes into account window manager decorations around the edges
;; of the frame.
(defun x-set-frame-position (&optional frame xoff yoff)
  "Sets position of FRAME in pixels to XOFF by YOFF.
This is actually the position of the upper left corner of the frame,
including any window manager decorations.

If either of XOFF or YOFF are nil, that coordinate is not changed.

Negative values for XOFF or YOFF are interpreted relative to the rightmost
or bottommost possible position that stays within the display.

Values for XOFF or YOFF that are a list of the form (+ N) are interpreted
to mean N pixels relative to top/left corner; A value of the form (- N)
are interpreted to mean -N pixels relative to the bottom/right corner.

For example, an xoff value of (+ -10) would place the left side of the
frame 10 pixels off the edge of the left side of the display.
A value of (- -10) would place the right edge of the frame 10 pixels off
the edge of the rightmost edge of the display.

To put the frame exactly at the bottom right corner, use the
coordinates (- 0) (- 0)."
  (unless frame (setq frame (selected-frame)))
  (let* ((wininfo (frame-xwininfo frame))
         (borders (* 2 (cdr (assq 'border-width wininfo))))
         ;; fields are: offset, disp-dim, frame-dim, curpos, origin
         (xdata (vector xoff
                        (x-display-pixel-width frame)
                        (cdr (assq 'width wininfo))
                        (cdr (assq 'abs-x wininfo))
                        0))
         (ydata (vector yoff
                        (x-display-pixel-height frame)
                        (cdr (assq 'height wininfo))
                        (cdr (assq 'abs-y  wininfo))
                        0)))
    (mapc (lambda (vec)
            (let ((off    (aref vec 0))
                  (origin (aref vec 4))
                  (delta (- (aref vec 1)
                            (aref vec 2)
                            borders)))
              (cond ((null off)
                     (setq off (aref vec 3)))
                    ((numberp off)
                     (if (< off 0)
                         (setq origin delta)))
                    ((consp off)
                     (cond ((eq '- (car off))
                            (setq origin delta
                                  off (- (nth 1 off))))
                           ((< (nth 1 off) 0)
                            (setq origin (- delta)
                                  off (nth 1 off)))
                           (t (setq off (nth 1 off))))))
              (aset vec 0 off)
              (aset vec 4 origin)))
      (list xdata ydata))
    (set-frame-position frame
                        (+ (aref xdata 4) (aref xdata 0))
                        (+ (aref ydata 4) (aref ydata 0)))))

(defun x-geometry-coord-add (coord delta)
  "Adjust positional coordinate COORD by DELTA amount.
COORD is a coordinate in a form allowed by `x-set-frame-position'; that is,
it may be either an integer or a list of the form (+/- N).
DELTA must be an integer. "
  (cond ((numberp coord)
         ;; If result switches signs from coord, maintain corner-relativity.
         (let ((r (+ coord delta)))
           (if (< (* r coord) 0)
               (list (if (>= coord 0) '+ '-) r)
             r)))
        ((consp coord)
         (list (nth 0 coord)
               (+ (nth 1 coord) delta)))))

(defun x-adjust-frame-position (frame x &optional y)
  (or frame (setq frame (selected-frame)))
  (or x (setq x 0))
  (or y (setq y 0))
  (let* ((fp (frame-parameters frame))
         (xpos (cdr (assq 'left fp)))
         (ypos (cdr (assq 'top fp))))
    (modify-frame-parameters frame
     (list (cons 'left (x-geometry-coord-add xpos x))
           (cons 'top  (x-geometry-coord-add ypos y))))))

(defun x-set-frame-geometry (frame geom)
  "Set FRAME to GEOM, specified as an X-style geometry string.

Unlike `modify-frame-parameters', this function will take into account
window manager decorations to keep the frame's edges on the display,
particularly when specifying negative coordinates \(i.e. coordinates from
the bottom or right edges of the screen\)."
  (let* ((param (x-parse-geometry geom))
         (x (assq 'left param))
         (y (assq 'top param)))
    (if (or (and (numberp (cdr x)) (< (cdr x) 0))
            (and (numberp (cdr y)) (< (cdr y) 0))
            (and (consp (cdr x)) (eq '- (cadr x)))
            (and (consp (cdr y)) (eq '- (cadr y))))
        (setq param (delq y (delq x param)))
      (setq x nil y nil))
    (modify-frame-parameters frame param)
    (when (or x y)
      (sit-for 0) ;; redisplay frame so it can move
      (x-set-frame-position frame (cdr x) (cdr y)))))

;; Be aware that this function returns coordinates that are correct for the
;; emacs frame itself, but may not reflect its absolute position on the
;; display because the window manager chooses to wrap chrome (e.g. title
;; bars) around it.
;;
;; This behavior is consistent with other emacs primitives but it can be
;; frustrating to try to position a frame using absolute coordinates and
;; then have them be off by some amount due to window manager chrome
;; fudging.  There are other functions in this library to provide
;; compensation for that.  See in particular `frame-xwininfo'.
(defun x-frame-geometry (&optional frame &rest relative)
  "Return the geometry of the frame as an X-style geometry string.
The string returned is in a form understood by `x-parse-geometry'."
  (let* ((fp (frame-parameters frame))
         (disp (cdr (assq 'display fp)))
         (border (* 2 (cdr (assq 'border-width fp))))
         (left (cdr (assq 'left fp)))
         (top  (cdr (assq 'top fp))))

    (when (numberp left)
      (setq left (list '+ left)))
    (when (memq 'right relative)
      (let* ((display-width (x-display-pixel-width disp))
             (frame-width (+ border (frame-pixel-width frame)))
             (right-edge (x-geometry-coord-add left frame-width)))
        (setq left (x-geometry-coord-add (list '- display-width)
                                         (- (nth 1 right-edge))))))
    (setq left (mapconcat (lambda (s) (format "%s" s)) left ""))

    (when (numberp top)
      (setq top (list '+ top)))
    (when (memq 'bottom relative)
      (let* ((display-height (x-display-pixel-height disp))
             (frame-height (+ border (frame-pixel-height frame)))
             (bottom-edge (x-geometry-coord-add top frame-height)))
        (setq top (x-geometry-coord-add (list '- display-height)
                                        (- (nth 1 bottom-edge))))))
    (setq top (mapconcat (lambda (s) (format "%s" s)) top ""))

    (format "%dx%d%s%s"
            (cdr (assq 'width fp))
            (cdr (assq 'height fp))
            left top)))


(defadvice delete-frame (around frame-fns:close-x-connection activate)
  "If deleting the last frame on an X display, query before closing the display."
  (interactive (list nil current-prefix-arg))
  (let ((frame (or (ad-get-arg 0) (selected-frame))))
    (cond
     ((and terminal-frame
           (eq frame terminal-frame))
      (error "Do not close initial terminal frame; it will just hang."))
     ((or (eq 'x (cdr (assq 'window-system (frame-parameters frame))))
          (eq window-system 'x))
      (let* ((primary-display (and (boundp 'x-display-name)
                                   (symbol-value 'x-display-name)))
             (display (or (cdr (assq 'display (frame-parameters frame)))
                          primary-display))
             (other-frames-on-display
              (filtered-frame-list
               (lambda (f)
                 (and (not (eq f frame))
                      (string= display
                               (or (cdr (assq 'display (frame-parameters f)))
                                   "")))))))
        (cond ((null display))
              ((and (null other-frames-on-display)
                    (not (y-or-n-p (concat "Close connection to X server "
                                           display "? ")))))
              (t
               (prog1
                   ad-do-it
                 (and (not (frame-live-p frame))
                      (null other-frames-on-display)
                      (stringp primary-display)
                      ;; Closing the x connection to the original display can
                      ;; crash some versions of emacs.
                      (not (string= display primary-display))
                      (x-close-connection display)))))))
     (t ad-do-it))))

(defadvice make-frame-on-display (before frame-fns:histcomplete activate)
  "Provide completion and history on previously-seen display names."
  (interactive (list (x-display-completing-read "Make frame on display: "))))

;;;###autoload
(defun other-frame-absolute (arg)
  "Like other-frame, but don't skip over non-visible frames.
If the target frame is not visible, make it visible."
  (interactive "p")
  (let ((frame (selected-frame)))
    (while (> arg 0)
      (setq frame (next-frame frame))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame))
      (setq arg (1+ arg)))
    (raise-frame frame)
    (select-frame frame)
    (set-mouse-position (selected-frame) (1- (frame-width)) 0)
    (and (fboundp 'unfocus-frame)
         (unfocus-frame))))

;;;###autoload
(defun set-basic-frame-color (color &optional frame fringe-full-brightness-p)
  (interactive "sColor: ")
  (or frame (setq frame (selected-frame)))
  (cond ((facep 'fringe)
         ;; Emacs 21.  Set specific faces, otherwise other faces are trashed.
         (set-face-foreground 'fringe
                              (if fringe-full-brightness-p
                                  color
                                (make-less-bright-color color 2 frame))
                              frame)

         (set-face-foreground 'default   color frame)
         (set-face-foreground 'mode-line color frame)
         (set-face-background 'cursor    color frame)
         (set-face-background 'mouse     color frame))
        (t
         ;; Emacs 20 and earlier.
         (save-frame-excursion
           (select-frame frame)
           (set-foreground-color color)
           (set-cursor-color     color)
           (set-mouse-color      color)))))

;;;###autoload
(defun set-frame-titles (title &optional frame)
  "Set the icon and window titles of frame FRAME to TITLE.
If called interactively or no frame is specified in a function call, the
selected frame is modified."
  (interactive "sSet selected frame title: ")
  (modify-frame-parameters (or frame (selected-frame))
                           (mapcar (lambda (key) (cons key title))
                                   '(name title icon-name))))

;;;###autoload
(defalias 'set-selected-frame-title 'set-frame-titles)
(make-obsolete 'set-selected-frame-title "use `set-frame-titles' instead")

;;;###autoload
(defun set-default-frame-title (title)
  "Set the default icon and window titles of future frames to TITLE."
  (interactive "sSet default frame title: ")
  (let ((syms '(name title icon-name)))
    (while syms
      (set-alist-slot 'default-frame-alist (car syms) title)
      (setq syms (cdr syms)))))

;;;###autoload
(defun set-display (&optional disp)
  "Set DISPLAY environment variable.
If argument is nil or \"\", unset variable."
  (interactive
   (list (let ((s (cond ((getenv "DISPLAY"))
                        ((eq window-system 'x)
                         (cdr (assq 'display (frame-parameters))))
                        (t ":0.0"))))
           (x-display-completing-read "DISPLAY = " (cons s 0)))))
  (and (string= disp "")
       (setq disp nil))
  (setenv "DISPLAY" disp))

;;;###autoload
(defun set-cursor-type (&optional frame type height-or-width)
  "Set default cursor type for current frame.
If the current buffer has a local value for `cursor-type', the
new cursor type may not be visible there."
  (interactive (list nil
                     (completing-read
                      "Cursor type (default `box'): "
                      '((box) (hollow) (nil) (bar) (hbar))
                      nil t nil nil "box")
                     (when current-prefix-arg
                       (read-number "Size in pixels: " 2))))
  (when (stringp type)
    (setq type (intern type)))
  (when (and (memq type '(bar hbar))
             height-or-width)
    (setq type (cons type height-or-width)))
  (modify-frame-parameters frame `((cursor-type . ,type))))

;; Unicode says that the characters U+0060 and U+0027 are a grave accent
;; and a vertical quote, not an open and close quotation mark.  The XFree86
;; (and X.org Foundation) 4.x fonts display these characters accordingly.
;; However, GNU documentation (info and doc strings) use them as open and
;; close quotation marks (a holdover from ascii days and display terminals
;; where these characters appeared balanced).
;;
;; The emacs development source for 21.5 (later renamed to 22.0) was
;; briefly changed to use U+2018 and U+2019 character glyphs (these come
;; from iso10646 fonts) for display of U+0060 and U+0027 in the
;; "standard-display-european" display table, but consensus was that this
;; was a bad idea.
;;
;; Ken'ichi HANDA <handa@m17n.org> wrote on emacs-devel:
;;
;;     I think `' should not be displayed by U+2018 and U+2019.  Unicode
;;     defines them not as balanced quotes.  Using them as balanced quotes
;;     is abuse of characters as far as we follow Unicode.
;;
;;     Considering the long standing convention, I don't suggest to stop
;;     this abuse.  But, at least, we should not disturb people who use
;;     those characters correctly in the sense of Unicode by displaying
;;     them with characters of different semantics.
;;
;; I agree.  However I am choosing to abuse the display for myself privately.
;;
;; More details about quotes at: http://www.cl.cam.ac.uk/~mgk25/ucs/quotes.html
;;
;;;###autoload
(defun display-balanced-single-quotes (&optional prefix)
  "Replace display of iso8859-1 ` \(0x60\) and ' \(0x27\) characters with symmetric glyphs.

On window-systems frames, these characters are replaced
with the glyphs from iso10646 U+2018 and U+2019, respectively.

On tty frames, ' \(0x27\) is replaced with \264 (0xb4), which is usually
symmetric with ` \(0x60\).

This command works by modifying `standard-display-table' \(which see\); if
the version of emacs in use supports simultaneous X and tty frames,
consider first making `standard-display-table' frame-local with
`make-standard-display-table-frame-local'.

With positive prefix arg, enable glyph replacement.
With negative prefix arg, restore display to original glyphs.
Otherwise, toggle current display."
  (interactive "P")
  (and (consp prefix) (setq prefix (car prefix)))
  (cond ((or (null prefix) (equal prefix 0))
         (setq prefix 'toggle))
        ((and (numberp prefix) (> prefix 0))
         (setq prefix t))
        (t (setq prefix nil)))
  (let ((tbl standard-display-table)
        (uni 'mule-unicode-0100-24ff))
    (cond ((not window-system)
           ;; On character terminals, just try to make the vertical quote
           ;; match the slanted backquote.
           (if (or (eq prefix t)
                   (and (eq prefix 'toggle)
                        (null (aref tbl ?'))))
               (aset tbl ?' [180])
             (aset tbl ?' nil)))
          ((and (eq window-system 'x)
                (member (downcase (x-server-vendor))
                        '("the xfree86 project, inc"
                          "the x.org foundation"))
                (> (aref (number-to-string (nth 2 (x-server-version))) 0) ?3)
                (get uni 'charset)) ;; FSF v21
           (cond ((or (eq prefix t)
                      (and (eq prefix 'toggle)
                           (null (aref tbl ?'))))
                  (aset tbl ?' (vector (make-char uni 114 121)))
                  (aset tbl ?` (vector (make-char uni 114 120))))
                 (t
                  (aset tbl ?' nil)
                  (aset tbl ?` nil)))))
    (when (interactive-p)
      (message "balanced quote glyphs %s"
               (if (aref tbl ?') "enabled" "disabled"))))
  prefix)


;;;###autoload
(defun make-standard-display-table-frame-local ()
  "Make all current and future frames have a frame-local standard-display-table.
Frames with the same window-system type e.g. `x', `nil' (for tty), share the same
display table."
  (interactive)
  (require 'disp-table) ; make sure standard-display-table initialized
  (make-variable-frame-local 'standard-display-table)
  (add-hook 'after-make-frame-functions
            'set-frame-type-local-standard-display-table)
  (mapc 'set-frame-type-local-standard-display-table (frame-list)))

(defun set-frame-type-local-standard-display-table (&optional frame)
  "Initialize FRAME with the proper frame-local standard-display-table."
  (or frame (setq frame (selected-frame)))
  (modify-frame-parameters frame
    (list (cons 'standard-display-table
                (frame-type-local-standard-display-table frame t t)))))

(defun frame-type-local-standard-display-table (frame-type &optional createp copy-default-p)
  "Return the standard-display-table for frames of type FRAME-TYPE.
If FRAME-TYPE is a frame object, return the table for window-system type of that frame.

Optional argument CREATEP means create a new standard-display-table for
frames of that type if none exists yet.

Optional argument COPY-DEFAULT-P means start the new table with a full copy
of the global standard-display-table \(if any\)."
  (when (or (null frame-type)
            (framep frame-type))
    (setq frame-type (frame-display-type frame-type)))
  (let ((tbl (get 'standard-display-table frame-type)))
    (cond (tbl)
          (createp ; no tbl yet
           (if (and copy-default-p
                    (default-boundp 'standard-display-table)
                    (char-table-p (default-value 'standard-display-table)))
               (setq tbl (copy-char-table
                          (default-value 'standard-display-table)))
             (setq tbl (make-display-table))
             (put 'standard-display-table frame-type tbl))))
    (or tbl standard-display-table)))

;;;###autoload
(defun frame-display-type (&optional frame)
  "Return the window-system type for FRAME, or `tty' if not on a window system."
  (or frame (setq frame (selected-frame)))
  ;; Handle case where window-system is frame-local because
  ;; there are both tty and x frames.  We have to check that
  ;; the variable is actually in the frame-parameter alist; a
  ;; nil result from `frame-parameter' wouldn't be
  ;; distinguishable from the spec for a tty frame.
  (cond ((let ((cell (assq 'window-system (frame-parameters frame))))
           (when cell
             (or (cdr cell) 'tty))))
        ;; Otherwise, nil means tty frame
        ;; Don't let buffer-local vars shadow value
        (t (or (default-value 'window-system) 'tty))))

(defun copy-char-table (table)
  "Make a full copy of char-table TABLE.
All slots, including table subtype and extra slots, are copied.
If any of the slots is a symbol, the value of that symbol is not copied."
  (let* ((type (char-table-subtype table))
         (new (make-char-table type))
         (extra-slots (get type 'char-table-extra-slots))
         (slot 0))
    (map-char-table (lambda (key val) (aset new key val)) table)
    (while (< slot extra-slots)
      (set-char-table-extra-slot new slot (char-table-extra-slot table slot))
      (setq slot (1+ slot)))
    new))


;; This is here for the sake of empty-other-frames.
;;;###autoload
(defun switch-to-empty-buffer ()
  "Switch current window to a read-only buffer with no contents."
  (interactive)
  (switch-to-buffer " *empty*")
  (fundamental-mode)
  (setq default-directory "~/")
  (and (fboundp 'protect-buffer-from-kill-mode)
       (protect-buffer-from-kill-mode 1))
  (setq buffer-read-only t)
  (current-buffer))

;;;###autoload
(defun empty-other-frames (&optional preserved-frame)
  "Insure that no other frames display any buffers.

The purpose of this is twofold: first, it prevents `other-buffer' from
skipping buffers in the current frame's buffer list just because they may
be visible in some other frame; secondly, it reduces the potential for
redisplay on remote frames, which might freeze emacs when a network
connection is down or slow at a later point in time.

If escreen is enabled, non-trivial window configurations are saved and a
new escreen is created; or if an escreen already exists with just the empty
buffer in a single window, that escreen will be selected.

This function uses `switch-to-empty-buffer' to alter the window buffers."
  (interactive)
  (mapc (lambda (frame)
          (let ((wlist (window-list frame 'never-minibuf
                                    (frame-first-window frame))))
            (cond ((and (null (cdr wlist)) ; only 1 window
                        (not (window-dedicated-p (car wlist))))
                   (save-window-excursion
                     (select-window (car wlist))
                     (switch-to-empty-buffer)))

                  ;; If escreens are in use and the current screen wasn't a
                  ;; one-window screen altered above, then search all the
                  ;; screens on this frame to see if there are any which
                  ;; already have just one window with the empty buffer.
                  ;; If so, switch to it.  Otherwise, create a new screen
                  ;; so that nontrivial window configurations are not lost.
                  ((fboundp 'escreen-create-screen)
                   (save-frame-excursion
                     (select-frame frame)
                     ;; update screen map now so we have accurate data;
                     ;; normally only updated when switching screens.
                     (escreen-save-current-screen-configuration)
                     (let ((screens (escreen-get-active-screen-numbers))
                           screen-window-map)
                       (while (consp screens)
                         (setq screen-window-map
                               (escreen-configuration-data-map
                                (escreen-configuration-escreen (car screens))))
                         (cond ((and (null (cdr screen-window-map)) ; 1 window
                                     (string= ;; name of buffer in that window
                                      (escreen-configuration-data-map-critical-buffer-name
                                       (escreen-configuration-data-map-critical
                                        (car screen-window-map)))
                                      " *empty*"))
                                (escreen-goto-screen (car screens))
                                (setq screens t))

                               (t (setq screens (cdr screens)))))

                       (unless (eq screens t)
                         (escreen-create-screen)
                         (switch-to-empty-buffer)))))

                  (t
                   (save-window-excursion
                     (select-window (car wlist))
                     (delete-other-windows)
                     ;; Is this a good idea?  If the window is dedicated,
                     ;; perhaps it shouldn't be deleted.
                     ;; To hell with it.  If it's that important, use escreen.
                     (when (window-dedicated-p (selected-window))
                       (select-window (split-window))
                       (delete-other-windows))
                     (switch-to-empty-buffer))))))
    (delq (or preserved-frame (selected-frame))
          (frame-list))))


;; Not sure if these belong here, but what the hell.

;;;###autoload
(defun make-aaa-frame (&optional name frame-params)
  "Make a frame that resembles an Ann Arbor Ambassador portrait mode display."
  (interactive (list (and current-prefix-arg
                          (read-string "Frame name: "))))
  (or name (setq name (cdr (assq 'name default-frame-alist))))
  (let ((params `((name                 . ,name)
                  (icon-name            . ,name)
                  (title                . ,name)
                  (font                 . "fixed")
                  (menu-bar-lines       . 0)
                  (tool-bar-lines       . 0)
                  (vertical-scroll-bars . nil)
                  (left-fringe          . 0)
                  ;; Until there is some way to put the ascii `\'
                  ;; continuation char back on X frames, the right-fringe
                  ;; is needed.
                  ;;(right-fringe       . 0)
                  (cursor-type          . hbar)
                  (width                . 80)
                  (height               . 60)
                  (border-color         . "black")
                  (background-color     . "black")))
        frame)
    (while frame-params
      (set-alist-slot params (car (car frame-params)) (cdr (car frame-params)))
      (setq frame-params (cdr frame-params)))
    (setq frame (make-frame params))
    (mapc (lambda (face)
            (set-face-foreground (car face) "green" frame)
            (set-face-background (car face) "black" frame))
          (frame-face-alist frame))
    ;; Now fix up some of the faces to be more sensible, e.g. flipping fg/bg
    (set-basic-frame-color "green" frame)
    frame))

;;;###autoload
(defun make-frobbity-frame (&optional name)
  "Make a frame with menu bar, scroll bars, etc."
  (interactive (list (and current-prefix-arg
                          (read-string "Frame name: "))))
  (or name (setq name (cdr (assq 'name default-frame-alist))))
  (make-frame `((name                 . ,name)
                (icon-name            . ,name)
                (title                . ,name)
                (menu-bar-lines       . 1)
                (tool-bar-lines       . 3)
                (vertical-scroll-bars . t))))

;;;###autoload
(defun make-large-simple-frame
  (&optional width height x-offset y-offset title color display)
  "Create a large frame free of most GUI elements (menus, toolbars, etc).
If optional parameters WIDTH or HEIGHT are not specified, create a frame
occupying as much horizontal and/or vertical space as possible.

If the display on which the frame is to be created has a vertical
  resolution of more than 1280 pixels (e.g. 1920x1440), the frame is created
  with a 10x20 fixed width font.
If the display on which the frame is to be created has a vertical
  resolution of 1024 or more pixels (e.g. 1280x1024), the frame is created
  with a 9x15 fixed width font.
On smaller displays, a 6x13 fixed width font is used.

If X-OFFSET is positive, create frame on display with that offset in pixels
from the left edge of the display.  If offset is negative, offset the frame
from the right edge of the display.
Y-OFFSET treated similarly for top and bottom edges.

If TITLE is non-nil, use the title \"notitle\" during initial frame
creation; it is reset later.  The user's window manager can be configured
not to put a title bar across the top of the window when this title name is
chosen; otherwise there should be no effect.

Optional arguments COLOR and DISPLAY specify the frame color for most
attributes (text, mode line, mouse, cursor) and the X display,
respectively."
  (interactive)
  (let* ((display-height (x-display-pixel-height display))
         (display-width  (x-display-pixel-width  display))
         (font-dim (cond ((>  display-height 1280) '(10 . 20))
                         ((>= display-height 1024) '( 9 . 15))
                         (t                        '( 6 . 13))))
         (border-width (or (assq 'border-width default-frame-alist) 1))
         (frame-height (or height
                           (/ (- display-height (* 2 border-width))
                              (cdr font-dim))))
         ;; the fringe is an Emacs 21 window region
         (fringe-width (if (facep 'fringe) (* 2 (car font-dim)) 0))
         (frame-width (or width
                          (/ (- display-width (* 2 border-width) fringe-width)
                             (car font-dim))))
         (h-offset (cond ((null x-offset)
                          (setq x-offset 0))
                         ((< x-offset 0)
                          (setq x-offset (- display-width
                                            ;; frame width in pixels
                                            (* (car font-dim) frame-width)
                                            fringe-width
                                            (* 2 border-width)
                                            (- x-offset))))
                         (t x-offset)))
         (v-offset (cond ((null y-offset)
                          (setq y-offset 0))
                         ((< y-offset 0)
                          (setq y-offset (- display-height
                                            ;; frame height in pixels
                                            (* (cdr font-dim) frame-height)
                                            (* 2 border-width)
                                            (- y-offset))))
                         (t y-offset)))
         (frame-title (cond ((null title) "notitle")
                            ((symbolp title)
                             (cdr (assq 'name default-frame-alist)))
                            (t title)))
         (frame-params `((name             . ,frame-title)
                         (icon-name        . ,frame-title)
                         (title            . ,frame-title)
                         (font             . ,(format "%dx%d"
                                                      (car font-dim)
                                                      (cdr font-dim)))
                         (menu-bar-lines   . 0)
                         (tool-bar-lines   . 0)
                         (vertical-scroll-bars . nil)
                         (width            . ,frame-width)
                         (height           . ,frame-height)
                         (left             . ,h-offset)
                         (top              . ,v-offset)
                         (border-color     . "black")))
         frame)

    (setq frame (if display
                    (make-frame-on-display display frame-params)
                  (make-frame frame-params)))

    ;; If the frame was created with the title "notitle" to keep the window
    ;; manager from putting a title bar on it, now change it to something
    ;; more meaningful now that the frame exists.
    (and (string= frame-title "notitle")
         (set-frame-titles (format "emacs@%s" system-name) frame))

    (set-basic-frame-color (or color "white") frame)
    (set-face-background 'highlight           "black"         frame)
    (set-face-foreground 'highlight           "white"         frame)
    (set-face-background 'region              "blue"          frame)
    (set-face-foreground 'region              "white"         frame)
    (set-face-background 'secondary-selection "darkslateblue" frame)
    (set-face-foreground 'secondary-selection "white"         frame)
    frame))

(provide 'frame-fns)

;;; frame-fns.el ends here.
