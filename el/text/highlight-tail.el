;;; Saved through ges-version 0.3.3dev at 2004-05-14 12:16
;;; ;;; From: Rafal Jedruszek <necui@nic-nac-project.de>
;;; ;;; Subject: Re: highlight-tail.el --- draw a "tail" while writing by smoothly
;;; ;;;  changing background color
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Mon, 19 Apr 2004 18:53:31 +0200

;;; [1. text/plain]

;;; There is a new version out.  Next versions will be available
;;; online:
;;; <url:http://nic-nac-project.de/~necui/AGONIA/page~highlight-tail.el/css~elisp>

;;; The main change is speed improvement (thx goes to Phillip Lord
;;; for some advices in this sphere).

;;; [2. application/emacs-lisp; highlight-tail.el]

;;; highlight-tail.el --- draw a "tail" while writing, by smoothly changing background color

;; Author: Rafal Jedruszek <necui@nic-nac-project.de>
;; Keywords: highlight tail burn
;; Favourite-brand-of-beer: None, I hate beer.

(defconst highlight-tail-version "0.9" "The current version of `highlight-tail-mode'.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; === WHAT IS IT?
;;
;; This minor-mode draws a tail in real time, when you write.  It
;; changes the background color of some last typed characters and
;; smoothly fade them out to the given color (probably the background
;; color).
;;
;; So from now on, your EMACS will be even more sexy! ;o )
;;
;; --- front highlighting
;; 
;; There is also a feature of highlighting characters in front of the
;; cursor, but it's very experimental at this stage and I do not
;; recommend using it.  Well, i personally do, but I coded this option
;; and I know what I can expect from it.  So again, I do not recommend
;; using it.  Why?  Because to highlight some characters in front of
;; the cursor, we need to have some space (see:
;; `highlight-tail-const-width').
;; When we are near the end of the line, this mode needs to create
;; this space, and when the highlighting is done - this space needs to
;; be deleted, so you won't have a file with 15 blank spaces at the
;; end of each line.  And exactly here is the problem.  This deleting
;; is not perfect, so it can someday delete your whole document, then
;; crash emacs and you will need to repeat your work.
;; 
;; Well there is another problem (rather littler calibre) - the spaces
;; are not deleted sometimes.  So it's good to do some whitespace
;; cleanups in the buffer after editing.
;;
;; Also when spaces are critical in your document format (maybe some
;; column work or so) this option won't do.
;; 
;; This option is just a toy to play one time.  But I like it and use
;; it daily, so decided to add it here.
;; 
;;
;; === INSTALLATION
;;
;; Place this file in your load-path and add
;;
;; (require 'highlight-tail-mode)
;; (message "Highlight-tail-mode loaded - now your Emacs will be even more sexy!")
;; ; [ here some setq of variables ]
;; (highlight-tail-reload)
;;
;; to your ~/.emacs
;;
;; === CONFIGURATION
;;
;; The default configuration is quite good, but you could and should
;; customize it to your own needs.  Here are some examples.  It's hard
;; for me to explain what you will see when use them, so just give them
;; them a try.  Place theese setq in your .emacs.el file *before* the
;; (highlight-tail-reload) function call.
;;
;; 1. -----
;;
;; (setq highlight-tail-colors '(("black" . 0)
;;                               ("#bc2525" . 25)
;;                               ("black" . 100)))
;; 
;; 2. -----
;;
;; (setq highlight-tail-steps 14
;;       highlight-tail-timer 1)
;; 
;; 3. -----
;; 
;; (setq highlight-tail-posterior-type 'const
;;       indent-tabs-mode nil  ; it is needed for front highlighting to work good
;;       highlight-tail-highlight-front 't)
;;
;; Theese are all customizable variables.  I think you get the idea
;; how to customize this mode for best fit.
;; 
;; ATTENTION
;;
;; You will often need to run (highlight-tail-reload) to make changes
;; work.
;;
;; === SPEED
;;
;; From version 0.8 this mode doesn't use much CPU power.  There were
;; problems with this in earlier versions.  Now it takes about 2-8% of
;; my Athlon XP 2000+ power in normal speed typing (maybe it is still
;; too much? - if you have suggestions how to make it eat less - mail
;; me).  When I press a key and hold it down it eats approximately 15%
;; (in comparison to prior version (100%) it is a very good result).
;; The CPU eaten depends mainly on two variables:
;; `highlight-tail-steps' and `highlight-tail-timer'.  So combine with
;; theese two to achieve satisfactory performance.
;;
;; === X11
;;
;; You should run highlight-tail-mode under X11, not in console nor terminal
;; emulator, because results will be miserable.

;;; History:

;; * 0.9
;;      + Now under GPL.
;;      + Changed start, middle and end colors to a list of indefinable length
;;      + Added front and constant highlighting options.
;;      + Changed lists to hashes and vectors for speed (I don't really now
;;        did this help.)

;; * 0.8
;;      + Eats a lot less of CPU cycles than before.  Thanks to
;;        Phillip Lord for some advices how to achieve it.
;;      + Works good under xemacs.
;;      + Added middle color option
;;
;; * 0.7
;;      + First public version.

;;; TODO:

;; * Maybe yet some optimizations for speed.
;; * Highlighting in vertical (only for play, just like front-highlighting;
;;   this will be very slow)
;;
;; You are free to mail me, what would you like to see in this mode.
;; necui@nic-nac-project.de

;;; Variables:

(defvar highlight-tail-colors-fade-table nil
  "The list of colors for fading out.
This list is computed on every reload by
function `highlight-tail-make-colors-fade-table'.

Please do not change this variable.")

(defvar highlight-tail-colors '(("#bc2525" . 0)
                                ("#000000" . 100))
  "*List of colors through which the fading will last.
The argument for every color is a percent at which this color
will have greatest intensity.

For instance:
'((\"#bc2525\" . 0)
  (\"#000000\" . 100))")

(defvar highlight-tail-steps 80
  "*Number of fading out steps.
The color will be changed that many times until
it will reach last color from `highlight-tail-colors'.")

(defvar highlight-tail-timer 0.04
  "*Number of seconds between fade out steps.
It can be sth like this: 0.2 or maybe better 0.02")

(defvar highlight-tail-highlight-front 'nil
  "*Highlighting takes place in front of cursor too.
You can define the width of this highlight in
variable `highlight-tail-const-width'.

You will need to do:
\(setq indent-tabs-mode nil)
to this highlight work as it is intended to work.

I DO NOT RECOMMEND USING FRONT HIGHLIGHTING. IT IS EXPERIMENTAL.
Turn it on, see how it works, and then better turn it off.
You have been warned. :o )")

(defvar highlight-tail-posterior-type 't
  "*Type of highlighting.
t      - normal highlighting
const  - highlight constant width of characters
         (see `highlight-tail-const-width')
nil    - do not highlight the back (posterior) at all")

(defvar highlight-tail-const-width 10
  "*Number of chars to highlight when constant highlighting.
Makes sense only when `highlight-tail-highlight-front' is non-nil or
`highlight-tail-posterior-type' is const.")

(defvar highlight-tail-overlays-list nil
  "Hash of actually being displayed overlays and their colors.
The hash consists of lists like this one: (overlay . color).
Color is a number from which parser will build a face name.
For example color number 5 will amount to \"highlight-tail-face-5\".
The list is changed on every `highlight-tail-process-overlays-list' call.

Please do not change this variable.")

(defvar highlight-tail-front-overlays-list 'nil
  "Vector of front overlays and their colors, when front highlighting is on.
\(note `highlight-tail-highlight-front')
The vector consists of lists like this one: (overlay . color).
Color is a number from which parser will build a face name.
For example color number 5 will amount to \"highlight-tail-face-5\".
The list is changed on every `highlight-tail-process-overlays-list' call.

Please do not change this variable.")

(defconst highlight-tail-overlays-category 'highlight-tail-ownership
  "Category that will be assigned to all front overlays.")

(defvar highlight-tail-const-overlays-list nil
  "Vector of overlays and their colors, when constant highlighting is on.
\(note `highlight-tail-posterior-type')
The vector consists of lists like this one: (overlay . color).
Color is a number from which parser will build a face name.
For example color number 5 will amount to \"highlight-tail-face-5\".
The list is changed on every `highlight-tail-process-overlays-list' call.

Please do not change this variable.")

(defvar highlight-tail-update-front-overlays-to-this-list nil
  "Original colors of overlays in the front, when constant highlighting is on.
\(see `highlight-tail-highlight-front').
The `highlight-tail-front-overlays-list' will be smoothly changed to this
variable on few keypresses.

Please do not change this variable.")

(defvar highlight-tail-update-const-overlays-to-this-list nil
  "Original colors of overlays when const highlighting is on.
\(see `highlight-tail-posterior-type' variable).
The `highlight-tail-const-overlays-list' will be smoothly changed to this
variable on few keypresses.

Please do not change this variable.")

(defvar highlight-tail-face-max nil
  "Number of \"max face\" computed by `highlight-tail-make-faces'.
Max face means the color completely faded out.

Please do not change this variable.")

(defvar highlight-tail-end-of-line nil
  "Currently highlighted end of line (as marker).

Please do not change this variable.")

(defvar highlight-tail-markers-for-deletion nil
  "Vector of points to be deleted.
They will be deleted if at the end of line, no overlay at
and a space character there.
This only occurs when front highlighting is on
\(note `highlight-tail-highlight-front').

Please do not change this variable.")

(defvar highlight-tail-current-buffer nil
  "Buffer when highlighting is being done.

Please do not change this variable.")

(defvar highlight-tail-current-highlighting-line nil
  "Line when highlighting is being done.

Please do not change this variable.")

(defvar highlight-tail-internal-timer nil
  "Timer that executes color changing.
Every tick of this timer will change colors of overlays.
This variable is attributed in the program.

Please do not change this variable.")

(defvar highlight-tail-map nil
  "Keymap for highlight-tail.")

(defvar highlight-tail-mode nil
  "*The highlight-tail-mode state.")

;;; Code:

(defun highlight-tail-post-command ()
  "Check for the last command and decide to refresh highlighting or not."
  (when (and highlight-tail-mode
             (equal this-command 'self-insert-command))
    (highlight-tail-make-new-overlays)))

(defun highlight-tail-make-new-overlays ()
  "Highlight places (make/move overlays) where they should be highlighted.
It is called on every `self-insert-command'.

Also do sth with blank spaces when the line has been changed.  Set the
end of line marker, when the buffer has been switched.  Set the
current line where highlighting takes place.  All theese is needed
only when `highlight-tail-highlight-front' is non-nil."
  ;; if the mode had been just reactivated or sth like that
  (unless (member highlight-tail-internal-timer timer-list)
    (highlight-tail-reload))

  ;; begin
  (when highlight-tail-posterior-type
    (if (eq highlight-tail-posterior-type 'const)
        (progn
          ;; first run - make overlays
          (unless highlight-tail-const-overlays-list
            (highlight-tail-make-const-overlays-list))
          ;; done
          (highlight-tail-update-const-overlays-list))
      ;; not const highlighting - make new overlay in the current place
      ;; with face-value of 0 (brightest)
      (highlight-tail-make-new-overlay)))

  ;; are we going to highlight the front too?
  (when highlight-tail-highlight-front
    (unless (window-minibuffer-p)
      ;; first run - make overlays and markers
      (unless highlight-tail-front-overlays-list
        (highlight-tail-make-front-overlays-list))
      (unless highlight-tail-current-highlighting-line
        (setq highlight-tail-current-highlighting-line
              (highlight-tail-lines-to-buffer-start (point))))
      ;; make sure the end of line is from current buffer!!
      (unless (eq (current-buffer)
                  highlight-tail-current-buffer)
        (setq highlight-tail-current-buffer (current-buffer))
        (set-marker highlight-tail-end-of-line (save-excursion
                                                 (end-of-line)
                                                 (point-marker))))
      ;; if the line has been changed - purge
      (if (= highlight-tail-current-highlighting-line
             (highlight-tail-lines-to-buffer-start (point)))
          (when (and (marker-position (elt highlight-tail-markers-for-deletion
                                           0))
                     (< (1+ (point))
                        (elt highlight-tail-markers-for-deletion
                             0)))
            (highlight-tail-remove-front-overlays-and-blank-spaces))
        (setq highlight-tail-current-highlighting-line
              (highlight-tail-lines-to-buffer-start (point)))
        (highlight-tail-remove-front-overlays-and-blank-spaces)
        (set-marker highlight-tail-end-of-line (save-excursion
                                                 (end-of-line)
                                                 (point-marker))))

      ;; make place for theese overlays
      ;; if at end of frame width, do not go beyond this
      (if (or truncate-lines
              (and (< (window-width) (frame-width))
                   (equal truncate-partial-width-windows t)))
          (let* ((point (point))
                 (point-needed (+ point highlight-tail-const-width))
                 (point-last (1+ (marker-position highlight-tail-end-of-line))))
            (when (> point-needed point-last)
              (save-excursion
                (goto-char point-last)
                (insert ? ))))
        ;; if lines are not truncated (last column must be end of highlight)
        (let* ((point (point))
               (column-last (if (featurep 'xemacs)
                                (1- (window-width))
                              (window-width)))
               ;; column is a relative column (as it would be in first line)
               (column (if (> (current-column) column-last)
                           (let ((helper (current-column)))
                             (while (> helper column-last)
                               (setq helper (- helper column-last)))
                             helper)
                         (current-column)))
               (point-needed (+ point highlight-tail-const-width 1))
               (column-needed (+ column highlight-tail-const-width 1))
               (point-last (marker-position highlight-tail-end-of-line)))
          (if (>= column-needed column-last)
              (when (and (not (= point point-last))
                         (equal (char-before point-last) ? ))
                (save-excursion
                  (goto-char point-last)
                  (backward-delete-char 1)))
            (when (> point-needed (1+ point-last))
              (save-excursion
                (goto-char point-last)
                (insert ? ))))))
      (when highlight-tail-front-overlays-list
        (highlight-tail-update-front-overlays-and-markers)))))

(defun highlight-tail-make-new-overlay ()
  "Make new highlight in the current point with highest intensity."
  (let* ((end-point (point))
         (start-point (1- end-point))
         (face-number 0))
    (when (gethash start-point highlight-tail-overlays-list)
      (highlight-tail-delete-overlay (car (gethash start-point highlight-tail-overlays-list)))
      (remhash start-point highlight-tail-overlays-list))
    (puthash start-point
             (cons (highlight-tail-make-overlay start-point end-point)
                   face-number)
             highlight-tail-overlays-list)
    (let ((overlay-address (car (gethash start-point
                                         highlight-tail-overlays-list))))
      (highlight-tail-overlay-put overlay-address 'evaporate t)
      (highlight-tail-overlay-put overlay-address 'face (intern
                                                         (concat
                                                          "highlight-tail-face-"
                                                          (number-to-string face-number)))))))

(defun highlight-tail-make-front-overlays-list ()
	"Make front overlays list, that will be later operated on.
Will be operated by `highlight-tail-process-overlays-list'
\(change colors etc.).

It only occurs when `highlight-tail-highlight-front' is non-nil."
  (let* ((front-char (1- highlight-tail-const-width))
         (face-indicator (/ highlight-tail-face-max
                            (float (1+ front-char)))))
		(setq highlight-tail-front-overlays-list (make-vector highlight-tail-const-width
																													'nil))
		(setq highlight-tail-update-front-overlays-to-this-list
					(make-vector highlight-tail-const-width
											 'nil))
    (setq highlight-tail-end-of-line (save-excursion
                                       (end-of-line)
                                       (point-marker)))
    (set-marker-insertion-type highlight-tail-end-of-line t)
;; we are going from end
    (while (>= front-char 0)
      (let ((multiply-fc-fi (round (* front-char face-indicator))))
        (aset highlight-tail-front-overlays-list
              front-char
              (cons (highlight-tail-make-overlay (point) (point))
                       highlight-tail-face-max))
        (aset highlight-tail-update-front-overlays-to-this-list
              front-char
              multiply-fc-fi
              )
        (highlight-tail-overlay-put (car (elt highlight-tail-front-overlays-list front-char))
                                    'category
                                    highlight-tail-overlays-category)
        (highlight-tail-overlay-put (car (elt highlight-tail-front-overlays-list front-char))
                                    'face (intern
                                           (concat
                                            "highlight-tail-face-"
                                            (number-to-string highlight-tail-face-max))))
        (setq front-char (1- front-char))))))

(defun highlight-tail-make-markers-for-deletion-vector ()
  "Clear and then make markers for deletion vector.

It only occurs when `highlight-tail-highlight-front' is non-nil."
  (let ((count 0))
    ;; delete all markers
    (if highlight-tail-markers-for-deletion
        (while (< count (length highlight-tail-markers-for-deletion))
          (set-marker (aref highlight-tail-markers-for-deletion
                            count)
                      'nil)
          (setq count (1+ count))))
    (setq highlight-tail-markers-for-deletion
          (make-vector highlight-tail-const-width
                       nil))
    (setq count 0)
    (while (< count highlight-tail-const-width)
      (aset highlight-tail-markers-for-deletion
            count
            (make-marker))
      (setq count (1+ count)))
))

(defun highlight-tail-update-front-overlays-and-markers ()
  "Update front overlays list (colors to max, positions etc.)
Move markers to new position too, by calling function
`highlight-tail-markers-for-deletion-here'.

It only occurs when `highlight-tail-highlight-front' is non-nil."
  (let ((count 0))
    (set-marker highlight-tail-end-of-line (save-excursion
                                             (end-of-line)
                                             (point)))
    ;; highlight-tail-end-of-line variable must be set before this:
    (highlight-tail-markers-for-deletion-here (point))
    (while (< count (length highlight-tail-front-overlays-list))
      (let ((front-place (+ (point) count)))
        (if (and (or (< (+ (current-column) count) (1- (frame-width)))
                     (> (current-column) (1- (frame-width))))
                 (< front-place (highlight-tail-line-end-position
                                 (point))))
            ;; if not on list
            (highlight-tail-move-overlay (car (elt highlight-tail-front-overlays-list count))
                                         front-place (1+ front-place)
                                         (current-buffer))
          ;; move to current-buffer to not blink in other buffer
          ;; it is good for minibuffer
          (highlight-tail-move-overlay (car (elt highlight-tail-front-overlays-list count))
                                       1 1
                                       (current-buffer))))
      (let ((new-value (round (- (cdr (elt highlight-tail-front-overlays-list count))
                                 (* (- highlight-tail-face-max
                                       (elt highlight-tail-update-front-overlays-to-this-list count))
                                    0.15)))))
        (if (< new-value (elt highlight-tail-update-front-overlays-to-this-list count))
            (setq new-value (elt highlight-tail-update-front-overlays-to-this-list count)))
        (setcdr (elt highlight-tail-front-overlays-list count) new-value))
      (setq count (1+ count)))))

(defun highlight-tail-markers-for-deletion-here (point)
  "Place `highlight-tail-const-width' amount of markers in front of the cursor.
Argument POINT is the cursor position."
  (let ((count 0)
        helper)
    (while (< count highlight-tail-const-width)
      (setq helper (+ point count 1))
      ;; keep theese markers in one line
      (if (<= helper highlight-tail-end-of-line)
          (set-marker (elt highlight-tail-markers-for-deletion count)
                      helper))
      (setq count (1+ count)))))

(defun highlight-tail-make-const-overlays-list ()
  "Make constant overlays list, that will be later operated on.
Will be operated by `highlight-tail-process-overlays-list'
\(change colors etc.)

It only occurs when `highlight-tail-posterior-type' is 'const."
  (let* ((const-char (1- highlight-tail-const-width))
         (face-indicator (/ highlight-tail-face-max
                            (float (1+ const-char)))))
    (setq highlight-tail-const-overlays-list (make-vector highlight-tail-const-width
                                                          'nil))
    (setq highlight-tail-update-const-overlays-to-this-list
          (make-vector highlight-tail-const-width
                       'nil))
;; we are going from end
    (while (>= const-char 0)
      (let ((multiply-fc-fi (round (* const-char face-indicator))))
        (aset highlight-tail-const-overlays-list
              const-char
              (cons (highlight-tail-make-overlay (point) (point))
                       highlight-tail-face-max))
        (aset highlight-tail-update-const-overlays-to-this-list
              const-char
              multiply-fc-fi)
        (highlight-tail-overlay-put (car (elt highlight-tail-const-overlays-list const-char))
                                    'face (intern
                                           (concat
                                            "highlight-tail-face-"
                                            (number-to-string highlight-tail-face-max))))
        (setq const-char (1- const-char))))))

(defun highlight-tail-update-const-overlays-list ()
  "Update constant overlays list (colors, positions etc.)

It only occurs when `highlight-tail-posterior-type' is 'const."
  (let ((count 0))
    (while (< count (length highlight-tail-const-overlays-list))
      (let ((const-place (- (point) count)))
        (if (< (- (point) const-place)
               (current-column))
            (highlight-tail-move-overlay (car (elt highlight-tail-const-overlays-list count))
                          const-place (1- const-place)
                          (current-buffer))
          ;; move to current-buffer to not blink in other buffer
          ;; it is good for minibuffer
          (highlight-tail-move-overlay (car (elt highlight-tail-const-overlays-list count))
                        1 1
                        (current-buffer))))
      (let ((new-value (round (- (cdr (elt highlight-tail-const-overlays-list count))
                                 (* (- highlight-tail-face-max
                                       (elt highlight-tail-update-const-overlays-to-this-list count))
                                    0.15)))))
        (if (< new-value (elt highlight-tail-update-const-overlays-to-this-list count))
            (setq new-value (elt highlight-tail-update-const-overlays-to-this-list count)))
        (setcdr (elt highlight-tail-const-overlays-list count) new-value))
      (setq count (1+ count)))))

(defun highlight-tail-process-overlays-list ()
  "Go through all overlays and make sth with them.
Such as compute new faces, purge old overlays etc.

This is called every `highlight-tail-timer' amount of time."
  (sit-for 0)
  (let ((count 0))
    ;; if mode had been just disabled - delete all overlays
    ;; and cancel the timer
    (if (not highlight-tail-mode)
        (highlight-tail-tide-up))

    ;; if there are some overlays
    (when highlight-tail-posterior-type
      (if (eq highlight-tail-posterior-type 'const)
          ;; list of const overlays
      (when highlight-tail-const-overlays-list
        (setq count 0)
        (while (< count highlight-tail-const-width)
          (let ((cur-face-number (cdr (elt highlight-tail-const-overlays-list count))))
            ;; number < `highlight-tail-face-max'
            (if (not (= cur-face-number
                        highlight-tail-face-max))
                (progn
                  (setq cur-face-number (1+ cur-face-number))
                  (setcdr (elt highlight-tail-const-overlays-list count)
                          cur-face-number)
                  (highlight-tail-overlay-put (car (elt highlight-tail-const-overlays-list count))
                                              'face
                                              (intern
                                               (concat "highlight-tail-face-"
                                                       (number-to-string cur-face-number)))))
              (highlight-tail-move-overlay (car (elt highlight-tail-const-overlays-list count))
                                           1 1
                                           (current-buffer)))

            (setq count (1+ count)))))
      ;; if not const highlighting
      (when highlight-tail-overlays-list
        (maphash 'highlight-tail-overlays-list-hash-process
                 highlight-tail-overlays-list))))

    ;; now the list of front overlays
    (when (and highlight-tail-highlight-front
               highlight-tail-front-overlays-list)
      (setq count 0)
      (while (< count highlight-tail-const-width)
        (let ((cur-face-number (cdr (elt highlight-tail-front-overlays-list count))))
          ;; number < `highlight-tail-face-max'
          (if (not (= cur-face-number
                      highlight-tail-face-max))
              (progn
                (setq cur-face-number (1+ cur-face-number))
                (setcdr (elt highlight-tail-front-overlays-list count)
                        cur-face-number)
                (highlight-tail-overlay-put (car (elt highlight-tail-front-overlays-list count))
                                            'face
                                            (intern
                                             (concat "highlight-tail-face-"
                                                     (number-to-string cur-face-number)))))
            (highlight-tail-move-overlay (car (elt highlight-tail-front-overlays-list count))
                                         1 1
                                         (current-buffer))))
        (setq count (1+ count)))
      (highlight-tail-process-markers-for-deletion))))

(defun highlight-tail-process-markers-for-deletion ()
  "Do something with markers, so they won't stay in one place forever.
Also delete unneeded blank spaces at the end of lines."
  (let ((count (1- highlight-tail-const-width))
        (point (point))
        marker
        point-to-del
        line-end-position)
    (while (>= count 0)
      (setq marker (elt highlight-tail-markers-for-deletion
                        count))
      (setq point-to-del (marker-position marker))
      (when point-to-del
        (setq line-end-position (highlight-tail-line-end-position point-to-del))
        (if (= (highlight-tail-lines-to-buffer-start point-to-del)
               (highlight-tail-lines-to-buffer-start point))
            ;; if the same line
            (if (<= point-to-del point)
                (set-marker marker 'nil)
              (progn
                ;; if there aren't any disturbed overlays
                (unless (highlight-tail-own-overlays-at (1- point-to-del))
                  (when (and (= point-to-del line-end-position)
                             (equal (char-before point-to-del) ? ))
                    (set-marker marker 'nil)
                    (save-excursion
                      (goto-char point-to-del)
                      (backward-delete-char 1))))))
          ;; if other line
          (set-marker marker 'nil)
          (when (and (= point-to-del line-end-position)
                     (equal (char-before point-to-del) ? ))
            (save-excursion
              (goto-char point-to-del)
              (backward-delete-char 1))
            (setq point (point)))))
      (setq count (1- count)))))

(defun highlight-tail-overlays-list-hash-process (key value)
  "Process every KEY in `highlight-tail-overlays-list'."
  (let ((cur-face-number (cdr value)))
    (if (not (= cur-face-number
                highlight-tail-face-max))
        (progn
          (setq cur-face-number (1+ cur-face-number))
          (setcdr value cur-face-number)
          (highlight-tail-overlay-put (car value)
                                      'face
                                      (intern
                                       (concat "highlight-tail-face-"
                                               (number-to-string cur-face-number)))))
      ;; number == `highlight-tail-face-max'
      (progn
      (highlight-tail-delete-overlay (car value))
      (remhash key highlight-tail-overlays-list)))))


(defun highlight-tail-make-colors-fade-table ()
  "Compute a list of colors that will smoothly change from one to another.
The list is stored in variable `highlight-tail-colors-fade-table'.

Look at `highlight-tail-colors' too."
  (let ((count 1))
    (setq highlight-tail-colors-fade-table 'nil)
    (while (< count (length highlight-tail-colors))
      (let ((steps (- (* (/ (float (cdr (nth count highlight-tail-colors)))
                            100)
                         highlight-tail-steps)
                      (* (/ (float (cdr (nth (1- count) highlight-tail-colors)))
                            100)
                         highlight-tail-steps))))
      (setq highlight-tail-colors-fade-table
            (append (highlight-tail-find-colors-list
                     (car (nth (1- count) highlight-tail-colors))
                     (car (nth count highlight-tail-colors))
                     steps)
                    highlight-tail-colors-fade-table)))
      (setq count (1+ count)))
    (setq highlight-tail-colors-fade-table
          (reverse highlight-tail-colors-fade-table))))

(defun highlight-tail-find-colors-list (color-from color-to steps-count)
  "Create a list of smoothly changed colors;
From COLOR-FROM to COLOR-TO; STEPS-COUNT length."
  (let (color-from-red                  ; --
        color-from-green                ; --
        color-from-blue                 ; --
        color-to-red                    ; --
        color-to-green                  ; --
        color-to-blue                   ; --
        color-red                       ; the color we are
        color-green                     ; looking for
        color-blue                      ;
        color-step-red ; the color is smoothly changing; we'll calculate a value
        color-step-green ; that will be added to COLOR-FROM at every single step,
        color-step-blue ; multiplied by current step number of course.
        color-step-red-plus             ; \
        color-step-green-plus ;  = will values be positive of negative
        color-step-blue-plus            ; /
        color-red-difference            ; \
        color-green-difference ;  = differences between FROM and TO values
        color-blue-difference           ; /
        result-list
        (step 1))
    (if (highlight-tail-color-in-hex-format color-from)
        (setq color-from-red (string-to-number (substring color-from 1 3) 16)
              color-from-green (string-to-number (substring color-from 3 5) 16)
              color-from-blue (string-to-number (substring color-from 5 7) 16 ))
      (let ((temp-color (highlight-tail-choose-color-from-list-by-name color-from)))
      (setq color-from-red (nth 1 temp-color))
      (setq color-from-green (nth 2 temp-color))
      (setq color-from-blue (nth 3 temp-color))))
    (if (highlight-tail-color-in-hex-format color-to)
        (setq color-to-red (string-to-number (substring color-to 1 3) 16)
              color-to-green (string-to-number (substring color-to 3 5) 16)
              color-to-blue (string-to-number (substring color-to 5 7) 16))
      (let ((temp-color (highlight-tail-choose-color-from-list-by-name color-to)))
      (setq color-to-red (nth 1 temp-color))
      (setq color-to-green (nth 2 temp-color))
      (setq color-to-blue (nth 3 temp-color))))

    (setq ;; compute difference of COLOR-FROM and COLOR-TO
     color-red-difference (abs (- color-from-red color-to-red))
     color-green-difference (abs (- color-from-green color-to-green))
     color-blue-difference (abs (- color-from-blue color-to-blue))
     ;; compute what every single step of fading will change
     color-step-red (/ (float color-red-difference) steps-count)
     color-step-green (/ (float color-green-difference) steps-count)
     color-step-blue (/ (float color-blue-difference) steps-count)
     ;; check if step values should be positive or negative
     color-step-red-plus (>= color-to-red color-from-red)
     color-step-green-plus (>= color-to-green color-from-green)
     color-step-blue-plus (>= color-to-blue color-from-blue)
     )
    ;; if desirable - make values negative
    (if (not color-step-red-plus) (setq color-step-red (* color-step-red -1)))
    (if (not color-step-green-plus) (setq color-step-green (* color-step-green -1)))
    (if (not color-step-blue-plus) (setq color-step-blue (* color-step-blue -1)))

    ;; now compute the list ;;
    ;; we have colors in red, green and blue values;
    (while (<= step steps-count)
      (setq color-red (+ color-from-red (* color-step-red step))
      color-green (+ color-from-green (* color-step-green step))
      color-blue (+ color-from-blue (* color-step-blue step)))
      (setq result-list
      (cons (highlight-tail-build-hex-color color-red color-green color-blue)
            result-list))
      (setq step (1+ step)))
    result-list))

(defun highlight-tail-choose-color-from-list-by-name (color-name)
  "Find a COLOR-NAME in list `color-name-rgb-alist'."
  ;; throw out spaces, because there are no in `color-name-rgb-alist'
  (let ((colors-list color-name-rgb-alist)
        color-element) ; single element found in emacs list of colors `color-name-rgb-alist'
                                        ; it'll be like: ("ghostwhite" 248 248 255))
    (while colors-list
      (if (equal (replace-regexp-in-string " " "" color-name) (car (car colors-list)))
      (setq color-element (car colors-list)))
      (setq colors-list (cdr colors-list)))
    color-element))

(defun highlight-tail-build-hex-color (red green blue)
  "Build a color like #00FF00 from given RED, GREEN and BLUE.
For example: 0 255 0 will result in #00FF00."
  (format "#%02X%02X%02X" (round red) (round green) (round blue)))

(defun highlight-tail-color-in-hex-format (color)
  "Find out if COLOR is in hex format or not."
  (string-equal (if (featurep 'xemacs)
                    (replace-in-string color
                                       "#[0-9a-fA-F]\\{6\\}"
                                       "")
                  (replace-regexp-in-string "#[0-9a-fA-F]\\{6\\}"
                  ""
                  color))
                ""))

(defun highlight-tail-make-faces (colors-list)
  "Make faces from list of colors.
Faces will be named: highlight-tail-face-X, where X is
a position number in COLORS-LIST.

Set `highlight-tail-face-max' variable too."
  (let ((colors-list colors-list)
        (count 0)
        highlight-tail-face-name)
    (while (< count (length colors-list))
      (setq highlight-tail-face-name
      (intern (concat "highlight-tail-face-" (number-to-string count))))
      (make-face highlight-tail-face-name)
      (set-face-background highlight-tail-face-name (nth count colors-list))
      (setq count (1+ count)))
    (setq highlight-tail-face-max (1- count))))

(defun highlight-tail-lines-to-buffer-end (point)
  "Find out how many lines to the end of the buffer from POINT."
  (1+ (- (count-lines (point-min) (point-max))
         (count-lines (point-min) point))))

(defun highlight-tail-lines-to-buffer-start (point)
  "Find out how many lines to the beginning of the buffer from POINT."
  (let (result)
    (setq result (1- (count-lines (point-min) point)))
    (save-excursion
      (goto-char point)
      (if (= (highlight-tail-line-beginning-position)
             point)
          (1+ result)
        result))))

(defun highlight-tail-end-of-line (&optional N)
  "Move to the end of line.
Taking into consideration that there may be some spaces
added by front highlighting.  (note `highlight-tail-highlight-front').

Argument N is relayed to function `end-of-line' (which will be called)."
  (interactive)
  (if N
      (end-of-line N)
    (end-of-line))
  (when highlight-tail-highlight-front
    (let ((count 0))
      (while (< count highlight-tail-const-width)
        (when (and (highlight-tail-own-overlays-at (1- (point)))
                   (eq (char-before (point))
                       ? ))
          (backward-char 1))
        (setq count (1+ count))))))

(defadvice basic-save-buffer (before highlight-tail-remove-blanks first)
  "Before saving the buffer, divest blank front-highlighted spaces."
  (highlight-tail-remove-front-overlays-and-blank-spaces))

(defun highlight-tail-remove-front-overlays-and-blank-spaces ()
  "Remove front overlays and blank spaces.
It is useful to hook this function before file saving, before sending
mails etc."
  (when (and highlight-tail-highlight-front
             highlight-tail-front-overlays-list)
    (let ((count 0))
      (while (< count (length highlight-tail-front-overlays-list))
        (let ((front-place (+ (point) count)))
          (highlight-tail-move-overlay (car (elt highlight-tail-front-overlays-list count))
                                       1 1
                                       (current-buffer)))
        (setq count (1+ count))))
    (highlight-tail-process-markers-for-deletion)))

(defun highlight-tail-line-beginning-position ()
  "Return the character position of the first character on the current line."
  (if (featurep 'xemacs)
      (save-excursion
        (beginning-of-line)
        (point))
    (line-beginning-position)))

(defun highlight-tail-line-end-position (n)
  "Return the character position of the last character in the line where point N live."
  (save-excursion
    (goto-char n)
    (if (featurep 'xemacs)
        (progn
          (end-of-line)
          (point))
      (line-end-position))))

(defun highlight-tail-own-overlays-at (point)
  "Say if there are front-highlighting overlays at POINT."
  (let ((count 0)
        (overlays (highlight-tail-overlays-at point))
        ownership)
    (while (< count (length overlays))
      (if (equal (highlight-tail-overlay-get (nth count overlays) 'category)
                 highlight-tail-overlays-category)
      (progn
        (setq ownership t)
        (setq count (length overlays))))
      (setq count (1+ count)))
    ownership))

(if (featurep 'xemacs)
    (progn
      (defalias 'highlight-tail-make-overlay 'make-extent)
      (defalias 'highlight-tail-overlays-at 'extents-at)
      (defalias 'highlight-tail-overlay-put 'set-extent-property)
      (defalias 'highlight-tail-overlay-get 'extent-property)
      (defalias 'highlight-tail-delete-overlay 'delete-extent)
      (defalias 'highlight-tail-move-overlay 'set-extent-endpoints)
      (defalias 'highlight-tail-overlay-end 'extent-end-position)
      (defalias 'highlight-tail-overlay-start 'extent-start-position))
  (progn
    (defalias 'highlight-tail-make-overlay 'make-overlay)
    (defalias 'highlight-tail-overlays-at 'overlays-at)
    (defalias 'highlight-tail-overlay-put 'overlay-put)
    (defalias 'highlight-tail-overlay-get 'overlay-get)
    (defalias 'highlight-tail-delete-overlay 'delete-overlay)
    (defalias 'highlight-tail-move-overlay 'move-overlay)
    (defalias 'highlight-tail-overlay-end 'overlay-end)
    (defalias 'highlight-tail-overlay-start 'overlay-start)))

(defun highlight-tail-tide-up (&optional first-run)
  "Delete all overlays, cancel the timer, and so on...
if FIRST-RUN do not delete the timer."
  (let ((count 0))
    (unless first-run
      (maphash 'highlight-tail-overlays-list-tide-up
               highlight-tail-overlays-list)
      (clrhash highlight-tail-overlays-list)
      (ad-deactivate 'basic-save-buffer))
    (while (< count (length highlight-tail-front-overlays-list))
      (highlight-tail-delete-overlay (car (elt highlight-tail-front-overlays-list count)))
      (setq count (1+ count)))
    (when highlight-tail-highlight-front
      (highlight-tail-remove-front-overlays-and-blank-spaces))
    (when highlight-tail-markers-for-deletion
      (setq highlight-tail-markers-for-deletion 'nil))
    (setq count 0)
    (while (< count (length highlight-tail-const-overlays-list))
      (highlight-tail-delete-overlay (car (elt highlight-tail-const-overlays-list count)))
      (setq count (1+ count)))
    (setq highlight-tail-overlays-list 'nil)
    (setq highlight-tail-const-overlays-list 'nil)
    (setq highlight-tail-front-overlays-list 'nil)
    (setq highlight-tail-update-front-overlays-to-this-list 'nil)
    (setq highlight-tail-update-const-overlays-to-this-list 'nil)
    (unless first-run
      (if (featurep 'xemacs)
          (delete-itimer highlight-tail-internal-timer)
        (cancel-timer highlight-tail-internal-timer)))))

(defun highlight-tail-overlays-list-tide-up (key value)
  "Delete all overlays from `highlight-tail-overlays-list'."
  (if (overlayp (car value))
      (highlight-tail-delete-overlay (car value))))

(defun highlight-tail-reload ()
  "Recreate color-fade-table, faces, add proper hook, turn on `highlight-tail-mode', and os on...
Run it, when you've made changes to some highlight-tail-mode variables."
  ;; if there is no `color-name-rgb-alist' var and some color is not in hex - call out an error
  (if (not (boundp 'color-name-rgb-alist))
      (let ((count 0))
        (while (< count (length highlight-tail-colors))
          (if (not (highlight-tail-color-in-hex-format
                    (car (nth count highlight-tail-colors))))
              (error "Some color set badly; must be in format #FFFFFF"))
          (setq count (1+ count)))))

  (highlight-tail-mode 1)
  ; make maps
  (unless highlight-tail-map
    (setq highlight-tail-map (make-sparse-keymap))
    (substitute-key-definition
     'end-of-line 'highlight-tail-end-of-line highlight-tail-map global-map)
    (push (cons 'highlight-tail-mode highlight-tail-map)
          minor-mode-map-alist))

  (ad-activate 'basic-save-buffer)

  (highlight-tail-tide-up t)

  (setq highlight-tail-overlays-list (make-hash-table))
  (if highlight-tail-highlight-front
      (highlight-tail-make-markers-for-deletion-vector))
;      (setq highlight-tail-markers-for-deletion (make-hash-table)))
  (highlight-tail-make-colors-fade-table)
  (highlight-tail-make-faces highlight-tail-colors-fade-table)
  (setq highlight-tail-internal-timer (run-at-time nil highlight-tail-timer
                                                        'highlight-tail-process-overlays-list))
  (add-hook 'post-command-hook 'highlight-tail-post-command))

(defun highlight-tail-mode (arg)
  "Draw a \"tail\" while you're typing.

This minor-mode draws a tail in real time, when you write.  It
changes the background color of some last typed characters and
smoothly fade them out.

There is also an option for highlighting some chars in front
of the cursor.  But it is not perfect,  I think this should
not be used.

If ARG is 0 or less than zero then the mode will be disabled.
If ARG is nil then the mode will be switched.
If ARG is greater than zero then this mode will be turned on."
  (interactive "P")
  (setq highlight-tail-mode
        (if (null arg) (not highlight-tail-mode)
          (> (prefix-numeric-value arg) 0)))
  (add-to-list 'minor-mode-alist '(highlight-tail-mode " ht"))
  (if (interactive-p)
      (if highlight-tail-mode
          (progn
            (add-hook 'post-command-hook 'highlight-tail-post-command)
            (message "Highlight tail mode enabled"))
        (progn
          (highlight-tail-tide-up)
          (message "Highlight tail mode disabled")))))

(provide 'highlight-tail)

;;; highlight-tail.el ends here
;;; [3. text/plain]


 
;;; ;;; -- 
;;; ;;; Zabiæ j± ! - przestrog± dla mych zbyt ambitnych marzeñ ! / studni± |
;;; ;;; Zabiæ j± ! - zatrzymuje obieg mojej wiary !      ,______/ d¼wiêków |
;;; ;;; Zabiæ j± ! - dotykiem czyim¶ muszê pomóc sobie !/- homepage: - - - |
;;; ;;; Zabiæ j± ! - mnie ju¿ nie bêdzie boleæ... !    /http://42.pl/url/M6|

