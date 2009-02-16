;;; misc-cmds.el --- Miscellaneous commands (interactive functions).
;;
;; Filename: misc-cmds.el
;; Description: Miscellaneous commands (interactive functions).
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Wed Aug  2 11:20:41 1995
;; Version: 21.1
;; Last-Updated: Sat Aug 19 17:37:43 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 2155
;; URL: http://www.emacswiki.org/cgi-bin/wiki/misc-cmds.el
;; Keywords: internal, unix, extensions, maint, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-fns', `misc-cmds', `misc-fns'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Miscellaneous commands (interactive functions).
;;
;;  Main new functions defined here:
;;
;;    `beginning-of-line+', `beginning-or-indentation', `chgrp',
;;    `chmod', `chown', `clear-regexp-search-history',
;;    `clear-regexp-search-ring' `clear-search-history',
;;    `clear-search-ring', `clear-search-histories',
;;    `count-chars-in-region', `current-line', `delete-lines',
;;    `end-of-line+', `forward-char-same-line', `forward-overlay',
;;    `goto-long-line', `goto-longest-line',
;;    `goto-previous-global-mark', `goto-previous-mark',
;;    `indirect-buffer', `kill-buffer-and-its-windows',
;;    `line-number-at-pos', `no-op', `read-shell-file-command',
;;    `region-length', `region-to-buffer', `region-to-file',
;;    `selection-length', `view-X11-colors', `yank-secondary'.
;;
;;  Suggested key bindings:
;;
;;   (global-set-key [(control meta ?y)] 'yank-secondary)
;;   (define-key ctl-x-map "w" 'region-to-file)
;;   (global-set-key [C-tab] 'goto-previous-mark)
;;   (global-set-key [C-M-tab] 'goto-previous-global-mark)
;;   (global-set-key [C-S-f1] 'region-to-buffer)
;;   (global-set-key [C-S-backspace] 'region-to-file)
;;   (global-set-key [home] 'backward-line-text)
;;   (substitute-key-definition 'kill-buffer
;;                              'kill-buffer-and-its-windows global-map)
;;   (substitute-key-definition 'beginning-of-line 'beginning-of-line+ global-map)
;;   (substitute-key-definition 'end-of-line 'end-of-line+ global-map)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/08/19 dadams
;;     Added: goto-long(est)-line, line-number-at-pos.
;; 2006/02/11 dadams
;;     Added: region-length (selection-length, count-chars-in-region).
;; 2006/01/28 dadams
;;     Added: clear(-regexp)-search-history, clear-search-histories.
;; 2006/01/01 dadams
;;     defsubst -> defun.
;; 2005/07/15 dadams
;;     Moved delete-lines back here.
;; 2005/07/14 dadams
;;     forward-overlay: ensure arg is a number.
;; 2005/07/12 dadams
;;     forward-char-same-line: Convert raw prefix arg to numeric before arithmetic.
;; 2005/07/10 dadams
;;     Removed delete-lines (moved to icicles.el and renamed icicles-delete-lines).
;; 2005/05/28 dadams
;;     region-to-buffer: Use another-buffer, if available.
;; 2005/05/09 dadams
;;     Renamed: flash-ding-minibuffer-frame to 1on1-flash-ding-minibuffer-frame.
;; 2005/01/20 dadams
;;     Removed exit-with-confirmation (use kill-emacs-query-functions in setup.el).
;; 2004/11/16 dadams
;;     Replaced beginning-of-line*, end-of-line* with + versions.
;; 2004/11/14 dadams
;;     Added beginning-or-indentation, beginning-of-line*, end-of-line*.
;; 2000/11/28 dadams
;;     Optional require's via 3rd arg=t now.
;; 1999/04/13  dadams
;;     Added: delete-lines.
;; 1999/03/17 dadams
;;     1. Protect with fboundp.
;;     2. kill-buffer-and-its-windows: use get-buffer-window-list.
;;     3. Commented out: xwud, display-xwd-image-file, xwd,
;;        capture-image-as-xwd-file, display-buffer.
;; 1996/06/03 dadams
;;     display-xwd-image-file: Do via background processes:
;;                             shell-command -> start-process-shell-command.
;; 1996/06/03 dadams
;;     display-xwd-image-file:
;;       1. Allow XWD-FILE arg as list.  Added DIR arg.
;;       2. No longer provide -noclick option by default.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/24 dadams
;;     Added: read-shell-file-command, chmod, chgrp, chown.
;; 1996/04/23 dadams
;;     Added display-xwd-image-file (xwud) and capture-image-as-xwd-file (xwd).
;; 1996/04/23 dadams
;;     Added: goto-previous-mark, goto-previous-global-mark.
;; 1996/04/16  dadams
;;     Added declp-buffer-w-switches and declp-region-w-switches.
;; 1996/03/20 dadams
;;     no-op, exit-with-confirmation, view-X11-colors, forward-overlay,
;;     declp-buffer, declp-region, yank-secondary: defun -> defsubst
;; 1996/02/28 dadams
;;     Added forward-overlay, forward-char-same-line.
;; 1996/02/15 dadams
;;     Added yank-secondary.
;; 1996/02/06 dadams
;;     Put variable-interactive property on appropriate user option vars.
;; 1996/02/05 dadams
;;     1. Added: default-pr-switches, declp-switches, declp-sheet-options.
;;     2. declp-buffer,declp-region,pr-declp-buffer,pr-declp-region: Optional args.
;;     3. pr-declp-buffer, pr-declp-region, declp-region-1:
;;        Proper treatment of pr switches; pr error treatment; No BSD lpr shortcut.
;; 1996/01/25 dadams
;;     kill-buffer-and-its-windows: Added args to call to windows-on.
;; 1996/01/16 dadams
;;     Added: read-number-up, declp-buffer, declp-region, pr-declp-buffer,
;;            pr-declp-region.
;; 1996/01/12 dadams
;;     Added region-to-buffer, region-to-file.
;; 1996/01/08  dadams
;;     Added redefinition of display-buffer that raises frame.
;; 1995/08/24 dadams
;;     1) Added view-X11-colors.  2) flash-ding -> flash-ding-minibuffer-frame.
;; 1995/08/18 dadams
;;     1) Added no-op and local version of print-region-1.
;; 1995/08/08 dadams
;;     Added: exit-with-confirmation, lpr stuff.
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

(and (< emacs-major-version 21)         ;; dolist, pop
     (eval-when-compile (require 'cl))) ;; (plus, for Emacs <20: when, unless)

(require 'frame-fns nil t) ;; (no error if not found): flash-ding
(require 'misc-fns nil t) ;; (no error if not found) another-buffer

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-cmds)
(require 'misc-cmds)                 ; Ensure loaded before compile this.

;;;;;;;;;;;;;;;;;;;;;;;

(defun no-op (&rest arguments)
  "Do nothing and return nil.  All ARGUMENTS are ignored."
  (interactive))

(defun view-X11-colors ()
  "View file `/usr/lib/X11/rgb.txt', which lists available X11 colors."
  (interactive) (view-file-other-window "/usr/lib/X11/rgb.txt")) ; In `view.el'.

(defun forward-overlay (&optional arg)
  "Move forward ARG overlays.
Move cursor to next position where an overlay starts or ends.
If there are no more overlay boundaries, move to (point-max)."
  (interactive "p")
  (setq arg (or arg 1))
  (setq arg (1- arg))
  (while (natnump arg) (goto-char (next-overlay-change (point))) (decf arg)))


;;;###autoload
(defun forward-char-same-line (&optional arg)
  "Move forward a max of ARG chars on the same line, or backward if ARG < 0.
Returns the signed number of chars moved if /= ARG, else returns nil."
  (interactive "p")
  (let* ((start (point))
         (fwd-p (natnump arg))
         (max (save-excursion
                (if fwd-p (end-of-line) (beginning-of-line))
                (- (point) start))))
    (setq arg (prefix-numeric-value arg))
    (forward-char (if fwd-p (min max arg) (max max arg)))
    (and (< (abs max) (abs arg)) max)))

(defun end-of-line+ (&optional n)
  "Move cursor to end of current line or end of next line if repeated.
This is similar to `end-of-line', but:
  If called interactively with no prefix arg:
     If the previous command was also `end-of-line+', then move to the
     end of the next line.  Else, move to the end of the current line.
  Otherwise, move to the end of the Nth next line (Nth previous line
     if N<0).  Command `end-of-line', by contrast, moves to the end of
     the (N-1)th next line."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)))
  (unless n (setq n 0))                 ; non-interactive with no arg
  (if (and (eq this-command last-command) (not current-prefix-arg))
      (forward-line 1)
    (forward-line n))
  (end-of-line))

(defun beginning-of-line+ (&optional n)
  "Move cursor to beginning of current line or next line if repeated.
This is the similar to `beginning-of-line', but:
1. With arg N, the direction is the opposite: this command moves
   backward, not forward, N lines.
2. If called interactively with no prefix arg:
      If the previous command was also `beginning-of-line+', then move
      to the beginning of the previous line.  Else, move to the
      beginning of the current line.
   Otherwise, move to the beginning of the Nth previous line (Nth next
      line if N<0).  Command `beginning-of-line', by contrast, moves to
      the beginning of the (N-1)th next line."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)))
  (unless n (setq n 0))                 ; non-interactive with no arg
  (if (and (eq this-command last-command) (not current-prefix-arg))
      (forward-line -1)
    (forward-line (- n))))

(defun beginning-or-indentation (&optional n)
  "Move cursor to beginning of this line or to its indentation.
If at indentation position of this line, move to beginning of line.
If at beginning of line, move to beginning of previous line.
Else, move to indentation position of this line.

With arg N, move backward to the beginning of the Nth previous line.
Interactively, N is the prefix arg."
  (interactive "P")
  (cond ((or (bolp) n)
         (forward-line (- (prefix-numeric-value n))))
        ((save-excursion (skip-chars-backward " \t") (bolp)) ; At indentation.
         (forward-line 0))
        (t (back-to-indentation))))

(defalias 'selection-length 'region-length)
(defalias 'count-chars-in-region 'region-length)
(defun region-length ()
  (interactive)
  (let ((len (abs (- (mark) (point)))))
    (message "Region contains %s characters" len)
    len))

(unless (fboundp 'line-number-at-pos)   ; Exists in Emacs 22.
  (defun line-number-at-pos (&optional pos)
    "Buffer line number at position POS. Current line number if POS is nil.
Counting starts at (point-min), so any narrowing restriction applies."
    (1+ (count-lines (point-min) (save-excursion (when pos (goto-char pos))
                                                 (forward-line 0) (point))))))

(defun goto-longest-line (pt mrk)
  "Go to the first of the longest lines in the region or after point.
If the region is active, then it is checked for longest lines;
otherwise, the rest of the buffer following point is checked.

Interactively, displays a message with the information that is
returned, a list of three elements:

\(LINE LINE-LENGTH OTHER-LINES LINES-CHECKED)

LINES-CHECKED is the number of lines measured.
LINE is the first of the longest lines measured.
LINE-LENGTH is the length of LINE.
OTHER-LINES is a list of other lines checked that are as long as LINE."
  (interactive "r")
  (when (and mark-active (> (point) (mark))) (exchange-point-and-mark))
  (let* ((start-line (line-number-at-pos))
         (max-width 0)
         (line start-line)
         long-lines col)
    (while (and (not (eobp)) (or (not mark-active) (< (point) (mark))))
      (end-of-line)
      (setq col (current-column))
      (when (>= col max-width)
        (if (= col max-width)
            (setq long-lines (cons line long-lines))
          (setq long-lines (list line)))
        (setq max-width col))
      (forward-line 1)
      (setq line (1+ line)))
    (setq long-lines (nreverse long-lines))
    (let ((lines long-lines))
      (while (and lines (> start-line (car lines))) (pop lines))
      (goto-line (or (car lines) start-line)))
    (when (interactive-p)
      (let ((others (cdr long-lines)))
        (message
         "Line %d: %d chars%s (%d lines measured)"
         (car long-lines) max-width
         (concat (and others
                      (format ", Others: {%s}" 
                              (mapconcat #'identity (cdr long-lines) ", "))))
         (- line start-line))))
    (list (car long-lines) max-width (cdr long-lines) (- line start-line))))

(defun goto-long-line (len)
  "Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN."
  (interactive "P")
  (setq len (if (consp len) fill-column (prefix-numeric-value len)))
  (let ((start-line (line-number-at-pos))
        (len-found 0)
        (found nil))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (setq found
            (< len (setq len-found
                         (- (save-excursion (end-of-line) (point)) (point))))))
    (cond (found
           (when (interactive-p)
             (message "Line %d: %d chars" (line-number-at-pos) len-found)))
          (t
           (goto-line start-line)
           (message "Not found")))))

(defun delete-lines (num-lines)
  "Delete NUM-LINES lines, starting at point.
Lines are deleted, not killed.
With positive prefix arg, deletion is forward.
With negative prefix arg, deletion is backward."
  (interactive "p")
  (when (not (zerop num-lines))
    (let ((column (current-column))
          (forward-p (natnump num-lines)))
      (if forward-p (beginning-of-line) (end-of-line))
      (let ((beg (point)))
        (forward-line (if forward-p (1- num-lines) (1+ num-lines)))
        (if forward-p (end-of-line) (beginning-of-line))
        (delete-region beg (point)))
      (when (eq (following-char) ?\n) (delete-char 1))
      (move-to-column column))))

;;;;;;###autoload
;;;(defvar default-pr-switches "-fl68"
;;;  "*String of default switches to pass to `pr'.
;;;These may be overridden in `pr-declp-buffer' and `pr-declp-region'.")
;;;(put 'default-pr-switches 'variable-interactive
;;;     "sDefault switches to pass to `pr' (e.g. \"-fl68\"): ")

;;;;;;###autoload
;;;(defvar declp-switches nil
;;;  "*List of strings to pass as extra switch args to `declp-command'.")

;;;;;;###autoload
;;;(defvar declp-command "declp" "*Shell command for printing a file.
;;;Should usually be either \"declp\" or \"declpt\".")
;;;(put 'declp-command 'variable-interactive
;;;     "sShell command for printing a file. (\"declp\" or \"declpt\"): ")

;;;(defmacro declp-sheet-options (number-up)
;;;  (` (if (and (integerp (, number-up)) (not (zerop (, number-up))))
;;;         (if (natnump (, number-up))
;;;             (format " -K 2 -N %d " (, number-up))
;;;           (format " -N %d " (, number-up)))
;;;       "")))

;;;;;;###autoload
;;;(defun declp-buffer-w-switches ()
;;;  "Print buffer using `declp-command' and switches that you specify.
;;;Variable `declp-switches' is a list of proposed default switches."
;;;  (interactive)
;;;  (let ((cmd (read-from-minibuffer
;;;              (concat "Print buffer `" (buffer-name) "' with command:   ")
;;;              (apply 'concat declp-command " " declp-switches) nil nil
;;;              'minibuffer-history)))
;;;    (save-restriction (widen) (message "Spooling...")
;;;                      (shell-command-on-region (point-min) (point-max) cmd)))
;;;  (message "Spooling... done"))

;;;(defun declp-buffer (&optional number-up)
;;;  "Print buffer contents using `declp-command'.
;;;NUM-UP pages are printed on a side of paper, bordered by a rectangle
;;;if NUM-UP is a non-zero integer.  NUM-UP is the prefix arg, if any.
;;;Otherwise you are prompted for NUM-UP.
;;;   NUM-UP > 0 => Print on both sides of paper.
;;;   NUM-UP < 0 => Only print on one side of paper.
;;;   Otherwise  => Print 1 page per sheet, on one side of paper, and
;;;                 do not print a rectangular border around each page.
;;;Global variable `declp-switches' is a list of switches (strings)
;;;for `declp-command'."
;;;  (interactive (list (if current-prefix-arg
;;;                         (prefix-numeric-value current-prefix-arg)
;;;                       (read-number-up 'declp-buffer))))
;;;  (declp-region-1 (point-min) (point-max)
;;;                  (cons (declp-sheet-options number-up) declp-switches)))

;;;;;;###autoload
;;;(defun declp-region-w-switches (start end)
;;;  "Print region using `declp-command' and switches that you specify.
;;;Variable `declp-switches' is a list of proposed default switches."
;;;  (interactive "r")
;;;  (let ((cmd (concat (read-from-minibuffer
;;;                      (concat "Print region with command:   ")
;;;                      (apply 'concat declp-command " " declp-switches) nil nil
;;;                      'minibuffer-history))))
;;;    (message "Spooling...")
;;;    (shell-command-on-region start end cmd))
;;;  (message "Spooling... done"))

;;;(defun declp-region (start end &optional number-up)
;;;  "Print region contents using `declp-command'.
;;;NUM-UP pages are printed on a side of paper, bordered by a rectangle
;;;if NUM-UP is a non-zero integer.  NUM-UP is the prefix arg, if any.
;;;Otherwise you are prompted for NUM-UP.
;;;   NUM-UP > 0 => Print on both sides of paper.
;;;   NUM-UP < 0 => Only print on one side of paper.
;;;   Otherwise  => Print 1 page per sheet, on one side of paper, and
;;;                 do not print a rectangular border around each page.
;;;Global variable `declp-switches' is a list of switches (strings)
;;;for `declp-command'."
;;;  (interactive (list (region-beginning) (region-end)
;;;                     (if current-prefix-arg
;;;                         (prefix-numeric-value current-prefix-arg)
;;;                       (read-number-up 'declp-region))))
;;;  (declp-region-1 start end
;;;                  (cons (declp-sheet-options number-up) declp-switches)))

;;;;;;###autoload
;;;(defun pr-declp-buffer (&optional number-up pr-switches)
;;;  "Print buffer with page headings using `declp-command'.
;;;The Unix `pr' command is used to provide the page headings.
;;;You are prompted for PR-SWITCHES, which is a string of switches
;;;to the `pr' command.  For information on `pr', type `\\[manual-entry] pr'.
;;;\(Note: The `-m' option to `pr' makes no sense in this context.)

;;;NUM-UP pages are printed on a side of paper, bordered by a rectangle
;;;if NUM-UP is a non-zero integer.  NUM-UP is the prefix arg, if any.
;;;Otherwise you are prompted for NUM-UP.
;;;   NUM-UP > 0 => Print on both sides of paper.
;;;   NUM-UP < 0 => Only print on one side of paper.
;;;   Otherwise  => Print 1 page per sheet, on one side of paper, and
;;;                 do not print a rectangular border around each page.

;;;Global variables:
;;;`declp-switches' is a list of switches (strings) for `declp-command'.
;;;`default-pr-switches' is a string of default switches for `pr'.
;;;Switches in PR-SWITCHES override those in `default-pr-switches'."
;;;  (interactive
;;;   (let (pr-opt
;;;         (pr-opts ()))
;;;     (list (if current-prefix-arg
;;;               (prefix-numeric-value current-prefix-arg)
;;;             (read-number-up 'pr-declp-region))
;;;           (progn
;;;             (setq pr-opts (list (read-from-minibuffer "Page title: "
;;;                                                       (cons (buffer-name) 1))
;;;                                 "-h")) ; Order reversed below to '-h title'.
;;;             (while (not (string= "" pr-opt))
;;;               (push (setq pr-opt (read-from-minibuffer
;;;                                   "Switches for `pr' (RET to end): "))
;;;                     pr-opts))
;;;             (pop pr-opts)              ; ""
;;;             (nreverse pr-opts)))))
;;;  (declp-region-1 (point-min) (point-max)
;;;                  (cons (declp-sheet-options number-up) declp-switches)
;;;                  (or pr-switches ""))) ; Non-nil for pr.

;;;;;;###autoload
;;;(defun pr-declp-region (start end &optional &optional number-up pr-switches)
;;;  "Print region with page headings using `declp-command'.
;;;The Unix `pr' command is used to provide the page headings.
;;;You are prompted for PR-SWITCHES, which is a string of switches
;;;to the `pr' command.  For information on `pr', type `\\[manual-entry] pr'.
;;;\(Note: The `-m' option to `pr' makes no sense in this context.)

;;;NUM-UP pages are printed on a side of paper, bordered by a rectangle
;;;if NUM-UP is a non-zero integer.  NUM-UP is the prefix arg, if any.
;;;Otherwise you are prompted for NUM-UP.
;;;   NUM-UP > 0 => Print on both sides of paper.
;;;   NUM-UP < 0 => Only print on one side of paper.
;;;   Otherwise  => Print 1 page per sheet, on one side of paper, and
;;;                 do not print a rectangular border around each page.

;;;Global variables:
;;;`declp-switches' is a list of switches (strings) for `declp-command'.
;;;`default-pr-switches' is a string of default switches for `pr'.
;;;Switches in PR-SWITCHES override those in `default-pr-switches'."
;;;  (interactive
;;;   (let (pr-opt
;;;         (pr-opts ()))
;;;     (list (region-beginning) (region-end)
;;;           (if current-prefix-arg
;;;               (prefix-numeric-value current-prefix-arg)
;;;             (read-number-up 'pr-declp-region))
;;;           (progn
;;;             (setq pr-opts (list (read-from-minibuffer "Page title: ") "-h"))
;;;             (while (not (string= "" pr-opt))
;;;               (push (setq pr-opt (read-from-minibuffer
;;;                                   "Switches for `pr' (RET to end): "))
;;;                     pr-opts))
;;;             (pop pr-opts)              ; ""
;;;             (nreverse pr-opts)))))
;;;  (declp-region-1 start end
;;;                  (cons (declp-sheet-options number-up) declp-switches)
;;;                  (or pr-switches ""))) ; Non-nil for pr.

;;;;; Adapted from `print-region-1' in `lpr.el'.
;;;(defun declp-region-1 (start end switches &optional page-headers)
;;;  ;; On some MIPS system, having a space in the job name
;;;  ;; crashes the printer demon.  But using dashes looks ugly
;;;  ;; and it seems too annoying to do for those MIPS systems.
;;;  (let ((name (concat (buffer-name) " Emacs buffer"))
;;;     (title (concat (buffer-name) " Emacs buffer"))
;;;     (width tab-width))
;;;    (save-excursion
;;;      (when (/= tab-width 8)
;;;        (print-region-new-buffer start end)
;;;        (setq tab-width width)
;;;        (save-excursion (goto-char end) (setq end (point-marker)))
;;;        (untabify (point-min) (point-max)))
;;;      ;; Filter region through `pr'.
;;;      (message "Filtering with `pr'...")
;;;      (when page-headers
;;;        (print-region-new-buffer start end)
;;;        (when (not (zerop (apply 'call-process-region start end "pr" t t nil
;;;                                 default-pr-switches page-headers)))
;;;          (display-buffer " *spool temp*")
;;;          (error "Error in switches to `pr'"))
;;;        (setq start (point-min))
;;;        (setq end (point-max)))
;;;      (message "Spooling...")
;;;      (apply 'shell-command-on-region
;;;             (list start end (apply 'concat declp-command " " switches)))
;;;      (when (markerp end) (set-marker end nil))
;;;      (message "Spooling... done"))))

;;;(defun read-number-up (fn)
;;;  "Read NUMBER-UP argument for a declp print function,
;;;`declp-buffer', `declp-region', `pr-declp-buffer', or `pr-declp-region'."
;;;  (let ((prompt "Number of pages per sheet of paper (`?' for help): ")
;;;        input)
;;;    (while (not (and (condition-case nil (setq input (read-minibuffer prompt))
;;;                       (error nil))     ; Read a non-Lisp expression.
;;;                     (numberp input)))  ; Read a Lisp sexp, but not a number.
;;;      (save-window-excursion (describe-function fn))) ; Defined in `help.el'.
;;;    (round input)))                     ; Convert floating point to integer.

(defun yank-secondary ()
  "Insert the secondary selection at point.
Moves point to the end of the inserted text.  Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))

(defun goto-previous-mark ()
  "Jump to previous mark, rotating the (local) `mark-ring'.
Does not affect the `global-mark-ring'.
This is equivalent to `set-mark-command' with a non-nil argument."
  (interactive) (set-mark-command t))

;;;###autoload
(defun goto-previous-global-mark (&optional pop-p)
  "Jump to previous global mark, rotating the `global-mark-ring'.
Non-nil prefix arg POP-P means just do a `pop-global-mark'."
  (interactive "P")
  ;; `pop-global-mark', then put popped mark at end of `global-mark-ring'.
  ;; 1. `pop-global-mark':
  ;;    (It's inlined here to keep access to MARKER for #2, below.)
  (while (and global-mark-ring (not (marker-buffer (car global-mark-ring))))
    (pop global-mark-ring)) ;; Pop entries which refer to non-existent buffers.
  (unless global-mark-ring (error "No global mark set"))
  (let* ((marker (car global-mark-ring))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (when (and (eq (point-marker) marker) (atom (cdr global-mark-ring)))
      (error "No other global marks"))
    (pop global-mark-ring)
    (set-buffer buffer)
    (unless (and (>= position (point-min)) (<= position (point-max))) (widen))
    (goto-char position)
    (switch-to-buffer buffer)
    ;; 2. Put popped mark at end of `global-mark-ring'.
    (unless pop-p
      (setq global-mark-ring (nconc global-mark-ring (list marker))))))

;;;###autoload
(defun region-to-buffer (start end buffer arg)
  "Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents."
  (interactive
   (let ((arg (and current-prefix-arg
                   (prefix-numeric-value current-prefix-arg))))
     (list (region-beginning) (region-end)
           (read-buffer (concat (if arg
                                    (if (natnump arg) "Append" "Prepend")
                                  "Write")
                                " region to buffer: ")
                        (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                            (another-buffer nil t)
                          (other-buffer (current-buffer))))
           arg)))
  (setq buffer (get-buffer-create buffer)) ; Convert to buffer.
  (when (eq buffer (current-buffer))
    (error "Cannot copy region to its own buffer"))
  (cond ((natnump arg)
         (save-excursion (set-buffer buffer) (goto-char (point-max)))
         (append-to-buffer buffer start end))
        (arg
         (save-excursion (set-buffer buffer) (goto-char (point-min)))
         (prepend-to-buffer buffer start end))
        (t (copy-to-buffer buffer start end))))

;;;###autoload
(defun region-to-file (start end filename arg)
  "With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents."
  (interactive
   (list (region-beginning) (region-end)
         (read-file-name (concat (if current-prefix-arg "Append" "Write")
                                 " region to file: "))
         current-prefix-arg))
  (let* ((curr-file (buffer-file-name))
         (same-file-p (and curr-file (string= curr-file filename))))
    (cond ((or (not same-file-p)
               (progn
                 (when (fboundp 'flash-ding) (flash-ding))
                 (yes-or-no-p
                  (format
                   "Do you really want to REPLACE the contents of `%s' by \
just the REGION? "
                   (file-name-nondirectory curr-file)))))
           (write-region start end filename arg)
           (when same-file-p (revert-buffer t t)))
          (t (message "OK.  Not written.")))))

;(defalias 'xwud 'display-xwd-image-file)
;;;;###autoload
;(defun display-xwd-image-file (xwd-file &optional options dir)
;  "Display an xwd image file XWD-FILE using the Unix `xwud' command.
;Arg XWD-FILE is a string naming the file, or else a list of such
;strings (non-interactively).

;If XWD-FILE is a list, then each of the files named in it is displayed
;in turn, a mouse click on an image causing it to be replaced by the
;next one.  In this case, relative file names are taken as relative to
;the directory DIR (the optional third arg), which defaults to the
;current `default-directory'.

;A non-nil prefix arg => You are prompted for `xwud' options.
;For a list of possible options, type \"-help\" as an option.
;For more information, type `\\[manual-entry] xwud'.

;Output from the `xwud' processes is put into buffer \"*XWD Display*\",
;but that buffer is not displayed."
;  (interactive "F*.xwd file to display: \nP")
;  (when (and options (not (stringp options)))
;    (setq options (read-from-minibuffer "`xwud' options: " nil nil nil
;                                        'minibuffer-history)))
;  (setq dir (or dir default-directory))
;  (if (listp xwd-file)
;      (dolist (file xwd-file)
;        (funcall 'display-xwd-image-file (expand-file-name file dir) options))
;    (let ((buf (get-buffer-create "*XWD Display*")))
;      (save-excursion (set-buffer buf) (erase-buffer))
;      (start-process-shell-command "xwud" buf "xwud"
;                                   (concat options " -in " xwd-file)))))

;;;; TO TEST:
;;;;(display-xwd-image-file
;;;;   (directory-files "~/ICONS" nil "drew-poster.+\.xwd$" t) nil "~/ICONS")

;(defalias 'xwd 'capture-image-as-xwd-file)
;;;;###autoload
;(defun capture-image-as-xwd-file (xwd-file &optional options)
;  "Capture an X window image as an *.xwd file via Unix `xwd' command.
;The \"-nobdrs\" `xwd' option is provided by default.
;A non-nil prefix arg => You are prompted for `xwd' options.
;For a list of options, type \"-help\" as an option.
;For more information, type `\\[manual-entry] xwud'."
;  (interactive "F*.xwd image file to create: \nP")
;  (if options
;      (unless (stringp options)
;        (setq options (read-from-minibuffer "`xwd' options: " " -nobdrs "
;                                            nil nil 'minibuffer-history)))
;    (setq options " -nobdrs "))
;  (message
;   "Click in X window you want to capture as image file `%s'." xwd-file)
;  (shell-command (concat "xwd " options " -out " xwd-file)))

;;;###autoload
(defun read-shell-file-command (command)
  "Prompt for shell COMMAND, using current buffer's file as default arg.
If buffer is not associated with a file, you are prompted for a file.
COMMAND is a symbol."
  (let ((file (or (buffer-file-name) (read-file-name "File: "))))
    (setq file (and file (file-name-nondirectory file)))
    (setq command (format "%s  " command)) ; Convert to string.
    (read-from-minibuffer
     "" (cons (concat command (and file (concat " " file)))
              (length command)))))

(defun chmod (cmd)
  "Execute Unix command `chmod'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chmod')."
  (interactive (list (read-shell-file-command 'chmod)))
  (shell-command cmd))

(defun chgrp (cmd)
  "Execute Unix command `chgrp'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chgrp')."
  (interactive (list (read-shell-file-command 'chgrp)))
  (shell-command cmd))

(defun chown (cmd)
  "Execute Unix command `chown'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chown')."
  (interactive (list (read-shell-file-command 'chown)))
  (shell-command cmd))


;;  ***** NOTE: The following EMACS PRIMITIVE has been REDEFINED HERE:
;;
;;  `display-buffer' - Raises frame too.

;(or (fboundp 'old-display-buffer)
;(fset 'old-display-buffer (symbol-function 'display-buffer)))

;;; REPLACES ORIGINAL (C source code?): Raises frame too.
;;;;###autoload
;(defun display-buffer (buffer &optional not-this-window)
;  "Make BUFFER appear in some window but don't select it.
;BUFFER can be a buffer or a buffer name.  Returns the window.

;If BUFFER is shown already in some window, just use that one,
;unless it is the selected window and the optional second arg
;NOT-THIS-WINDOW is non-nil (interactively, with prefix arg).
;Raises the frame in which buffer is already shown.

;If `pop-up-frames' is non-nil, make a new frame if no window
;shows BUFFER."
;  (interactive (list (read-buffer "Display buffer: " (other-buffer) 'existing)
;                     current-prefix-arg))
;  (let ((win (get-buffer-window buffer t)))
;    (if (or not-this-window (not win))
;        (old-display-buffer buffer not-this-window)
;      (raise-frame (window-frame win))
;      win)))                            ; Return the window.


;; Candidate as replacement for `kill-buffer', at least when used interactively.
;; Should not just redefine `kill-buffer', because some programs count on a
;; specific other buffer taking the place of the killed buffer (in the window).
;;;###autoload
(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list
                (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))
  (cond ((buffer-live-p buffer)         ; Kill live buffer only.
         (let ((wins (get-buffer-window-list buffer nil t))) ; On all frames.
           (when (and (buffer-modified-p buffer)
                      (fboundp '1on1-flash-ding-minibuffer-frame))
             (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
           (when (kill-buffer buffer)   ; Only delete windows if buffer killed.
             (dolist (win wins)         ; (User might keep buffer if modified.)
               (when (window-live-p win) (delete-window win))))))
        ((interactive-p)
         (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

;;; Like `clone-indirect-buffer' of Emacs 21.
(defun indirect-buffer ()
  "Edit stuff in this buffer in an indirect-buffer window.
The indirect buffer can have a different major mode from current."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name "*indirect*")))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))))

(defalias 'clear-search-ring 'clear-search-history)

(defun clear-search-history (&optional regexp-too-p)
  "Clear the search history (empty it).
With prefix arg, clear also the regular-expression search history."
  (interactive "P")
  (setq search-ring nil)
  (when regexp-too-p (setq regexp-search-ring nil)))

(defalias 'clear-regexp-search-ring 'clear-regexp-search-history)

(defun clear-regexp-search-history (&optional simple-too-p)
  "Clear the regular-expression search history (empty it).
With prefix arg, clear also the simple search history."
  (interactive "P")
  (setq regexp-search-ring nil)
  (when simple-too-p (setq search-ring nil)))

(defun clear-search-histories ()
  "Clear both search histories: simple search and regexp search."
  (interactive)
  (setq regexp-search-ring nil)
  (setq search-ring nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc-cmds.el ends here
