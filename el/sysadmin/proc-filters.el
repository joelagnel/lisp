;;; proc-filters.el -- some generally useful process filters

;; Copyright (C) 1992, 93, 99, 00, 02, 2005 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions

;; $Id: proc-filters.el,v 1.25 2005/03/21 10:21:36 friedman Exp $

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

;; These are basically my templates for useful process filters.
;; They have been designed to work with inferior processes that may emit
;; output while the user is typing; they won't become mixed.

;;; Code:

(defvar proc-filter-shell-prompt-pattern-modes
  '(shell-mode rlogin-mode ssh-mode telnet-mode ftelnet-mode)
  "*List of major modes which are shell-mode or comint-mode based.
Used by `proc-filter-shell-erase-buffer' to determine which variables
contain valid interpreter prompt regexps.")

(defvar process-filter-output-functions
  '(proc-filter-shell-output-filter)
  "*Functions to run on the most recent output region.
This hook is called by `process-filter-using-insert-before-markers'
and `process-filter-using-insert'.")

(defvar proc-filter-shell-output-filter-mode t
  "*If nil, `proc-filter-shell-output-filter' does nothing.")

(defvar proc-filter-shell-output-filters
  '(proc-filter-carriage-motion
    proc-filter-column-motion
    proc-filter-color
    proc-filter-trailing-whitespace)
  "*Filters to run by `proc-filter-shell-output-filter'.")


;;; These are not user variables.

;; This needs to be buffer-local because its value is saved between calls
;; to proc-filter-carriage-motion, which may be called in multiple buffers.
(defvar proc-filter-carriage-motion-last-end-char ?0)
(make-variable-buffer-local 'proc-filter-carriage-motion-last-end-char)

;; We can save some consing by reusing the same markers whenever
;; proc-filter-carriage-motion is called.  These don't need to be
;; buffer-local because their values are not reused between calls.
(defvar proc-filter-carriage-motion-beg (make-marker))
(defvar proc-filter-carriage-motion-end (make-marker))

(defvar proc-filter-carriage-overlay nil)
(make-variable-buffer-local 'proc-filter-carriage-overlay)


;;;###autoload
(defun process-filter-using-insert-before-markers (proc string
                                                        &optional filters)
  (let (proc-mark region-begin window)
    (save-excursion
      (set-buffer (process-buffer proc))
      (setq proc-mark (process-mark proc))
      (setq region-begin (marker-position proc-mark))
      ;; If process mark is at window start, insert-before-markers will
      ;; insert text off-window since it's also inserting before the start
      ;; Window mark.  Make sure we can see the most recent text.
      (setq window (and (= proc-mark (window-start))
                        (get-buffer-window (current-buffer))))
      (goto-char proc-mark)
      (insert-before-markers string)
      (run-process-filter-output-functions region-begin proc-mark filters))
    ;; Frob window-start outside of save-excursion so it works whether the
    ;; current buffer is the process buffer or not.
    (and window
         (>= (window-start window) region-begin)
         (set-window-start window region-begin 'noforce))))

;;;###autoload
(defun process-filter-using-insert (proc string &optional filters)
  (let* ((original-buffer (current-buffer))
         (process-buffer (process-buffer proc))
         (window (get-buffer-window process-buffer))
         (proc-mark (process-mark proc))
         old-proc-mark-pos
         user-point
         user-point-offset)
    (unwind-protect
        (progn
          (set-buffer process-buffer)
          (setq user-point (point))
          (setq old-proc-mark-pos (marker-position proc-mark))
          (setq user-point-offset (- user-point old-proc-mark-pos))
          (goto-char proc-mark)
          (insert string)
          (set-marker proc-mark (point))
          (run-process-filter-output-functions old-proc-mark-pos
                                               proc-mark filters)
          (if (>= user-point-offset 0)
              (goto-char (+ (marker-position proc-mark) user-point-offset))
            (goto-char user-point))
          (and window
               (set-window-point window (point))))
      (set-buffer original-buffer))))

(defun run-process-filter-output-functions (&optional beg end functions)
  (save-restriction
    (narrow-to-region (or beg (region-beginning)) (or end (region-end)))
    (let ((fns (or functions process-filter-output-functions)))
      (while fns
        (goto-char (point-min))
        (funcall (car fns))
        (setq fns (cdr fns))))))


;;;###autoload
(defun process-re-output-filter (string &rest re)
  "Generic comint process output filter.
The argument STRING is only used if the current buffer is not a comint
process buffer; it is used compute the size of the region containging the
most recent process output.  Otherwise, only the most recent comint output
region is modified.
The remaining arguments RE are a regexps which matches text to be removed
from the region."
  (let* ((point-marker (point-marker))
         (end (process-mark (get-buffer-process (current-buffer))))
         (beg (process-filter-last-output-start string end)))
    (save-match-data
      (while re
        (goto-char beg)
        (while (re-search-forward (car re) end t)
          (delete-region (match-beginning 0) (match-end 0)))
        (setq re (cdr re))))
    (goto-char point-marker)))

;; Prefer comint-last-input-end to comint-last-output-start, since the
;; latter may be earlier in the buffer and we do not want to modify user
;; input regions.
(defun process-filter-last-output-start (&optional string end)
  (cond ((and (boundp 'comint-last-input-end)
              (boundp 'comint-last-output-start)
              comint-last-input-end
              comint-last-output-start)
         (max comint-last-input-end comint-last-output-start))
        ((stringp string)
         (- (or end (process-mark (get-buffer-process (current-buffer))))
            (length string)))
       (t (point-min))))


;;;###autoload
(defun proc-filter-shell-output-filter-mode (&optional prefix)
  "Toggle proc-filter-shell-output-filter-mode (see variable docstring).
If called with a positive prefix argument, always enable.
If called with a negative prefix argument, always disable.
If called with no prefix argument, toggle current state."
  (interactive "P")
  (setq proc-filter-shell-output-filter-mode
        (cond ((null prefix)
               (not proc-filter-shell-output-filter-mode))
              (t
               (>= (prefix-numeric-value prefix) 0))))
  (and (interactive-p)
       (message "proc-filter-shell-output-filter-mode is %s"
                (if proc-filter-shell-output-filter-mode
                    "enabled"
                  "disabled"))))

(defun proc-filter-shell-output-filter (&optional string)
  "Run all filters in `proc-filter-shell-output-filters'.
This is a reasonable thing to put on `comint-output-filter-functions'."
  (and proc-filter-shell-output-filter-mode
       (let* ((inhibit-field-text-motion t)
              (buffer-read-only nil)
              (filters proc-filter-shell-output-filters)
              (end (process-mark (get-buffer-process (current-buffer))))
              (beg (process-filter-last-output-start string end)))
         (save-excursion
           (save-restriction
             (save-match-data
               (narrow-to-region beg end)
               (while filters
                 (goto-char (point-min))
                 (funcall (car filters) string)
                 (setq filters (cdr filters)))))))))

(defun proc-filter-carriage-motion (&optional string)
  "Interpret carriage control characters in buffer.
Translate carriage return/linefeed sequences to linefeeds.
Make single carriage returns delete to the beginning of the line.
Make backspaces delete the previous character.

If a final naked carriage return appears in the region, it is not processed
right away; this is so that it can be interpreted correctly when the next
piece of output arrives.  Likewise, destructive backspaces are not
processed until replacement text is output."
  (cond ((string= "" string))
        ;; Avoid the work below if there are no special chars to process
        ((not (or (memq proc-filter-carriage-motion-last-end-char '(?\r ?\b))
                  (< (skip-chars-forward "^\b\r" (point-max))
                     (- (point-max) (point-min))))))
        (t
         (or proc-filter-carriage-overlay
             (let ((ovl (make-overlay 0 0)))
               (overlay-put ovl 'invisible 'proc-filter-carriage-motion)
               (overlay-put ovl 'evaporate t)
               (overlay-put ovl 'intangible t)
               (setq proc-filter-carriage-overlay ovl)))

         (save-restriction
           (let ((inhibit-field-text-motion t)
                 (inhibit-point-motion-hooks t)
                 (inhibit-read-only t)
                 (buffer-read-only nil)
                 (buffer-undo-list t) ; don't record changes here
                 (beg proc-filter-carriage-motion-beg)
                 (end proc-filter-carriage-motion-end))
             (set-marker beg (point-min))
             (set-marker end (point-max))

             (widen)

             ;; If last output chunk had an unprocessed trailing CR or
             ;; backspaces, include them in the current chunk.  Just use
             ;; point-min if buffer was erased.
             (cond ((char-equal ?\r proc-filter-carriage-motion-last-end-char)
                    (set-marker beg (max (point-min) (1- beg))))
                   ((char-equal ?\b proc-filter-carriage-motion-last-end-char)
                    ;; Cannot use (re-search-backward "\b+" nil t)
                    ;; because nearest \b at end will match; greediness only
                    ;; works in forward direction.
                    (while (and (> beg (point-min))
                                (char-equal ?\b (char-after (1- beg))))
                      (set-marker beg (1- beg)))))

             (setq proc-filter-carriage-motion-last-end-char
                   (char-after (1- end)))
             (cond ((char-equal ?\r proc-filter-carriage-motion-last-end-char)
                    ;; If last char in output chunk is a CR or backspaces,
                    ;; do not process them now.  This enables correct
                    ;; processing of CR LF below even if the CR and LF
                    ;; arrive in different output chunks.
                    (set-marker end (1- end))
                    (move-overlay proc-filter-carriage-overlay end (1+ end)))
                   ((char-equal ?\b proc-filter-carriage-motion-last-end-char)
                    ;; Or alternatively, if there is output followed by
                    ;; backspaces to erase the just-output text at the end
                    ;; of an output chunk, delay processing them until the
                    ;; next output sequence; otherwise the text is never
                    ;; visible in the buffer since backspace processing is
                    ;; destructive.
                    ;;
                    ;; Rant: if you are going to backspace over text for
                    ;; e.g. progress indicators, the backspaces should be
                    ;; emitted in the output chunk just prior to the
                    ;; replacement output, not at the end of the prior
                    ;; output (which is usually supposed to be visible for
                    ;; some period of time).  Many programs violate this
                    ;; principle on the assumption that backspace is not
                    ;; destructive.
                    (let ((last (marker-position end)))
                      (while (char-equal ?\b (char-after (1- end)))
                        (set-marker end (1- end)))
                      (move-overlay proc-filter-carriage-overlay end last))))

             ;; CR LF -> LF
             (goto-char beg)
             (while (re-search-forward "\r$" end t)
               (delete-char -1))
             ;; bare CR -> delete preceding line
             (goto-char beg)
             (while (search-forward "\r" end t)
               (beginning-of-line)
               (delete-region (point) (match-end 0)))
             ;; BS -> delete preceding character
             ;; don't attempt to delete beyond point-min
             (goto-char beg)
             (while (search-forward "\b" end t)
               (and (> (- (point) (point-min)) 1)
                    (delete-char -2))))))))

(defun proc-filter-column-motion (&optional string)
  "Process column positioning escape sequences."
  (while (re-search-forward "\e\\[\\([0-9]+\\)G" nil t)
    (let ((n (string-to-int (buffer-substring (match-beginning 1)
                                              (match-end 1))))
          distance)
      (delete-region (match-beginning 0) (match-end 0))
      ;; current-column doesn't work when narrowing is in effect such that
      ;; column 0 isn't included in the region.
      (setq distance (- n (save-restriction (widen) (current-column))))
      (and (> distance 0)
           (insert-before-markers (make-string distance ?\x20))))))

;; Some linux distributions configure user sessions by default to enable
;; color highlighting of all output from `ls'.
;; Thees don't work in emacs buffers by default; this function strips them
;; out unless ansi-color.el is enabled.
(defun proc-filter-color (&optional string)
  "Possibly strip ANSI terminal color escape sequences."
  (cond ((and (boundp 'ansi-color-for-comint-mode)
              ansi-color-for-comint-mode
              (memq 'ansi-color-process-output
                    comint-output-filter-functions)))
        (t
         (while (re-search-forward "\e\\[[0-9;]*m" nil t)
           (delete-region (match-beginning 0) (match-end 0))))))

;; To avoid output chunking problems, this function ignores the current
;; output line but does check the line just immediately previous to the
;; current output chunk, in case it was skipped over from processing the
;; previous output chunk.
;; Thus, trailing whitespace after prompts are never deleted.
(defun proc-filter-trailing-whitespace (&optional string)
  "Strip trailing whitespace from output lines."
  (save-restriction
    (let ((beg (point-min))
          (end (point-max)))
      (widen)
      (goto-char end)
      (skip-chars-backward "^\n")
      (setq end (point))

      (goto-char beg)
      (skip-chars-backward "^\n")
      (beginning-of-line)
      (narrow-to-region (point) end))

    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))


;;;###autoload
(defun reset-process-mark (&optional proc)
  "Set process-mark for process PROC to point-max.
This is useful if the process mark has been clobbered in some mysterious way."
  (interactive)
  (or proc (setq proc (get-buffer-process (current-buffer))))
  (set-marker (process-mark proc) (point-max)))

;;;###autoload
(defun proc-filter-shell-erase-buffer ()
  "Delete all buffer contents leading up to the process mark.
Leave a prompt visible."
  (interactive)
  (save-match-data
    (let ((orig-point (point-marker))
          (proc (get-buffer-process (current-buffer)))
          pattern)
      (cond
       ((and (boundp 'shell-prompt-pattern)
             (memq major-mode proc-filter-shell-prompt-pattern-modes))
        (setq pattern shell-prompt-pattern))
       ((boundp 'comint-prompt-regexp)
        (setq pattern comint-prompt-regexp))
       (t
        (signal 'void-variable (list 'comint-prompt-regexp
                                     'shell-prompt-pattern
                                     shell-prompt-pattern-modes))))
      (cond
       ((and proc (> (process-mark proc) orig-point))
        (goto-char (process-mark proc))
        (and (re-search-backward pattern nil t)
             (progn
               (delete-region (point-min) (point))
               (goto-char (process-mark proc)))))
       (t
        (and (re-search-backward pattern nil t)
             (delete-region (point-min) (point)))
        (goto-char orig-point))))))

(provide 'proc-filters)

;;; proc-filters.el ends here.
