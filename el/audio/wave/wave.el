;;; wave.el --- Editing Sound Files
;; Copyright (C) 2001, 2002, 2003 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; Wave is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Wave is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Wave; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'message)

(defface wave-split-face
  '((((class color))
     (:foreground "yellow" :background "black")))
  "Face used for split marks.")

(defvar wave-split-positions nil)
(defvar wave-file nil)
(defvar wave-summary nil)
(defvar wave-scale nil)			 

(defvar wave-mode-map nil)
(unless wave-mode-map
  (setq wave-mode-map (copy-keymap text-mode-map))
  (define-key wave-mode-map "s" 'wave-set-split-position)
  (define-key wave-mode-map "z" 'wave-zoom)
  (define-key wave-mode-map "q" 'wave-quit)
  (define-key wave-mode-map "Q" 'wave-quit-playing)
  (define-key wave-mode-map "p" 'wave-skip-backwards)
  (define-key wave-mode-map "n" 'wave-skip)
  (define-key wave-mode-map "S" 'wave-split-file)
  (define-key wave-mode-map "\r" 'wave-toggle-play))

(defun wave-mode (&optional arg)
  "Mode for editing WAVE files.

\\{wave-mode-map}"
  (interactive)
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (set-syntax-table table)
    (modify-syntax-entry ?' "w"))
  (setq major-mode 'wave-mode)
  (setq mode-name "Wave")
  (use-local-map wave-mode-map)
  (set (make-local-variable 'wave-summary) nil)
  (run-hooks 'wave-mode-hook))

;;;###autoload
(defun wave-file (file &optional start length)
  "Summarize FILE."
  (interactive "fFile name: ")
  (if (eq major-mode 'wave-mode)
      (switch-to-buffer (generate-new-buffer (concat file " Wave")))
    (pop-to-buffer (generate-new-buffer (concat file " Wave"))))
  (wave-mode)
  (setq wave-summary (wave-analyze-file file start length))
  (when (eq start nil)
    (setq wave-split-positions nil
	  wave-file file)
    (let ((max 0))
      (mapcar (lambda (elem)
		(setq max (max (cadr (assq 'value elem)) max)))
	      (cdr wave-summary))
      (setq wave-scale (/ (* 1.0 (1- (window-height))) max))))
  (wave-generate-summary))

(defun wave-generate-summary ()
  (let* ((general (car wave-summary))
	 (summary (cdr wave-summary))
	 (height (- (window-height) 1)))
    (erase-buffer)
    (dotimes (i height)
      (insert "\n"))
    (dolist (column summary)
      (wave-insert-column (* wave-scale (cadr (assq 'value column)))
			  (cadr (assq 'position column))
			  (cadr (assq 'frame-size general))))
    (if (not wave-split-positions)
	(goto-char (point-min))
      (wave-goto-last-split))
    (set-buffer-modified-p nil)))

(defun wave-goto-last-split ()
  (goto-char (point-min))
  (end-of-line)
  (while (and (not (bobp))
	      (not (eq (get-text-property (point) 'face)
		       'wave-split-face)))
    (forward-char -1)))
    

(defun wave-insert-column (height position frame-size)
  (goto-char (point-min))
  (end-of-line)
  (dotimes (i height)
    (insert "#")
    (forward-line 1)
    (end-of-line))
  (while (not (eobp))
    (insert " ")
    (forward-line 1)
    (end-of-line))
  (goto-char (point-min))
  (end-of-line)
  (put-text-property (1- (point)) (point) 'position position)
  (when (wave-split-position-p position frame-size)
    (put-text-property (1- (point)) (point) 'face 'wave-split-face)))

(defun wave-split-position-p (position frame-size)
  (let ((pos-p nil))
    (dolist (split wave-split-positions)
      (when (and (>= split position)
		 (< split (+ position frame-size)))
	(setq pos-p t)))
    pos-p))

(defun wave-analyze-file (file &optional start length)
  (with-temp-buffer
    (call-process "~/bin/summarize"
		  nil (current-buffer) nil
		  "-s" (if start (number-to-string (- start length)) "0")
		  "-l" (if length (number-to-string length) "-1")
		  file)
    (goto-char (point-min))
    (while (re-search-forward "[0-9])" nil t)
      (forward-char -1)
      (insert ".0"))
    (goto-char (point-min))
    (read (current-buffer))))

(defun wave-split-file ()
  "Split the file according to the specifications."
  (interactive)
  (let ((default-directory (file-name-directory wave-file)))
    (with-temp-buffer
      (call-process "~/bin/bsplit"
		    nil (current-buffer) nil
		    wave-file
		    (with-temp-buffer
		      (insert
		       (mapconcat 'number-to-string
				  (mapcar
				   (lambda (s)
				     (* s 2))
				   (sort
				    (copy-sequence wave-split-positions) '<))
				  ":"))
		      (goto-char (point-min))
		      (while (search-forward ".0" nil t)
			(replace-match ""))
		      (buffer-string)))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun wave-set-split-position ()
  "Mark the current column as a split position."
  (interactive)
  (push (get-text-property (point) 'position) wave-split-positions)
  (put-text-property (point) (1+ (point)) 'face 'wave-split-face))

(defun wave-zoom (&optional n)
  "Zoom to the next N frames."
  (interactive "p")
  (let ((position (get-text-property (point) 'position))
	(frame-size (cadr (assq 'frame-size (car wave-summary)))))
    (wave-file wave-file position (* frame-size n))))

(defun wave-quit ()
  "Pop back to the previous wave buffer."
  (interactive)
  (when (eq major-mode 'wave-mode)
    (kill-buffer (current-buffer))
    (when (eq major-mode 'wave-mode)
      (wave-generate-summary))))

(defvar wave-play-process nil)

(defun wave-quit-playing ()
  "Kill the player."
  (interactive)
  (ignore-errors
    (kill-process wave-play-process)))

(defun wave-skip ()
  "Toggle playing."
  (interactive)
  (forward-char 1)
  (wave-toggle-play))

(defun wave-skip-backwards ()
  "Toggle playing."
  (interactive)
  (forward-char -1)
  (wave-toggle-play))

(defun wave-toggle-play ()
  "Toggle playing."
  (interactive)
  (when (and wave-play-process
	     (memq (process-status wave-play-process) '(open run)))
    (kill-process wave-play-process))
  (let ((frame-size (cadr (assq 'frame-size (car wave-summary)))))
    (setq wave-play-process
	  (start-process "aplay" (get-buffer-create " *aplay*")
			 "/usr/local/src/alsa-utils-0.9.0beta6/aplay/aplay"
			 "-f" "S16_LE" "-r" "44100" "-c" "2"
			 "-t" "raw"
			 "--device" "testchan"
			 "-S" (number-to-string
			     (* (- (get-text-property (point) 'position) frame-size)
				2))
			 wave-file))))

(provide 'wave)

;;; wave.el ends here
