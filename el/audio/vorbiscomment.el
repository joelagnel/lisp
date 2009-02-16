;;; Saved through ges-version 0.3.3dev at 2003-09-21 13:35
;;; ;;; From: Jesper Harder <harder@myrealbox.com>
;;; ;;; Subject: vorbiscomment.el 1.1 --- Edit comments in Ogg Vorbis files
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Followup-To: gnu.emacs.help
;;; ;;; Date: Sun, 21 Sep 2003 02:38:29 +0200
;;; ;;; Organization: http://purl.org/harder/
;;; ;;; Mail-Copies-To: never

;;; [1. text/plain]

;;; A small utility to edit comments in Ogg Vorbis files (a patent-free
;;; alternative to MP3).

;;; This version fixes a problem with saving files with non-ASCII file
;;; names.


;;; [2. application/emacs-lisp; vorbiscomment.el]

;;; vorbiscomment.el --- Edit comments in Ogg Vorbis files

;; Copyright (C) 2003 Jesper Harder

;; Author: Jesper Harder <harder@ifa.au.dk>
;; Created: 19 Aug 2003
;; Version: 1.1
;; Location: <http://purl.org/harder/>
;; Keywords: multimedia, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Use `M-x vorbiscomment' to extract the comment headers from an Ogg
;; Vorbis file.  `C-c C-c' in the comment buffer writes the comments
;; back to the file.  You need the program "vorbiscomment" from
;; vorbis-tools.

;;; History:

;; Version 1.1: Fix saving non-ASCII file names.

;;; Code:

(defvar vorbiscomment-keywords
  (list (concat "^"
		(regexp-opt '("title" "version" "album" "tracknumber"
			      "artist" "performer" "copyright"
			      "license" "organization" "description"
			      "genre" "date" "location" "contact"
			      "isrc") t) "="))
  "Canonical Ogg Vorbis tag names.")

(define-derived-mode vorbiscomment-mode text-mode "vorbiscomment"
  (setq font-lock-defaults '(vorbiscomment-keywords t t nil nil))
  (make-variable-buffer-local 'vorbiscomment-file))

(define-key vorbiscomment-mode-map (kbd "C-c C-c")
  'vorbiscomment-save-and-exit)

(defun vorbiscomment (file)
  "Edit comments in Ogg Vorbis file FILE.\\<vorbiscomment-mode-map>
Use \\[vorbiscomment-save-and-exit] to save comments."
  (interactive "fOgg Vorbis file: ")
  (switch-to-buffer (generate-new-buffer file))
  (vorbiscomment-mode)
  (setq vorbiscomment-file (expand-file-name file))
  (let ((coding-system-for-read 'utf-8))
    (call-process "vorbiscomment" nil t t "--raw" "-l" vorbiscomment-file)))

(defun vorbiscomment-save-and-exit ()
  "Save comments and exit."
  (interactive)
  (let (status)
    (encode-coding-region (point-min) (point-max) 'utf-8)
    (setq status
	  (with-output-to-string
	    (call-process-region (point-min) (point-max)
				 "vorbiscomment" nil standard-output nil
				 "--raw" "-q" "-w"
				 vorbiscomment-file)))
    (if (string= status "")
	(message "Comments saved in %s" vorbiscomment-file)
      (error status)))
  (kill-buffer nil))

(provide 'vorbiscomment)

;;; vorbiscomment.el ends here

