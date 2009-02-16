;;; An (X)Emacs command to convert PNG image to Face header

;; For your .emacs file:
;;(autoload 'insert-face-header "make-face-header" nil t)

(defun insert-face-header (file)
  "Insert a Face header at point."
  (interactive "*fPNG File: ")
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	default-enable-multibyte-characters
	face)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (or (looking-at "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a")
	  (error "%s does't look like a PNG file"
		 (file-name-nondirectory file)))
      (base64-encode-region (point-min) (point-max) t)
      (setq face (buffer-string)))
    (beginning-of-line)
    (insert "Face: " face "\n")
    (forward-line -1)
    (while (and (= 77 (move-to-column 77))
		(not (eolp)))
      (insert "\n "))
    (forward-line 1)))
