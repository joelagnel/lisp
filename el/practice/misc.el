(defun list-overlays-at (&optional pos)
  "Describe overlays at POS or point."
  (interactive)
  (setq pos (or pos (point)))
  (let ((overlays (overlays-at pos))
	(obuf (current-buffer))
	(buf (get-buffer-create "*Overlays*"))
	(props '(priority window category face mouse-face display
		 help-echo modification-hooks insert-in-front-hooks
		 insert-behind-hooks invisible intangible
		 isearch-open-invisible isearch-open-invisible-temporary
		 before-string after-string evaporate local-map keymap))
	start end text)
    (if (not overlays)
	(message "None.")
      (set-buffer buf)
      (erase-buffer)
      (dolist (o overlays)
	(setq start (overlay-start o)
	      end (overlay-end o)
	      text (with-current-buffer obuf
		     (buffer-substring start end)))
	(when (> (- end start) 20)
	  (setq text (concat (substring text 1 10)
			     "..."
			     (substring text -7))))
	(insert (format "From %d to %d: \"%s\":\n" start end text))
	(dolist (p props)
	  (when (overlay-get o p)
	    (insert (format " %15S: %S\n" p (overlay-get o p))))))
      (pop-to-buffer buf))))


(defun insert-current-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(global-set-key (kbd "C-c i") 'insert-current-date)

(defun major-modes ()
  "Return all major modes in an alist."
  (mapcar 'list (mapcar 'symbol-name (apropos-internal "-mode$" 'commandp))))


(defun my-indirect-region (start end mode)
  "Edit the region between in an indirect buffer."
  (interactive (list (region-beginning)
		     (region-end)
		     (intern (completing-read "Major mode: " (major-modes)))))
  (clone-indirect-buffer-other-window (current-buffer))
  (narrow-to-region start end)
  (call-interactively mode))

(defun ssh (host)
  (interactive "sHost: ")
  (let ((explicit-shell-file-name "/usr/bin/ssh")
	(explicit-ssh-args (list host)))
    (shell "*ssh*")))