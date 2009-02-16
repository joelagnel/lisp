;;; wlist-mode.el -- List all W3M buffers
;;
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;; 
;; For quickly switching between numerous W3M buffers.
;; Start with M-x wl-show-list RET
;;
;;; Code

(defvar wl-mode-hook nil "Hooks called when entering w3m list mode")

(defvar wl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")   #'wl-quit)
    (define-key map (kbd "n")   #'wl-next)
    (define-key map (kbd "p")   #'wl-prev)
    (define-key map (kbd "SPC") #'scroll-up)
    (define-key map (kbd "DEL") #'scroll-down)
    (define-key map (kbd "<")   #'beginning-of-buffer)
    (define-key map (kbd ">")   #'end-of-buffer)
    (define-key map (kbd "s")   #'isearch-forward)
    (define-key map (kbd "r")   #'isearch-backward)
    (define-key map (kbd "h")   #'describe-mode)
    (define-key map (kbd "TAB") #'wl-next)
    (define-key map (kbd "RET") #'wl-switch-to)
    map)
  "Keymap used in W3M list mode.")

(defun wl-mode ()
  "Major mode for listing active W3M windows.
\\{wl-mode-map}"
  (interactive)
  (use-local-map wl-mode-map)
  (setq major-mode 'wl-mode
	mode-name "W3M-List*"
	buffer-read-only t)
  (set-buffer-modified-p nil)
  (run-hooks 'wl-mode-hook))

(defun wl-show-list ()
  (interactive)
  (pop-to-buffer "*W3M List*" t)

  (setq buffer-read-only nil)
  (erase-buffer)

  (insert "W3M active sessions list:\n\n")

  (save-excursion
    (let ((bufs (buffer-list))
	  (inscol nil))
      (while bufs
	;; XXX
	(when (string-match "\\*w3m\\*\\|\\*w3m\\*<[0-9]+>" (buffer-name (car bufs)))
	  (insert (format "%-8s - " (buffer-name (car bufs))))
	  (setq inscol (current-column))
	  (add-text-properties (save-excursion (beginning-of-line) (point)) (point) (list 'w3m-buffer (car bufs)))
	  ;; Insert Title
	  (insert (format "%-7s" "Title: "))
	  (insert-face (w3m-buffer-title (car bufs)) 'w3m-header-line-location-title-face)
	  (insert "\n")

	  ;; Insert Url
	  (move-to-column inscol t)
	  (insert (format "%-7s" "Url: "))
	  (insert-face (with-current-buffer (car bufs) w3m-current-url)'w3m-header-line-location-content-face)
	  (insert "\n\n"))
	    
	(setq bufs (cdr bufs)))))

  (wl-mode))

(defun wl-quit ()
  (interactive)
  (delete-window))

(defun wl-get-buffer ()
  "Get w3m-buffer property at point."
  (get-text-property (point) 'w3m-buffer))

(defun wl-next (&optional n)
  "Jump to next N w3m buffer"
  (interactive "p")

  (let* ((nd t)
	 (fbuf (wl-get-buffer))
	 (buf fbuf)
	 (pnt))

    (save-excursion
      (while (and nd (if (> n 0) (not (eobp)) (not (bobp))))
	(if (> n 0)
	    (forward-char 1)
	  (backward-char 1))
	(setq buf (wl-get-buffer))
	(when (and (not (null buf)) (not (eq buf fbuf)))
	  (progn
	    (setq pnt (point))
	    (setq nd nil)))))

    (when (null nd)
      (goto-char pnt)
      (beginning-of-line))))

(defun wl-prev (&optional n)
  (interactive "p")
  (wl-next (- 0 n)))

(defun wl-switch-to ()
  "Switch to w3m-buffer at point."
  (interactive)
  (let ((bufs (buffer-list))
	(buf (wl-get-buffer))
	(founded nil))
    (if buf
	(progn
	  (while bufs
	    (if (eq buf (car bufs))
		(progn
		  (setq founded t)
		  (setq bufs nil))
	      (setq bufs (cdr bufs))))

	  (if founded
	      (progn
		(delete-window)
		(switch-to-buffer buf))
	    (message "Buffer not found in buffer-list")))

      (progn
	(message "Not w3m-buffer property at point")
	(beep)))))

;;; wlist-mode.el ends here
