;;; empd.el --- Emacs MPD (Music Player Daemon) frontend

;; Copyright (C) 2004-2005 Mikhail Gusarov <dottedmag@gorodok.net>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'libempd)

(defvar empd-browser-mode-map nil
"Keymap for EMPD mode")

(unless empd-browser-mode-map
  (setq empd-browser-mode-map (make-sparse-keymap))
  (define-key empd-browser-mode-map "\r" 'empd-add-path))

(defvar empd-browser-mode-hook nil)

(defun empd-browser-mode ()
  "Major mode for MPD browser. \\{empd-browser-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map empd-browser-mode-map)
  
  (setq mode-name "MPD browser mode")
  (setq major-mode 'empd-browser-mode)

  (run-hooks 'empd-browser-mode-hook))


(defun empd-browser ()
  (interactive)
  (let ((mpd-buffer (get-buffer-create "*MPD*")))
	(switch-to-buffer mpd-buffer)
	(empd-browser-mode)
	(erase-buffer)
	(empd->listall
	 `(lambda (string)
		(with-current-buffer ,mpd-buffer
		  (insert string "\n"))))))

(defun current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun empd-add-path ()
  (interactive)
  (let ((line (current-line)))
	(string-match "^[^:]*: \\(.*\\)$" line)
	(empd->add (match-string 1 line))))

(provide 'empd)

;;; empd.el ends here
