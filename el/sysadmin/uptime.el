;;; uptime.el by David N. Welton <davidw@dedasys.com>

;;; uptime defines 2 user visible functions, `uptime' and
;;; `linux-uptime'. `uptime' works on all systems, but must be
;;; initiliazed with `uptime-init' in your .emacs file.
;;; `linux-uptime' is, as the name suggests, specific to GNU/Linux
;;; systems because of its dependence on the /proc filesystem.

(require 'cl)

(defvar uptime-stat-file-starttime-position 21
  "Position in the file /proc/(emacs-pid)/stat where the `starttime'
value may be found.  Currently defaults to 21 on GNU/Linux systems")

(defun uptime-init ()
  "Initialize emacs uptime"
    (setq uptime-time-init (current-time)))

(defun uptime ()
  "Emacs uptime:-)"
  (interactive)
  (if (eq system-type 'gnu/linux)
      (linux-uptime)
    (unless (boundp 'uptime-time-init)
      (setq uptime-time-init (current-time))
      (message "better `uptime-init' in your .emacs!"))
    (non-linux-uptime)))

(defun non-linux-uptime ()
  (let* ((tm (current-time))
	 (diff (list (- (car tm) (car uptime-time-init))
		     (- (cadr tm) (cadr uptime-time-init))))
	 (seconds (+ (* (float (car diff)) 65536) (float (cadr diff))))
	 (days  (floor (/ seconds 86400)))
         (hours (progn (decf seconds (* days  86400)) (floor (/ seconds 3600))))
         (mins  (progn (decf seconds (* hours 3600))  (floor (/ seconds 60)))))
    (message (format "up %d days,  %02d:%02d" days hours mins))))

(defun linux-uptime ()
  "Emacs uptime for Linux (relies on /proc)"
  (interactive)
  (if (eq system-type 'gnu/linux)
      (message "GNU/Linux system")
    (error "`linux-uptime' only works on gnu/linux systems with the
proc filesystem installed!"))
  (let* ((seconds (getprocstat))
	 (days  (floor (/ seconds 86400)))
         (hours (progn (decf seconds (* days  86400)) (floor (/ seconds 3600))))
         (mins  (progn (decf seconds (* hours 3600))  (floor (/ seconds 60)))))
    (message (format "up %d days,  %02d:%02d" days hours mins))))

(defun getprocstat ()
  "do the dirty work of processing the proc files"
  (let* ((foo (call-process "cat" nil "statout" nil  (format "/proc/%d/stat" (emacs-pid))))
	 (foo (switch-to-buffer (get-buffer "statout")))
	 (statstring (buffer-string))
	 (starttime (aref (vconcat (split-string statstring)) uptime-stat-file-starttime-position))
	 (foo (kill-buffer (current-buffer)))
	 (foo (call-process "cat" nil "uptimeout" nil "/proc/uptime"))
	 (foo (switch-to-buffer (get-buffer "uptimeout")))
	 (uptimestring (buffer-string))
	 (systemuptimestring (car (split-string uptimestring)))
	 (foo (kill-buffer (current-buffer))))
    (round (- (string-to-number systemuptimestring)
       (/ (string-to-number starttime) 100)))))

