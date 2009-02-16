;;; Saved through ges-version 0.3.3dev at 2003-08-12 09:48
;;; ;;; From: Marco Parrone <marc0@autistici.org>
;;; ;;; Subject: watch-logs and watch-logs-new-frame
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Sat, 09 Aug 2003 22:17:32 GMT
;;; ;;; Organization: individual

;;; [[PGP Signed Part:Undecided]]
;;; [1. text/plain]

;;; Two functions to watch the logs.

(defun watch-logs ()
  "Watch the logs."
  (interactive)
  (shell-command "tail -f  /var/log/postgres/current &" "*postgres*")
  (shell-command "tail -f /var/log/messages &" "*messages*")
;  (shell-command "tail -f /var/log/exim/mainlog &" "*exim/mainlog*")
  (delete-other-windows)
  (switch-to-buffer "*postgres*")
  (split-window-vertically)
  (switch-to-buffer "*messages*")
;  (split-window-vertically)
;  (switch-to-buffer "*exim/mainlog*")
 )

(defun watch-logs-new-frame ()
  "Make a new frame for watching logs, and switch to it."
  (interactive)
  (select-frame (make-frame-command))
  (watch-logs))

;;; -- 
;;; marc0@autistici.org (rot13-string "znep0@rpa.bet")
;;; 2143 9E77 D5E6 115A 48AD  A170 D0EE F736 4E88 99C2
;;; [[End of PGP Signed Part]]

