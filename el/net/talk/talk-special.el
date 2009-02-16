;; This file contains code of questionable quality.
;;
;; I accept no responsibility for damaged machines
;; accounts or egos.
;;
;; To the best of my knowlege, what is contianed here seems
;; to work for myself, and for my friends.
;;
;; All testing of this software so far was done with:
;;   berkely unix V  4.2A
;;   emacs        v  18.58.2
;;   gcc          v  2.1
;;
;; boring blurb dated: July 6th
;;--------------------------------------
;; This file is a part of talk.el
;;--------------------------------------
;;
;;  Eric ludlam
;;
;;--------------------------------------

(provide 'talk-special)

(defun talk-unique-remote-p ()
  "Returns process if in local and only one remote exists, of if default is
a remote buffer. [ie. if only 1 process recipient of typed key.]"

  (if (equal (length talk-remote-process-list) 1)
      (car talk-remote-process-list)
    (if (equal mode-name talk-remote-mode-string)
	(get-buffer-process (current-buffer))
      nil)))

(defun talk-other-emacs-p (process)
  "Returns t if remote is emacs, nil if not."
  (save-excursion
    (set-buffer (process-buffer process))
    talk-remote-is-emacs))

(defun talk-send-minibuffer-message (message)
  "Read message in the minibuffer, and send only if other is using
emacs."
  (interactive "sMessage: ")
  (setq pl (cons (talk-unique-remote-p) '()))
  (if (and (talk-other-emacs-p (car pl)) (car pl))
      (talk-send-output pl (format "\03%s\n" message))
    (error "Can't send message to that process.")))

(defun talk-hug-remote ()
  "Send a *HUG* as a minibuffer message to remote."
  (interactive)
  (talk-send-minibuffer-message "*HUG*"))
