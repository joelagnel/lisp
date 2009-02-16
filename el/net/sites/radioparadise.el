;;; radioparadise.el --- NOT OFFICIAL. See lathi's homepage below.
;; Time-stamp: <2003-05-23 22:19:06 deego>
;; Copyright (C) 2003 Doug Alcorn
;; Emacs Lisp Archive entry
;; Filename: radioparadise.el
;; Package: radioparadise
;; Author: Doug Alcorn
;; Keywords:
;; Version:
;; Author's homepage: 
;; http://www.lathi.net/twiki-bin/view/Main/EmacsAndRadioparadise
;; For latest version:
;; http://www.lathi.net/twiki-bin/view/Main/EmacsAndRadioparadise

(defconst radioparadise-home-page
  "http://gnufans.net/~deego")


;; I decided to save lathi's script in the form of a library.  See Lathi's
;; page above for the original/actual/latest versions.

;; also made some changes to make it work with my gnu emacs.


;; these utilities are gotten from lathi's page and emacswiki and
;; modified to my own needs.  Please see their pages for original
;; versions.

;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:



(defgroup radioparadise nil
  "The group radioparadise."
  :group 'applications)

;;; xemacs/emacs foobar:



(defun radioparadise-replace-in-string (str from to)
  (if 
      (featurep 'xemacs)
      (replace-in-string str from to)
    (save-excursion
      (with-temp-buffer
	(insert str)
	(goto-char (point-min))
	(replace-string from to)
	(buffer-substring-no-properties (point-min) (point-max))))))


(defun radioparadise-exec-to-string (str)
  (if (featurep 'xemacs)
      (exec-to-string str)
    (shell-command-to-string str)))

;;; Real Code:

(defun radioparadise-tweak-answer (string)
  (let ((foo (split-string string "\n"))
	)
    (while (and foo (equal (car foo) ""))
      (pop foo))
    (if (not foo) ""
      (with-temp-buffer 
	(insert (car foo))
	(goto-char (point-min))
	(if (looking-at " *1\\.")
	    (replace-match ""))
	(goto-char (point-min))
	(when (search-forward "(" nil t)
	  (backward-delete-char 1)
	  (delete-region (point) (point-max)))
	(concat (buffer-substring-no-properties
		 (point-min) (min (point-max) 60)) "")))))

(defun radioparadise-now-playing ()
  "Scrape from the http://www.radioparadise.com/nowplay_frame.php page the current song"
  (radioparadise-replace-in-string  
   (radioparadise-exec-to-string 
    "lynx -dump http://www.radioparadise.com/nowplay_frame.php")
   ; matching a string like "Now Playing:   [2]Artist - Title"
   ; lynx -dump adds the [X] url indexes automatically
   ; also note the trickery needed to match "anything including a newline"
   "\\(.\\|\n\\)*Playing:.*\]\\(.*\\)\\(.\\|\n\\)*"
   "\\2"))

;;;###autoload
(defun radioparadise-insert-now-playing ()
  "Insert at point the song now playing on radioparadise.com"
  (interactive)
  (insert (concat 
	   (radioparadise-now-playing) " <http://www.radioparadise.com>")))

;;;###autoload
(defun radioparadise-message-now-playing ()
  "Display the current song from radioparadise.com in the minibuffer"
  (interactive)
  (message (radioparadise-now-playing)))

(defun radioparadise-erc-cmd-RP (&optional ignore)
  "Say the current song playing on http://www.radioparadise.com into the current ERC channel"
  (let ((song (radioparadise-now-playing)))
    (if song
	(erc-send-message 
	 (concat "http://www.radioparadise.com is now playing: " song))
      (erc-send-message 
       "I can't tell what song is playing on http://www.radioparadise.com"))))


(defun radioparadise-erc-send-message (msg)
  (goto-char (point-max))
  (insert msg))



;;;###autoload
(defun radioparadise-xmms-announce ()
  (interactive)
  (goto-char (point-max))
  (insert (radioparadise-erc-prepare-xmms-msg)))

;;;###autoload
(defun radioparadise-xmms  ()
  (interactive)
  (message (radioparadise-erc-prepare-xmms-msg)))


;; this adapted from emacswiki
(defun radioparadise-erc-cmd-XMMS (&optional ignore)
  "Say the current xmms mp3 song title to the current ERC channel"
  (erc-send-message 
   (radioparadise-erc-prepare-xmms-msg t)))

(defun radioparadise-erc-prepare-xmms-msg (&rest ignore)
  (concat 
   (if (string-match "#emacs" (buffer-name))
       "NP: " "listening to ")
   (if (= (string-to-int 
	   (shell-command-to-string "xmmd-shell -e list; echo $?"))
	  0)
       ;;(shell-command-to-string "xmmsctrl title")
       (radioparadise-tweak-answer
	(shell-command-to-string "xmms-shell -e list"))
     "nothing")))

;;;###autoload
(defun radioparadise-install-erc ()
  (interactive)
  (defalias 'erc-cmd-RP 'radioparadise-erc-cmd-RP)
  (defalias 'erc-cmd-XMMS 'radioparadise-erc-cmd-XMMS)
  )




(provide 'radioparadise)




;;; radioparadise.el ends here
