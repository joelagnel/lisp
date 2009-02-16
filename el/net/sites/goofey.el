;;; goofey --- send Goofey messages from Emacs !
;;
;; Goofey mode for Emacs.
;;
;; 1993-1999 Andrew J. Cosgriff !
;; ajc@bing.wattle.id.au
;;
;; Author: Andrew J Cosgriff <ajc@bing.wattle.id.au>
;; Created: sometime in 1993
;; Version: $Id: goofey.el,v 1.1 1999/01/29 11:28:57 ajc Exp $
;; URI: http://bing.bofh.asn.au/sw/emacs-lisp/
;; Keywords: emacs goofey messaging
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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary
;;
;; Being a big fan of goofey
;; (http://yoyo.cc.monash.edu.au/~tym/goofey.html) at Monash Uni, I
;; fgured it'd be kinda neat to be able to enter messages from within
;; Emacs to send off to people.  Why the hell not ?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 3.11 - Button 3 on someone's name in a "goofey who" buffer brings
;;        up a menu that lets you do stuff.
;;
;; 3.10 - You can now click button 2 on someone's name in a "goofey
;;        who" buffer to send a message to them.
;;
;; 3.8 - You can now complete usernames (from your watch and
;;       extrawatch) in the composition window.
;;
;; 3.7 - People in your watch and extrawatch are now highlighted in
;;       the "who" output.
;;
;; 3.2 - The output of who & finger are highlighted with font-lock.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
;;
(require 'message)
;;(require 'custom)
;; message requires custom anyway...

(defvar goofey-buffer-name "*Goofey Message*"
  "Name of goofey mode message entry buffer.")

(defvar goofey-buffer nil
  "Goofey Mode buffer")

(defvar goofey-targets nil
  "Targets of the next message to be sent.")

(defvar goofey-start-hook nil
  "Hook for stuff to be run when goofey compose starts.")

(defvar goofey-send-hook nil
  "Hook for preprocessing before message is sent.")

(defvar goofey-dont-wait nil
  "Only send the message to people who are goofed in.")

(defvar goofey-window-height 2
  "Height of the composition window, in lines.")

(defvar goofey-keymap-prefix "\C-x\M-g"
  "Key Prefix to use for the global keybindings for Goofey stuff")

(defvar goofey-keymap (make-sparse-keymap)
  "Goofey mode keymap")

(defvar goofey-watch nil
  "Cache of our goofey watch list")

(defvar goofey-extrawatch nil
  "Cache of our goofey extrawatch list")

(defface goofey-who-quiet-face
  '((((class color)
      (background dark))
     (:italic t))
    (((class color)
      (background light))
     (:italic t))
    (t
     (:bold t :italic t)))
  "Face used for displaying quiet people in a goofey \"who\" listing."
  :group 'goofey-faces)

(defface goofey-who-idle-face
  '((((class color)
      (background dark))
     (:italic t))
    (((class color)
      (background light))
     (:italic t))
    (t
     (:bold t :italic t)))
  "Face used for displaying idle people in a goofey \"who\" listing."
  :group 'goofey-faces)

(defface goofey-who-watched-face
  '((((class color)
      (background dark))
     (:foreground "cyan" :bold t))
    (((class color)
      (background light))
     (:foreground "red" :bold t))
    (t
     (:bold t :italic t)))
  "Face used for displaying watched people in a goofey \"who\" listing."
  :group 'goofey-faces)

(defface goofey-who-extrawatched-face
  '((((class color)
      (background dark))
     (:foreground "cyan" :bold nil))
    (((class color)
      (background light))
     (:foreground "red" :bold nil))
    (t
     (:bold t :italic t)))
  "Face used for displaying extrawatched people in a goofey \"who\" listing."
  :group 'goofey-faces)

(defface goofey-finger-header-face
  '((((class color)
      (background dark))
     (:foreground "cyan" :bold nil))
    (((class color)
      (background light))
     (:foreground "red" :bold nil))
    (t
     (:bold t :italic t)))
  "Face used for displaying headers in a goofey \"finger\" listing."
  :group 'goofey-faces)

(define-key global-map goofey-keymap-prefix goofey-keymap)
(define-key goofey-keymap "m" 'goofey)
(define-key goofey-keymap "w" 'goofey-do-who)
(define-key goofey-keymap "a" 'goofey-do-who-all)
(define-key goofey-keymap "f" 'goofey-do-finger)
(define-key goofey-keymap "x" 'goofey-do-command)

(defvar goofey-font-lock-keywords message-font-lock-keywords
  "Additional expressions to highlight in Goofey mode.")

;; XEmacs does it like this.  For Emacs, we have to set the
;; `font-lock-defaults' buffer-local variable.
(put 'goofey 'font-lock-defaults '(goofey-font-lock-keywords t))

(defun goofey ()
  "Send someone a message via goofey
Local Key bindings (while in the message buffer):
\\[goofey-do-send] : Send the message
\\[goofey-do-who] : Show who's on
\\[goofey-do-who-all] : Show everyone who's on
\\[goofey-do-finger] : Finger someone
\\[goofey-do-command] : Do a misc. goofey command
\\[goofey-toggle-dont-wait] : Toggle whether to send to people who are quiet or not present.

Global Key Bindings (prefixed with `goofey-keymap-prefix'):
\\{goofey-keymap}"
  (interactive)
  (goofey-compose-message))

(defun goofey-compose-message (&optional to)
  (setq goofey-buffer (get-buffer-create goofey-buffer-name))
  (pop-to-buffer goofey-buffer)
  (kill-all-local-variables)
  (setq mode-name "Goofey" major-mode 'goofey))
;  (shrink-window (- (window-displayed-height) goofey-window-height))

  (local-set-key "\C-c\C-c" 'goofey-do-send)
  (local-set-key "\C-c\C-w" 'goofey-do-who)
  (local-set-key "\C-c\C-a" 'goofey-do-who-all)
  (local-set-key "\C-c\C-f" 'goofey-do-finger)
  (local-set-key "\C-c\C-x" 'goofey-do-command)
  (local-set-key "\C-c\C-e" 'goofey-toggle-dont-wait)
  (local-set-key "\C-i" 'goofey-tab)

  (erase-buffer)
  (insert "To: \n--text follows this line--\n")
  (forward-line -2)
  (end-of-line)
  (if to (insert-string to))

  (font-lock-mode 1)

  (run-hooks 'goofey-start-hook))

(defun goofey-toggle-dont-wait ()
  "Toggle the value of goofey-dont-wait."
  (interactive)
  (if goofey-dont-wait
      (progn
	(setq goofey-dont-wait nil)
	(message "Goofey messages will be sent to people goofed in or not."))
    (progn
      (setq goofey-dont-wait t)
      (message "Goofey messages will only be sent to people goofed in."))))

(defun goofey-do-send ()
  "Sends off a goofey message"
  (interactive)
  (if (and (bufferp goofey-buffer))
      (progn (save-excursion
	       (set-buffer goofey-buffer)
	       (kill-buffer (get-buffer-create "*Completions*"))
	       (goto-char (point-min))
	       (if (re-search-forward "^To:[ ]*\\([A-Za-z0-9].*\\)\n--text follows this line--\n" nil t)
		   (progn (setq goofey-targets (match-string 1))
			  (delete-region (point-min) (match-end 0)))
		 (error "No recipients specified"))
	  
	       (run-hooks 'goofey-send-hook)
	       (delete-windows-on goofey-buffer)
	       (message "Sending message to %s..." goofey-targets)
	       (let ((goofey-out-buffer (get-buffer-create "*Shell Command Output*")))
		 (set-buffer goofey-buffer)
		 (shell-command-on-region
		  (point-min) (point-max)
		  (concat "goofey "
			  (if goofey-dont-wait "--forget-fail" "")
			  "-s " goofey-targets))
		 (set-buffer goofey-out-buffer)
		 (if (< 2 (count-lines (point-min) (point-max)))
		     (progn
		       (message "Sending message to %s...done !" goofey-targets)
		       (pop-to-buffer goofey-out-buffer)
		       (local-set-key "q" (lambda () (interactive) (goofey-quit (get-buffer-create "*Shell Command Output*"))))
		       (shrink-window (- (window-displayed-height) (count-lines (point-min) (point-max)))))))
	  
	       (kill-buffer goofey-buffer)))
    (progn
      (message "No message is waiting to be sent !")
      (ding))))

(defun goofey-get-watch (&optional reload)
  "Cache our goofey watch list"
  (when (or (null goofey-watch) reload)
    (setq goofey-watch-buffer (get-buffer-create "*Goofey Watch*"))
    (set-buffer goofey-watch-buffer)
    (shell-command "goofey -a+ watch" goofey-watch-buffer)
    (message "fetching goofey watch...done")
    (setq goofey-watch (concat "\\<\\*?\(?\\(" (replace-in-string (buffer-string (point-min) (- (point-max) 1)) " " "\\|" t) "\\)\)?\\>"))
    (kill-buffer goofey-watch-buffer))
  goofey-watch)

(defun goofey-get-extrawatch (&optional reload)
  "Cache our goofey watch list"
  (when (and (file-exists-p (expand-file-name "~/.goofey_extrawatch"))
	     (or (null goofey-extrawatch) reload))
    (setq goofey-watch-buffer (get-buffer-create "*Goofey Watch*"))
    (set-buffer goofey-watch-buffer)
    (insert-file-contents (expand-file-name "~/.goofey_extrawatch"))
    (setq goofey-extrawatch (concat "\\<\\*?\(?\\(" (replace-in-string (buffer-string (point-min) (- (point-max) 1)) " " "\\|" t) "\\)\)?\\>"))
    (setq goofey-extrawatch (replace-in-string goofey-extrawatch "\n" "\\|" t))
    (kill-buffer goofey-watch-buffer))
  goofey-extrawatch)

(defun goofey-do-who (&optional all)
  "Shows who is on goofey."
  (interactive)
  (let ((watch (goofey-get-watch)) (extrawatch (goofey-get-extrawatch)))
    (setq goofey-who-buffer (get-buffer-create "*Goofey Who*"))
    (set-buffer goofey-who-buffer)
    (local-set-key "q" (lambda () (interactive) (goofey-quit goofey-who-buffer)))
    (shell-command (concat "goofey -" (if all "w" "W")) goofey-who-buffer)
    (goofey-buttonize-buffer)
    (local-set-key 'button2 'goofey-compose-to-clicked-person)
    (local-set-key 'button3 'goofey-who-person-menu)

    (setq font-lock-keywords
	  '(("\(\\*?[a-zA-Z0-9_-]+\)" . goofey-who-idle-face)
	    ("\\*[a-zA-Z0-9_-]+" . goofey-who-quiet-face)))
    (if watch
	(setq font-lock-keywords
	      (append font-lock-keywords
		      (list (list watch 0 'goofey-who-watched-face t)))))
    (if extrawatch
	(setq font-lock-keywords
	      (append font-lock-keywords
		      (list (list extrawatch 0 'goofey-who-extrawatched-face t)))))
    (font-lock-mode 1)
    (goto-char (point-max))
    (pop-to-buffer goofey-who-buffer)
    (message "press q to close output window")
    (shrink-window (- (window-displayed-height) (count-lines
						 (point-min) (point-max))))))

(defun goofey-do-who-all ()
  "Shows who is on goofey, including idle people."
  (interactive)
  (goofey-do-who t))

(defun goofey-do-finger (person)
  "Fingers PERSON on goofey"
  (interactive "sEnter the person to finger : ")
  (with-output-to-temp-buffer "*Goofey Finger*"
    (setq goofey-finger-buffer (get-buffer-create "*Goofey Finger*"))
    (set-buffer goofey-finger-buffer)
    (local-set-key "q" (lambda () (interactive) (goofey-quit goofey-finger-buffer)))
    (shell-command (concat "goofey -w" person) t))
  (setq font-lock-keywords
	'(("^.*\\(last login\\|Preferred email\\): .*$" . goofey-finger-header-face)))
  (font-lock-mode 1)
  (pop-to-buffer goofey-finger-buffer)
  (message "press q to close output window")
  (shrink-window (- (window-displayed-height) (count-lines (point-min) (point-max)))))

(defun goofey-do-command (command)
  "Runs a goofey COMMAND."
  (interactive "sEnter the goofey command : ")
  (with-output-to-temp-buffer "*Goofey Command*"
    (setq goofey-command-buffer (get-buffer-create "*Goofey Command*"))
    (set-buffer goofey-command-buffer)
    (local-set-key "q" (lambda () (interactive) (goofey-quit goofey-command-buffer)))
    (shell-command (concat "goofey " command) goofey-command-buffer))
  (pop-to-buffer goofey-command-buffer)
  ;;(message "press q to close output window")
  (shrink-window (- (window-displayed-height) (count-lines (point-min) (point-max)))))

(defun goofey-quit (buffer)
  "Close the goofey output window"
  (interactive)
  (and (not (one-window-p)) (delete-window))
  (kill-buffer buffer))

;; completion stuff mostly nicked from Gnus' message.el

(defun goofey-tab ()
  "Expand person names in the To: header
Do a `tab-to-tab-stop' if not in that header."
  (interactive)
  (if (let ((mail-abbrev-mode-regexp "^To:"))
	(mail-abbrev-in-expansion-header-p))
      (goofey-expand-person)
    (tab-to-tab-stop)))

(defvar goofey-person-hashtb nil)

(defun make-goofey-person-hashtb (&optional reload)
  (let ((hashtb goofey-person-hashtb) (watch) (extrawatch) (p) (s))
    (when (or (null hashtb) reload)
      (save-excursion
	(setq hashtb nil)
	(setq goofey-watch-buffer (get-buffer-create "*Goofey Watch*"))
	(set-buffer goofey-watch-buffer)
	(erase-buffer)
	(shell-command "goofey -a+ watch" goofey-watch-buffer)
	(message "fetching goofey watch...done")
	(goto-char (point-min))
	(setq p (point-min))
	(while (not (=  (skip-chars-forward "0-9A-Za-z_\-") 0))
	  (setq s (replace-in-string (buffer-string p (point)) " " ""))
	  (setq hashtb
		(append hashtb
			(list (cons s s))))
	  (skip-chars-forward " \n")
	  (setq p (point)))
	(when (file-exists-p (expand-file-name "~/.goofey_extrawatch"))
	  (erase-buffer)
	  (insert-file-contents (expand-file-name "~/.goofey_extrawatch"))
	  (message "fetching goofey extra watch...done")
	  (goto-char (point-min))
	  (setq p (point-min))
	  (while (not (=  (skip-chars-forward "0-9A-Za-z_\-") 0))
	    (setq s (replace-in-string (buffer-string p (point)) "[ \n]" ""))
	    (setq hashtb
		  (append hashtb
			  (list (cons s s))))
	    (skip-chars-forward " \n")
	    (setq p (point)))))
      (setq goofey-person-hashtb hashtb))
    goofey-person-hashtb))

(defun goofey-expand-person ()
  "Expand the goofey recipient name under point."
  (let* ((b (save-excursion
	      (save-restriction
		(narrow-to-region
		 (save-excursion
		   (beginning-of-line)
		   (skip-chars-forward "^:")
		   (1+ (point)))
		 (point))
		(skip-chars-backward "^,\t\n ") (point))))
	 (completion-ignore-case t)
	 (string (buffer-substring b (progn (skip-chars-forward "^,\t\n ")
					    (point))))
	 (hashtb (make-goofey-person-hashtb))
	 (completions (all-completions string hashtb))
	 comp)
    (delete-region b (point))
    (cond
     ((= (length completions) 1)
      (if (string= (car completions) string)
	  (progn
	    (insert string)
	    (message "Only matching person"))
	(insert (car completions))))
     ((and (setq comp (try-completion string hashtb))
	   (not (string= comp string)))
      (insert comp))
     (t
      (insert string)
      (if (not comp)
	  (message "No matching people")
	(save-selected-window
	  (pop-to-buffer "*Completions*")
	  (buffer-disable-undo (current-buffer))
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (let ((standard-output (current-buffer)))
	      (display-completion-list (sort completions 'string<)))
	    (goto-char (point-min))
	    (delete-region (point) (progn (forward-line 3) (point))))))))))

;; "button"izing stuff nicked from info.el

(defun goofey-buttonize-region (start end)
  (setq extent (make-extent (point) end))
  (setq extent (make-extent start end))
  (set-extent-property extent 'goofey t)
  (set-extent-property extent 'highlight t))

(defun goofey-buttonize-buffer ()
  (let (p)
    (goto-char (point-min))
    (skip-chars-forward "*()% \n")
    (setq p (point))
    (while (not (=  (skip-chars-forward "0-9A-Za-z_\-") 0))
      (goofey-buttonize-region p (point))
      (skip-chars-forward "*()% \n")
      (setq p (point)))))

(defun goofey-compose-to-clicked-person (&optional event)
  "Compose a message to the person the user just clicked on."
  (interactive "@e")
  (or (and (event-point event)
	   (goofey-menu-send
	    (max (progn
		   (select-window (event-window event))
		   (event-point event))
		 (1+ (point-min)))))
      (error "click on a cross-reference to follow")))

(defun goofey-word-near-point (p)
  "Return the word at point"
  (let ((pos p)
	(value))
    (goto-char p)
    (skip-chars-backward "0-9A-Za-z_\-")
    (let ((start (point)))
      (forward-word 1)
      (setq value (buffer-substring-no-properties start (point))))
    (goto-char pos)
    value))

;; menu stuff nicked from info.el

(defun goofey-menu-send (&optional p)
  (interactive)
  (set-buffer goofey-who-buffer)
  (let ((name (goofey-word-near-point (or p (point)))))
    (goofey-quit goofey-who-buffer)
    (sit-for 0)
    (goofey-compose-message name))
  t)

(defun goofey-menu-finger (&optional p)
  (interactive)
  (set-buffer goofey-who-buffer)
  (let ((name (goofey-word-near-point (or p (point)))))
    (goofey-quit goofey-who-buffer)
    (sit-for 0)
    (goofey-do-finger name))
  t)

(defun goofey-menu-add-to-watch (&optional p)
  (interactive)
  (set-buffer goofey-who-buffer)
  (let ((name (goofey-word-near-point (or p (point)))))
    (goofey-quit goofey-who-buffer)
    (sit-for 0)
    (goofey-do-command (concat "-a+ watch " name))
    (setq goofey-watch nil
	  goofey-person-hashtb nil))
  t)

(defun goofey-menu-remove-from-watch (&optional p)
  (interactive)
  (set-buffer goofey-who-buffer)
  (let ((name (goofey-word-near-point (or p (point)))))
    (goofey-quit goofey-who-buffer)
    (sit-for 0)
    (goofey-do-command (concat "-a- watch " name))
    (setq goofey-watch nil
	  goofey-person-hashtb nil))
  t)

(defun goofey-construct-menu (&optional event name)
  "Construct a relevant menu of goofey commands."
  (or event (setq event (point)))
  (let ((menu) (name name))
    (setq menu
	  (list
	   (vector (concat "Send message to " name) 'goofey-menu-send)
	   (vector (concat "Finger " name) 'goofey-menu-finger)

	   (vector (concat "Add " name " to watch list") 'goofey-menu-add-to-watch)
	   (vector (concat "Remove " name " from watch list") 'goofey-menu-remove-from-watch)
	   ))
    menu))

(defun goofey-who-person-menu (event)
  "Pops up a menu of applicable goofey person commands."
  (interactive "e")
  (select-window (event-window event))
  (set-buffer (event-buffer event))
  (goto-char (event-point event))
  (let ((name (goofey-word-near-point
	       (max (event-point event)
		    (1+ (point-min))))))
    (let ((menu (goofey-construct-menu event name)))
      (setq menu (nconc (list "goofey"	; title: not displayed
			      "     goofey commands "
			      "--:shadowDoubleEtchedOut")
			menu))
      (let ((popup-menu-titles nil))
	(popup-menu menu)))))


;; goofey.el ends here
