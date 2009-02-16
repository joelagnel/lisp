;;; ion3.el --- Tight integration of emacs with the ion3 window manager

;; Copyright (C) 2005-2006 by Stefan Reichör

;; Filename: ion3.el
;; Author: Stefan Reichör, <stefan@xsteve.at>

;; ion3.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; ion3.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; ion3.el is an emacs interface for the ion3 window manager

;; You need mod_ionflux-3 (at least from 2005-04-21)
;; mod_ionflux-3 can be found here: http://modeemi.fi/~tuomov/repos/

;; Put the following in your .emacs to make the ion3-mode function available:
;; (autoload 'ion3-mode "ion3" "Major mode to edit ion3 config files" t)

;; The latest version of ion3.el can be found at http://www.xsteve.at/prg/emacs/ion3.el

;; Comments / suggestions welcome!

;;; Todo
;;  * Better error handling - at the moment they are only shown on the
;;    terminal, where ion3 was started

;;; History:
;;

;;; Code:

;; --------------------------------------------------------------------------------
;; ion3 interaction via ionflux
;; --------------------------------------------------------------------------------

(defun ion3-run-ionflux (cmd)
  (shell-command-to-string (concat "ionflux -e '" cmd "'")))

(defun ion3-send-string (str)
  "Send STR to ion3, using the ionflux program."
  (ion3-run-ionflux str))

(defun ion3-send-region (start end)
  "Send send the region to ion3, using the ionflux program."
  (interactive "r")
  (ion3-run-ionflux (buffer-substring start end)))

(defun ion3-send-line ()
  "Send send the actual line to ion3, using the ionflux program."
  (interactive)
  (ion3-run-ionflux (buffer-substring (line-beginning-position) (line-end-position))))

(defun ion3-send-proc ()
  "Send proc around point to ion3."
  (interactive)
  (let (start end)
    (save-excursion
      (lua-beginning-of-proc)
      (setq start (point))
      (lua-end-of-proc)
      (setq end (point)))
    (ion3-send-region start end)))

(defun ion3-send-buffer ()
  "Send send the buffer content to ion3, using the ionflux program."
  (interactive)
  (ion3-send-region (point-min) (point-max)))


(defun ion3-cmd (cmd)
  "Send a command to ion3.
The command is prefixed by a return statement."
  (interactive "sIon3 cmd: ")
  (let ((result (ion3-run-ionflux (concat "return " cmd))))
    (when (interactive-p)
      (message result))
    result))


;; --------------------------------------------------------------------------------
;; Utility functions that need ion3-emacs.lua
;; --------------------------------------------------------------------------------

(defun ion3-client-list ()
  "Return the list of the ion3 clients."
  (let* ((s (ion3-cmd "emacs.list_clients()"))
         (s0 (substring s 1 (- (length s) 2)))
         (client-list (split-string s0 "\\\\\n")))
    client-list))


;; (ido-completing-read "ion3 window: " (ion3-client-list) t t nil nil (car (ion3-client-list)))

(defun ion3-goto-client (name)
  ;;(interactive (list (ido-completing-read "select: " '("a" "aaab" "a/b" "a/b/c" "x/z"))))
  (interactive (list (ido-completing-read "select: " (ion3-client-list))))
  (ion3-send-string (concat "WRegion.goto(ioncore.lookup_clientwin(\"" name "\"))")))


;; --------------------------------------------------------------------------------
;; The ion3 edit mode, based on lua mode
;; --------------------------------------------------------------------------------

(defvar ion3-mode-map () "Keymap used in `ion3-mode' buffers.")

(when (not ion3-mode-map)
  (setq ion3-mode-map (make-sparse-keymap))
  (define-key ion3-mode-map [(control ?c) (control ?p)] 'ion3-send-proc)
  (define-key ion3-mode-map [(control ?c) (control ?r)] 'ion3-send-region)
  (define-key ion3-mode-map [(control ?c) (control ?b)] 'ion3-send-buffer)
  (define-key ion3-mode-map [(control ?c) (control ?l)] 'ion3-send-line)
  )

(easy-menu-define ion3-mode-menu ion3-mode-map
"'ion3-mode' menu"
                  '("Ion3"
                    ("Interaction"
                    ["Send Procedure" ion3-send-proc t]
                    ["Send Region" ion3-send-region t]
                    ["Send Buffer" ion3-send-buffer t]
                    ["Send String" ion3-send-string t]
                    ["Send Line" ion3-send-line t]
                    )
                    ["Goto client" ion3-goto-client t]
                    ))

(define-derived-mode ion3-mode lua-mode "ion3"
  "ion3-mode provides a tight integration of emacs and ion3.
"
  (use-local-map ion3-mode-map))



;; --------------------------------------------------------------------------------
;; various stuff for testing purposes
;; --------------------------------------------------------------------------------


;; (ion3-send-string "ioncore.goto_next_screen()")
;; (ion3-cmd "ioncore.goto_next_screen()")

;;(defun ion3-show-message-for-cmd (cmd)
;;  (interactive "sion3 command: ")
;;  (ion3-run-ionflux (concat "mod_query.message(ioncore.find_screen_id(0)," cmd ")")))


;; (ion3-client-list)


;; (ion3-show-message-for-cmd "ioncore.version()")
;; (ion3-send-string "return ioncore.version()")
;; (ion3-send-string "return 4+5")

;; (ion3-cmd "ioncore.version()")
;; (ion3-cmd "4+5")

 ;; (setenv "IONFLUX_SOCKET" "/tmp/fileM5J57y")

;; things to support
;;table ioncore.clientwin_list()

;; WClientWin ioncore.lookup_clientwin(string name)

;; bool WRegion.goto(WRegion reg)

(provide 'ion3)

;;; ion3.el ends here

;; arch-tag: 17c5fcf9-ea23-4ca5-b7d5-a0635b8b4230


