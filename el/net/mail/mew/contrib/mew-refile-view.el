;;; mew-refile-view.el --- View refile alist

;; Author:  Takashi P.KATOH <p-katoh@shiratori.riec.tohoku.ac.jp>
;; Created: Oct 22, 1998
;; Revised: Jan 06, 2004

;;; Code:

(defconst mew-refile-view-version "mew-refile-view.el version 0.07")

(require 'mew)
(if mew-xemacs-p (require 'easymenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User customize variables
;;;

;; -> mew-vars.el ?
(defvar mew-refile-view-exec-confirm t
  "*Non nil means `mew-refile-view-exec' prompts the user for
confirmation before refiling.")

(defvar mew-refile-view-show-trash nil
  "*Non nil means trash folder (i.e. delete-marked messages)
will be also shown.")

(defvar mew-refile-view-mode-hook nil)
(defvar mew-refile-view-mode-map nil)

(defvar mew-refile-view-mode-menu-spec
  '("Mew/RefileView"
    ["Next page"    scroll-up t]
    ["Prev page"    scroll-down t]
    ["Top"          beginning-of-buffer t]
    ["Bottom"       end-of-buffer t]
    ["Prev message" mew-refile-view-prev-msg t]
    ["Next message" mew-refile-view-next-msg t]
    "----"
    ["Show again"   mew-refile-view-again t]
    ["Goto summary" mew-refile-view-goto-summary t]
    ["Unmark (single refile folder)" mew-refile-view-unmark-one t]
    ["Unmark"	    mew-refile-view-unmark t]
    ["Refile"	    mew-refile-view-refile t]
    ["Delete"	    mew-refile-view-delete t]
    ["Quit"         mew-refile-view-quit t]
    ))

(if mew-refile-view-mode-map
    ()
  (setq mew-refile-view-mode-map (make-sparse-keymap))
  (define-key mew-refile-view-mode-map " "    'scroll-up)
  (define-key mew-refile-view-mode-map "\177" 'scroll-down)
  (define-key mew-refile-view-mode-map "."    'mew-refile-view-goto-summary)
  (define-key mew-refile-view-mode-map "h"    'mew-refile-view-goto-summary)
  (define-key mew-refile-view-mode-map "n"    'mew-refile-view-next-msg)
  (define-key mew-refile-view-mode-map "p"    'mew-refile-view-prev-msg)
  (define-key mew-refile-view-mode-map "N"    'mew-refile-view-next-fld)
  (define-key mew-refile-view-mode-map "P"    'mew-refile-view-prev-fld)
  (define-key mew-refile-view-mode-map "l"    'mew-refile-view-again)
  (define-key mew-refile-view-mode-map "u"    'mew-refile-view-unmark-one)
  (define-key mew-refile-view-mode-map "U"    'mew-refile-view-unmark)
  (define-key mew-refile-view-mode-map "o"    'mew-refile-view-refile)
  (define-key mew-refile-view-mode-map "d"    'mew-refile-view-delete)
  (define-key mew-refile-view-mode-map "x"    'mew-refile-view-exec)
  (define-key mew-refile-view-mode-map "q"    'mew-refile-view-quit)
  (define-key mew-refile-view-mode-map "Q"    'mew-refile-view-exit)
  (define-key mew-refile-view-mode-map "<"    'beginning-of-buffer)
  (define-key mew-refile-view-mode-map ">"    'end-of-buffer)
  (if mew-temacs-p
      (easy-menu-define
       mew-refile-view-mode-menu
       mew-refile-view-mode-map
       "Menu used in Refile view mode."
       mew-refile-view-mode-menu-spec)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refile view mode
;;;

(defun mew-re-search-forward-and-backward (regexp bound noerror)
  (or (re-search-forward regexp bound noerror)
      (re-search-backward regexp bound noerror)))
  
;; -> mew-vars ?
(defconst mew-refile-view-folder-regex "^[*+=]")
(defvar mew-original-folder nil)

(defun mew-assoc-add (key alist mem)
  (append (list (append (or (assoc key alist) (list key)) (list mem)))
	  (delete (assoc key alist) alist)))

(defun mew-car-string< (a1 a2)
  (let ((k1 (car a1)) (k2 (car a2)))
    (string< k1 k2)))

(defun mew-refile-view-make-alist (msg)
  ;; (mew-sinfo-get-refile) -> '(("+foo" "1" "2") ("+bar" "4" "3"))
  (let ((alist
	 (mapcar '(lambda (msg) (assoc msg (mew-sinfo-get-refile))) msg))
	result)
    (while alist
      (let ((flist (cdr (car alist))))
	(while flist
	  (setq result (mew-assoc-add (car flist) result (car (car alist)))
		flist (cdr flist))))
      (setq alist (cdr alist)))
    result))

(defun mew-refile-view (&optional prefix)
  (interactive "P")
  (mew-pickable
   (if (interactive-p)
       (mew-current-set-window-config))
   (let* ((folder (buffer-name))
	  (bufname (format "*Mew refile view* (%s)" folder))
	  (mew-refile-view-show-trash (or prefix mew-refile-view-show-trash))
	  (refile (mew-summary-mark-collect mew-mark-refile
					    (point-min) (point-max)))
	  (trash
	   (if mew-refile-view-show-trash
	       (mew-summary-mark-collect mew-mark-delete
					 (point-min) (point-max))
	     nil))
	  (unlink
	   (if mew-refile-view-show-trash
	       (mew-summary-mark-collect mew-mark-unlink
					 (point-min) (point-max))
	       nil))
	  (ofld (substring folder 1))
	  tmp)
     (if (and (mew-thread-p) (get-buffer ofld))
	 (progn
	   (save-excursion
	     (set-buffer ofld)
	     (setq tmp (mew-sinfo-get-refile)))
	   (mew-sinfo-set-refile tmp)))
     (if (not (or refile trash unlink))
	 (progn
	   (message "No refile marks")
	   (if (buffer-live-p (get-buffer bufname))
	       (progn
		 (set-buffer bufname)
		 (setq buffer-read-only nil)
		 (erase-buffer)
		 (insert "No refile marks\n")
		 (setq buffer-read-only t))))
       (let ((alist (mew-refile-view-make-alist refile))
	     tmpalist view summary num numlist)
	 (setq view (pop-to-buffer bufname))
	 (setq buffer-read-only nil)
	 (erase-buffer)
	 (mew-buffers-setup bufname)

	 (setq tmpalist alist)
	 (setq alist nil)
	 (while tmpalist
	   (if (string= mew-trash-folder (car (car tmpalist)))
	       (progn
		 (setq trash (append (cdr (car tmpalist)) trash))
		 (setq alist (append (cdr tmpalist) alist))
		 (setq tmpalist nil))
	     (setq alist (cons (car tmpalist) alist))
	     (setq tmpalist (cdr tmpalist))))
	 ;;
	 (setq alist (sort alist 'mew-car-string<))
	 (if trash
	     (setq alist (append alist (list (cons mew-trash-folder trash)))))
	 (if unlink
	     (setq alist (append alist (list (cons "+REMOVE" unlink)))))
	 (while alist
	   (set-buffer view)
	   (insert (concat (car (car alist)) "\n"))
	   (setq numlist (sort (mapcar 'string-to-int (cdr (car alist))) '<))
	   (while numlist
	     (setq num (car numlist)
		   numlist (cdr numlist))
	     ;;
	     (set-buffer (get-buffer folder))
	     ;; Mew4
	     (when (mew-re-search-forward-and-backward
                    (mew-regex-sumsyn-msg (int-to-string num)) nil t)
	       (if (mew-thread-p)
		   (setq summary
			 (concat
			  (mew-buffer-substring (line-beginning-position)
						(progn (move-to-column (mew-vinfo-get-column))
						       (point)))
			  (mew-buffer-substring (let ((mew-use-thread-cursor t))
						  (progn (mew-thread-move-cursor) (point)))
						(line-end-position))))
		 (setq summary (mew-buffer-substring (line-beginning-position)
						     (line-end-position))))
	       ;;
	       (set-buffer view)
	       (insert summary)
	       (let ((mew-highlight-mark-folder-list (list bufname)))
		 (mew-mark-unmark))
	       (insert "\n")))
	   (insert "\n")
	   (setq alist (cdr alist)))
	 (goto-char (point-min))
	 (mew-refile-view-mode
	  (if (string-match mew-refile-view-folder-regex folder)
	      folder nil)))
       ))))

(defun mew-refile-view-goto-summary ()
  "Get back to Summary mode."
  (interactive)
  (let (num)
    (save-excursion
      (beginning-of-line)
      (setq num (if (looking-at (concat "^[^\r\n]+" mew-regex-sumsyn-short))
		    (mew-match-string 2))))
    (if (not (and mew-original-folder (get-buffer mew-original-folder)))
	(progn
	  (message "No Summary buffer for %s" mew-original-folder)
	  nil)
      (if (get-buffer-window mew-original-folder)
	  (select-window (get-buffer-window mew-original-folder))
	(mew-summary-switch-to-folder mew-original-folder))
      (if num (mew-summary-move-and-display num))
      t)))

(defun mew-refile-view-again ()
  (interactive)
  (if (not (and mew-original-folder (get-buffer mew-original-folder)))
      (message "No Summary buffer for %s" mew-original-folder)
    (set-buffer mew-original-folder)
    (mew-refile-view)))

(defun mew-refile-view-quit ()
  "Exit from mew-refile-view-mode."
  (interactive)
  (bury-buffer (current-buffer))
  (delete-windows-on (current-buffer))
  (mew-current-get-window-config))

(defun mew-refile-view-exit ()
  "Exit from mew-refile-view-mode."
  (interactive)
  (let ((buf (current-buffer)))
    (delete-windows-on (current-buffer))
    (kill-buffer buf)
    (mew-current-get-window-config)))

(defun mew-refile-view-next-msg ()
  "Move to next message in Mew refile view buffer."
  (interactive)
  (let ((orig (point)))
    (end-of-line)
    (if (re-search-forward mew-regex-sumsyn-short nil t)
	(beginning-of-line)
      (goto-char orig))))

(defun mew-refile-view-prev-msg ()
  "Move to previous message in Mew refile view buffer."
  (interactive)
  (let ((orig (point)))
    (beginning-of-line)
    (if (re-search-backward mew-regex-sumsyn-short nil t)
	(beginning-of-line)
      (goto-char orig))))

(defun mew-refile-view-next-fld ()
  (interactive)
  (let ((orig (point)))
    (end-of-line)
    (if (or (re-search-forward "^[+%=]" nil t)
	    (re-search-forward "^$" nil t))
	(beginning-of-line)
      (goto-char orig))))

(defun mew-refile-view-prev-fld ()
  (interactive)
  (let ((orig (point)))
    (beginning-of-line)
    (if (re-search-backward "^[+%=]" nil t)
	(beginning-of-line)
      (goto-char orig))))

(defun mew-refile-view-exec ()
  (interactive)
  (if (not (and mew-original-folder (get-buffer mew-original-folder)))
      (message "No Summary buffer for %s" mew-original-folder)
    (if (or (not mew-refile-view-exec-confirm)
	    ;; or yes-or-no-p?
	    (y-or-n-p "Execute refiling for these messages? "))
	(let ((fld mew-original-folder)
	      thread)
	  (save-excursion
	    (if (save-excursion
		  (set-buffer fld)
		  (and (mew-thread-p)
		       (get-buffer (substring fld 1))))
		(progn
		  (setq thread t)
		  (set-buffer (substring fld 1)))
	      (set-buffer fld))
	    (mew-summary-exec))
	  (mew-refile-view-exit)
	  (if thread
	      (progn
		(mew-kill-buffer fld)
		(mew-summary-switch-to-folder (substring fld 1))))))))
      

(defun mew-refile-view-unmark-one ()
  "Unmark this message.
If this message has multi-refile folders, remove one of them."
  (interactive)
  (mew-refile-view-msg 'undo 'one))

(defun mew-refile-view-unmark ()
  "Unmark this message."
  (interactive)
  (mew-refile-view-msg 'undo))

(defun mew-refile-view-refile ()
  "Refile this message."
  (interactive)
  (mew-refile-view-msg 'refile))

(defun mew-refile-view-delete ()
  "Delete this message."
  (interactive)
  (mew-refile-view-msg 'delete))

(defun mew-refile-view-msg (op &optional one)
  (beginning-of-line)
  (let ((orig-point (point))
	(orig-buff (current-buffer))
	msg reffld bufref fld ofld tmp)
    (if (not (looking-at (concat "^[^\r\n]+" mew-regex-sumsyn-short)))
	(message "No message")
      (setq msg (mew-match-string 2))
      (if one
	  (save-excursion
	    (if (re-search-backward
		 (concat mew-refile-view-folder-regex ".+$")
		 nil t)
		(setq reffld (buffer-substring (match-beginning 0)
					       (match-end 0))))))
      ;; in mew summary buffer
      (if (mew-refile-view-goto-summary)
	  (mew-pickable
	   (cond
	    ((eq op 'refile)
	     (mew-summary-refile))
	    ((eq op 'undo)
	     (if (not (and one msg reffld))
		 (mew-summary-undo 1)
	       (setq bufref (assoc msg (mew-sinfo-get-refile)))
	       (if (< (length bufref) 3)
		   (mew-summary-undo 1)
		 ;; remove 1 folder
		 (setq bufref (delete reffld bufref))
		 (setq tmp (mew-sinfo-get-refile))
		 (mew-replace-with tmp bufref msg)
		 (mew-sinfo-set-refile tmp)
		 (if (mew-thread-p)
		     (progn
		       (setq fld (mew-summary-folder-name 'ext))
		       (setq ofld (substring fld 1))
		       (if (get-buffer ofld)
			   (save-excursion
			     (set-buffer ofld)
			     (mew-sinfo-set-refile tmp))))))))
	    ((eq op 'delete)
	     (mew-summary-delete 1)))
	   (mew-refile-view)))
      ;; we are out of mew summary buffer now
      (pop-to-buffer orig-buff)
      (if (< orig-point (point-max))
	  (goto-char orig-point)
	(goto-char (point-max)))
      (beginning-of-line))))

(defun mew-refile-view-mode (&optional folder)
  "Major mode for viewing refile alist.
The keys defined for this mode are:

SPC	Scroll up this message.
DEL	Back-scroll this message.
.	Get back to Summary mode.
h	Get back to Summary mode.
n	Move to next message.
p	Move to previous message.
N	Move to next folder.
P	Move to previous folder.
l	Reshow .
u	Unmark.
o	Refile again.
d	Put delete mark on this message.
x	Process marked messages.
q	Quit.
Q	Exit.
<	Go to top.
>	Go to bottom.
"
  (interactive)
  (if mew-xemacs-p
      (progn
        (set-buffer-menubar current-menubar)
        (add-submenu nil mew-refile-view-mode-menu-spec)))
  (setq major-mode 'mew-refile-view-mode)
  (setq mode-name "Refile-View")
  (setq mode-line-buffer-identification mew-mode-line-id)
  (use-local-map mew-refile-view-mode-map)
  (setq buffer-read-only t)
  (setq selective-display t)
  (setq selective-display-ellipses nil)
  (setq truncate-lines t)
  (make-local-variable 'mew-original-folder)
  (setq mew-original-folder folder)
  (mew-buffers-setup (buffer-name))
  (run-hooks 'mew-refile-view-mode-hook))

(provide 'mew-refile-view)

;;; Copyright Notice:

;; Copyright (C) 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-refile-view.el ends here
