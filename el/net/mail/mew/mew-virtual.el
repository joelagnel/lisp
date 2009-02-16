;;; mew-virtual.el --- Virtual mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual info
;;;

(defvar mew-vinfo-list
  '("func" "lra" "top" "db" "column" 
    "parent-folder" ;; Thread only
    "flds")) ;; Virtual (not Thread) only

(mew-blinfo-defun 'mew-vinfo mew-vinfo-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual mode
;;;

(defun mew-virtual-mode ()
  "Mew Virtual mode:: major mode to visualize messages in a virtual folder.
For more information, see the document of 'mew-summary-mode'."
  (interactive)
  (setq major-mode 'mew-virtual-mode)
  (setq mode-line-buffer-identification mew-mode-line-id)
  (use-local-map mew-summary-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;;
  (make-local-variable 'tab-width)
  (make-local-variable 'search-invisible)
  (setq search-invisible nil)
  (cond
   (mew-gemacs-p
    (unless (mew-thread-p)
      (jit-lock-register 'mew-summary-cook-region)))
   (t
    (make-local-hook 'window-scroll-functions)
    (add-hook 'window-scroll-functions 'mew-summary-cook-window nil 'local)))
  (mew-sinfo-set-disp-msg t)
  ;;
  (mew-summary-mode-name mew-mode-name-virtual)
  (mew-summary-setup-mode-line)
  (mew-summary-setup-decoration)
  (mew-highlight-cursor-line)
  (run-hooks 'mew-virtual-mode-hook))

(defun mew-summary-virtual (&optional ext)
  "Create Virtual mode with inputted pattern.
If called with '\\[universal-argument]',
execute 'mew-summary-virtual-with-external'. Otherwise, 
'mew-summary-virtual-with-internal' is called."
  (interactive "P")
  (if (not (mew-summary-or-virtual-p))
      (message "This command cannot be used in this mode")
    (if ext
	(if mew-summary-virtual-with-external-function
	    (mew-summary-virtual-with-external)
	  (message "mew-summary-virtual-with-external-function is nil"))
      (mew-summary-virtual-with-internal))))

(defun mew-summary-get-vfolder (flds)
  (let ((vfolder (mew-folder-to-virtual (mapconcat 'identity flds ","))))
    (when mew-ask-virtual-folder-name
      (setq vfolder (mew-input-string
		     "Virtual folder name %s(%s): " "" vfolder)))
    (unless (mew-folder-virtualp vfolder)
      (setq vfolder (mew-folder-to-virtual vfolder)))
    vfolder))

(defun mew-summary-virtual-with-internal ()
  "Create Virtual mode with mewl."
  (let* ((flds (mew-input-folders (mew-summary-physical-folder)))
	 folders vfolder pattern opts lra)
    (when flds
      (setq folders (mapcar 'mew-expand-folder2 flds))
      (setq pattern (mew-input-pick-pattern))
      (setq vfolder (mew-summary-get-vfolder flds))
      (mew-summary-switch-to-folder vfolder)
      (when (mew-summary-exclusive-p)
	(mew-vinfo-set-flds flds)
	(while folders
	  (setq opts (cons (car folders) (cons "-s" opts)))
	  (setq lra (cons (cons (car folders) (car flds)) lra))
	  (setq flds (cdr flds))
	  (setq folders (cdr folders)))
	(setq opts (cons "-a" (cons "-p" (cons pattern (nreverse opts)))))
	(mew-local-retrieve 'vir opts nil lra)))))

(defvar mew-summary-virtual-with-external-function
  'mew-summary-virtual-with-grep
  "*A function to be called by '\\[universal-argument] \\<mew-summary-mode-map>\\[mew-summary-virtual]'.
This function MUST returns a file name which contains message numbers.")

(defun mew-summary-virtual-with-external ()
  "Create Virtual mode with 'mew-summary-virtual-with-external-function'."
  (interactive)
  (let* ((flds (mew-input-folders (mew-summary-physical-folder)))
	 folders vfolder pattern file func opts lra)
    (when flds
      (setq folders (mapcar 'mew-expand-folder2 flds))
      (setq pattern (mew-input-grep-pattern))
      (mew-sinfo-set-find-key pattern) ;; xxx necessary?
      (setq vfolder (mew-summary-get-vfolder flds))
      (mew-summary-switch-to-folder vfolder)
      (when (mew-summary-exclusive-p)
	(with-temp-buffer
	  (mew-piolet
	   mew-cs-text-for-read mew-cs-text-for-write
	   (mew-alet
	    (setq file (funcall mew-summary-virtual-with-external-function
				folders pattern)))))
	(mew-vinfo-set-flds flds)
	(setq func `(lambda () (mew-delete-file ,file)))
	(setq opts (list "-i" file))
	(while folders
	  (setq lra (cons (cons (car folders) (car flds)) lra))
	  (setq flds (cdr flds))
	  (setq folders (cdr folders)))
	(mew-local-retrieve 'vir opts func lra)))))

(defun mew-summary-virtual-with-grep (flds pattern)
  (setq pattern (mew-cs-encode-arg pattern))
  (let ((file (mew-make-temp-name))
	picks fld dir msgs nxt)
    (while flds
      (setq dir (mew-expand-folder (car flds)))
      (let ((default-directory dir))
	(setq msgs (mew-dir-messages "."))
	(if (= (length msgs) 1) (setq msgs (cons null-device msgs)))
	(cd dir)
	(mew-erase-buffer)
	(message "Picking in %s..." (car flds))
	(while msgs
	  (goto-char (point-max))
	  (setq nxt (nthcdr mew-prog-grep-max-msgs msgs))
	  (if nxt (setcdr (nthcdr (1- mew-prog-grep-max-msgs) msgs) nil))
	  (apply 'call-process
		 mew-prog-vgrep nil t nil
		 (append mew-prog-vgrep-opts (list pattern) msgs))
	  (setq msgs nxt))
	(message "Picking in %s...done" (car flds)))
      (goto-char (point-min))
      (setq msgs nil)
      (while (not (eobp))
	(if (looking-at mew-regex-message-files2)
	    (setq msgs (cons (mew-match-string 0) msgs)))
	(forward-line))
      (setq msgs (mew-uniq-list msgs))
      (setq msgs (mapcar 'string-to-int msgs))
      (setq msgs (sort msgs '<))
      (setq msgs (mapcar 'int-to-string msgs))
      (if msgs (setq picks (cons (cons dir msgs) picks)))
      (setq flds (cdr flds)))
    (mew-erase-buffer)
    (setq picks (nreverse picks))
    (while picks
      (setq fld (mew-path-to-folder (car (car picks))))
      (insert "CD: " fld "\n")
      (setq msgs (cdr (car picks)))
      (while msgs
	(insert (car msgs) "\n")
	(setq msgs (cdr msgs)))
      (setq picks (cdr picks)))
    (mew-frwlet
     mew-cs-text-for-read mew-cs-text-for-write
     (write-region (point-min) (point-max) file nil 'no-msg))
    file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mark virtual
;;;

(defun mew-summary-mark-virtual (&optional ask-mark)
  "Making Virtual mode for messages marked with '*'.
If called with '\\[universal-argument]', you can specify a target mark."
  (interactive "P")
  (if (not (mew-summary-p))
      (message "This command cannot be used in this mode")
    (let* ((folder (mew-summary-folder-name 'ext))
	   (vfolder (mew-summary-get-vfolder (list folder)))
	   (mark mew-mark-review)
	   (start (point))
	   beg line med regex)
      (if ask-mark (setq mark (mew-input-mark)))
      (setq regex (mew-mark-regex mark))
      (mew-summary-switch-to-folder vfolder)
      (mew-erase-buffer)
      (mew-vinfo-set-flds (list folder))
      (set-buffer folder)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(beginning-of-line)
	(setq beg (point))
	(forward-line)
	;; This must be buffer-substring
	(setq line (buffer-substring beg (point)))
	(save-excursion
	  (set-buffer vfolder)
	  (mew-elet
	   (insert line)
	   (save-excursion
	     (when (and (search-backward "\r")
			(setq med (point))
			(looking-at mew-regex-sumsyn-short))
	       (goto-char (match-beginning 1))
	       (insert folder)
	       (put-text-property med (point) 'invisible t))))))
      (goto-char start)
      (set-buffer vfolder)
      (mew-summary-set-count-line)
      ;; Unmarking in both Summary and Thread
      (if (char-equal mark mew-mark-review)
	  (mew-mark-undo-mark mark 'no-msg 'virtual-only)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Old
;;;

(defun mew-summary-virtual-old ()
  "Obsoleted command."
  (interactive)
  (mew-message-for-summary "This command was obsoleted. Use '\\[mew-summary-virtual]' to make Virtual mode"))

(provide 'mew-virtual)

;;; Copyright Notice:

;; Copyright (C) 1996-2005 Mew developing team.
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

;;; mew-virtual.el ends here
