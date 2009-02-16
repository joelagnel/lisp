;;; mew-sort.el --- Sorting messages for Mew

;; Author:  Takashi P.KATOH <p-katoh@shiratori.riec.tohoku.ac.jp>
;;          Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  6, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort variety
;;;

(defvar mew-sort-switch
  '(("text" mew-sort-key-text mew-sort-string)
    ("ml"   mew-sort-key-ml   mew-sort-string)
    ("mlnum" mew-sort-key-mlnum mew-sort-string)
    ("date" mew-sort-key-date mew-sort-string)
    ("num"  mew-sort-key-num  mew-sort-number)
    ("postnum" mew-sort-key-postnum mew-sort-number)))

(defsubst mew-sort-key-text (key folder msg)
  (mew-subject-simplify key nil 'no-replace))

(defsubst mew-sort-key-ml (key folder msg)
  (let ((ret (mew-subject-simplify key nil 'no-replace)))
    (if (string-match "^[[(][^])]+[])][ \t]*" ret)
	(progn
	  (setq ret (substring ret (match-end 0)))
	  (mew-subject-simplify ret nil 'no-replace))
      ret)))

(defsubst mew-sort-key-mlnum (key folder msg)
  (let (mlname mlnum)
    (cond
     ((string-match "^\\([[(][^])]+\\)[: ]+\\([0-9]+\\)[])]" key)
      (setq mlname (match-string 1 key))
      (setq mlnum (match-string 2 key)))
     ((string-match "^[0-9]+$" key)
      (setq mlname "")
      (setq mlnum (match-string 0 key)))
     (t
      (setq mlname "")
      (setq mlnum "0")))
    (concat mlname (format "\000%010d" (string-to-int mlnum)))))

(defsubst mew-sort-key-date (key folder msg)
  (if (string= key "")
      (let ((time (mew-file-get-time (mew-expand-folder folder msg))))
	(mew-time-ctz-to-sortkey time))
    (mew-time-rfc-to-sortkey key)))

(defsubst mew-sort-key-num (key folder msg)
  (string-to-int key))

(defsubst mew-sort-key-postnum (key folder msg)
  (if (string-match "[0-9]+$" key)
      (string-to-int (match-string 0 key))
    (string-to-int key)))

(defsubst mew-sort-key (x) (cdr x))

(defsubst mew-sort-string (x y)
  (or (string= (mew-sort-key x) (mew-sort-key y))
      (string< (mew-sort-key x) (mew-sort-key y))))

(defsubst mew-sort-number (x y)
  (<= (mew-sort-key x) (mew-sort-key y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort body
;;;

(defvar mew-sort-line nil)

(defun mew-sort-index (x) (car x))

(defun mew-sort-insert (line msg)
  (insert line)
  (save-excursion
    (forward-line -1)
    (mew-syntax-change-message-number msg)))

;; If not found, returns nil.
(defun mew-summary-sort-rename (src dst pos &optional lastp)
  (if (mew-debug 'sort)
      (mew-elet
       (insert (format "move %s to %s\n" src dst)))
    (mew-elet
     (let (beg end line)
       (rename-file src dst)
       (mew-refile-change src dst)
       (cond
	(lastp
	 (when pos
	   (goto-char pos)
	   (mew-sort-insert mew-sort-line dst)
	   nil))
	((null pos) ;; first
	 (goto-char (point-min))
	 (when (re-search-forward (mew-regex-sumsyn-msg src) nil t)
	   (beginning-of-line)
	   (setq beg (point))
	   (forward-line)
	   (setq end (point))
	   ;; We need to keep properties in Summary mode.
	   ;; This must be "buffer-substring".
	   (setq mew-sort-line (buffer-substring beg end))
	   (delete-region beg end)
	   beg))
	(t
	 (goto-char (point-min))
	 (when (re-search-forward (mew-regex-sumsyn-msg src) nil t)
	   (beginning-of-line)
	   (setq beg (point))
	   (forward-line)
	   (setq end (point))
	   (cond
	    ((< pos beg)
	     ;; We need to keep properties in Summary mode.
	     ;; This must be "buffer-substring".
	     (setq line (buffer-substring beg end))
	     (goto-char end)
	     (delete-region beg end)
	     (save-excursion
	       (goto-char pos)
	       (mew-sort-insert line dst))
	     (point))
	    ((= pos beg)
	     ;; We need to keep properties in Summary mode.
	     ;; This must be "buffer-substring".
	     (setq line (buffer-substring beg end))
	     (delete-region beg end)
	     (goto-char pos)                    
	     (mew-sort-insert line dst)
	     (point))
	    (t
	     ;; We need to keep properties in Summary mode.
	     ;; This must be "buffer-substring".
	     (setq line (buffer-substring beg end))
	     (goto-char pos)
	     (mew-sort-insert line dst)
	     (delete-region beg end)
	     beg)))))))))

(defun mew-summary-sort (&optional arg)
  "Sort messages and list them up again.
If called with '\\[universal-argument]', sort the region.
After sorting, the cursor moves onto the beginning of the buffer
or the region. 

If this command is used in a remote folder, local cache messages are
sorted. But if you do so, '\\<mew-summary-mode-map>\\[mew-summary-ls]' + 'sync' would not work well."
  (interactive "P")
  (mew-summary-only
   (mew-summary-local-only
    (mew-summary-not-in-queue
     (mew-summary-not-in-draft
      (mew-summary-with-mewl
       (when (mew-summary-exclusive-p)
	 (let ((folder (mew-summary-folder-name)))
	   (if (null folder)
	       (message "No message")
	     (if (mew-mark-active-p) (setq arg t))
	     (mew-summary-sort-body folder arg))))))))))

(defun mew-summary-sort-body (folder arg)
  (let ((win (selected-window))
	key type idx files range beg end)
    ;;
    ;; Summary cache updates
    ;;
    (mew-summary-reset)
    (mew-summary-retrieve-gap folder)
    ;;
    ;; Determining range
    ;;
    (let (region rbeg rend)
      (if arg
	  (progn
	    (setq region (mew-summary-get-region))
	    (setq beg (car region))
	    (setq end (cdr region))
	    (if (= beg end) (error "No region")))
	(setq beg (point-min))
	(setq end (point-max)))
      (goto-char beg)
      (setq rbeg (mew-summary-message-number))
      (goto-char end)
      (forward-line -1)
      (setq rend (mew-summary-message-number))
      (forward-line)
      (if (and rbeg rend)
	  (setq range (concat rbeg "-" rend))
	(error "No region")))
    ;;
    ;; Asking a sort key after range
    ;;
    (let* ((sort-key (or (cdr (assoc folder mew-sort-default-key-alist))
			 mew-sort-default-key))
	   (key-type (mew-input-sort-key sort-key)))
      (setq type (cdr key-type))
      (setq key (concat (capitalize (car key-type)) ":")))
    ;;
    (if arg
	(message "Sorting %s: %s..." folder range)
      (message "Sorting %s..." folder))
    (mew-summary-lock t "Sorting")
    (condition-case nil
	(progn
	  ;;
	  ;; Calling mewl
	  ;;
	  (let* ((funcs (assoc type mew-sort-switch))
		 (func1 (nth 1 funcs))
		 (func2 (nth 2 funcs))
		 (i 0)
		 (fld (mew-expand-folder2 folder))
		 num med value ent)
	    (with-temp-buffer
	      (call-process mew-prog-mewl nil t nil
			    "-b" mew-mail-path "-l" "0"
			    "-d" key "-s" (concat fld " " range))
	      (goto-char (point-min))
	      (while (not (eobp))
		(if (not (looking-at "^\\([0-9]+\\)[ \t]*:[ \t]*"))
		    (forward-line)
		  (setq num (mew-match-string 1))
		  (setq med (match-end 0))
		  (forward-line)
		  (mew-header-goto-next)
		  (mew-header-decode-region key med (point))
		  (setq value (mew-buffer-substring med (1- (point))))
		  (setq files (cons num files))
		  (setq ent (cons (cons i (funcall func1 value folder num)) ent))
		  (setq i (1+ i)))))
	    (setq files (vconcat (nreverse files)))
	    (setq ent (sort ent func2))
	    (setq idx (vconcat (mapcar 'mew-sort-index ent))))
	  ;;
	  (when (mew-debug 'sort)
	    (mew-window-configure 'message)
	    ;; message buffer
	    (mew-elet
	     (mew-erase-buffer)
	     (insert "Sort as follows:\n")))
	  ;; for C-xC-x
	  (when (and (not (mew-debug 'sort)) (not (eobp)))
	    (beginning-of-line)
	    (mew-elet
	     (put-text-property (point) (1+ (point)) 'mew-sort-orig t)))
	  ;;
	  (if arg (narrow-to-region beg end))
	  ;;
	  ;;         sorted        sorted
	  ;;   files    idx    ->  files
	  ;; 0    10      1        (was 20)
	  ;; 1    20      2        (was 30)
	  ;; 2    30      0        (was 10)
	  ;;      31(new)
	  ;;
	  ;;     
	  ;;     src                dst
	  ;; 10  0 (*a)       31 (*b)
	  ;; 20  1 idx[0]     10    0
	  ;; 30  2 idx[1]     20    1
	  ;; 31  0 idx[2]     30    2
	  ;;     (*c)
	  ;; *a: initial src is 0
	  ;; *b: initial files[dst] is 31 (new tmp file)
	  ;; *c: break condition, src is looped!
	  ;;     files[src] is 31 (new tmp file)
	  ;;
	  (let* ((dir (mew-expand-folder folder))
		 (default-directory dir)
		 (len (length idx));; not (length files)
		 (tmp (mew-folder-new-message folder 'num-only))
		 (i 0)
		 src dst pos)
	    (while (< i len)
	      (setq mew-sort-line nil)
	      (unless (= i (aref idx i))
		(setq dst len)
		(setq src i)
		(setq pos (mew-summary-sort-rename (aref files src) tmp nil))
		(catch 'loop
		  (while t
		    (setq dst src)
		    (setq src (aref idx dst))
		    (if (= src i) (throw 'loop nil))
		    (setq pos (mew-summary-sort-rename (aref files src) (aref files dst) pos))
		    (aset idx dst dst)))
		(mew-summary-sort-rename tmp (aref files dst) pos 'last)
		(aset idx dst dst))
	      (setq i (1+ i))))
	  ;; The cursor moves onto point-min of the region.
	  (unless (mew-debug 'sort)
	    (goto-char (point-min)))
	  (if arg (widen))
	  ;;
	  (cond
	   ((mew-debug 'sort)
	    (mew-message-clear-end-of)
	    (set-buffer-modified-p nil)
	    (goto-char (point-min))
	    (select-window win))
	   (t
	    (let ((orig (next-single-property-change
			 (point-min) 'mew-sort-orig)))
	      ;; 'mew-sort-orig may start with bob.
	      (if (null orig)
		  (mew-push-mark)
		(save-excursion
		  (goto-char orig)
		  (beginning-of-line)
		  (setq orig (point)))
		(mew-elet
		 ;; 'mew-sort-orig is copied onto the entire message
		 ;; number. (window-width) is long enough to remove
		 ;; it.
		 (remove-text-properties
		  orig (+ orig (window-width)) '(mew-sort-orig nil)))
		(mew-push-mark)))
	    (mew-summary-folder-cache-save)
	    (set-buffer-modified-p nil)))
	  ;;
	  (mew-summary-unlock)
	  (unless (mew-debug 'sort)
	    (run-hooks 'mew-sort-hook))
	  (if arg
	      (message "Sorting %s: %s...done" folder range)
	    (message "Sorting %s...done" folder)))
      (quit
       (select-window win)
       (set-buffer-modified-p nil)
       (mew-summary-unlock)))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Packing
;;;

(defun mew-summary-pack-rename (src dst)
  (mew-elet
   (rename-file src dst)
   (mew-refile-change src dst)
   (when (re-search-forward (mew-regex-sumsyn-msg src) nil t)
     (mew-syntax-change-message-number2 dst)
     (forward-line))))

(defun mew-summary-pack ()
  "Pack messages and list them up again.
After packing, the cursor stays in the current message.
If this command is used in a remote folder,
local cache messages are packed."
  (interactive)
  (mew-summary-only
   (mew-summary-local-only
    (mew-summary-not-in-queue
     (mew-summary-not-in-draft
      (when (mew-summary-exclusive-p)
	(let ((folder (mew-summary-folder-name)))
	  (cond
	   ((null folder)
	    (message "No message"))
	   ((or (not mew-ask-pack) (y-or-n-p (format "Pack %s? " folder)))
	    (mew-summary-pack-body folder))))))))))

(defun mew-summary-pack-body (folder)
  (let* ((dir (mew-expand-folder folder))
	 (default-directory dir)
	 (n 1)
	 msgs msg src dst)
    ;;
    (mew-summary-reset)
    (mew-summary-retrieve-gap folder)
    ;;
    (message "Packing %s..." folder)
    (mew-summary-lock t "Packing")
    (condition-case nil
	(progn
	  (setq msgs (mew-dir-messages "."))
	  (setq msgs (mapcar 'string-to-int msgs))
	  (setq msgs (sort msgs '<)) ;; sort is inevitable
	  ;; the cursor stays the current position.
	  (save-excursion
	    (goto-char (point-min))
	    (while msgs
	      (setq msg (car msgs))
	      (setq msgs (cdr msgs))
	      (setq src (int-to-string msg))
	      (cond
	       ((= msg n);; including src is a directory
		(setq n (1+ n)))
	       ((file-directory-p src)
		)
	       (t
		(setq dst (int-to-string n))
		(while (file-exists-p dst)
		  (setq n (1+ n))
		  (setq dst (int-to-string n)))
		(mew-summary-pack-rename src dst)
		(setq n (1+ n))))))
	  (mew-summary-folder-cache-save)
	  (set-buffer-modified-p nil)
	  (mew-summary-unlock)
	  (run-hooks 'mew-pack-hook)
	  (message "Packing %s...done (the last is %d)" folder (1- n)))
      (quit
       (set-buffer-modified-p nil)
       (mew-summary-unlock)))))

(provide 'mew-sort)

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

;;; mew-sort.el ends here
