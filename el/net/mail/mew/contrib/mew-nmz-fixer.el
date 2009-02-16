;; -*- mode: Emacs-Lisp -*-
;;
;;; mew-nmz-fixer.el
;; 
;; Author: Hideyuki SHIRAI <shirai@mew.org>
;; Created: 2001-01-14(Sun)
;;
;; nmz-mew-summary-fixer.pl を使って、pack や sort した後に
;; mew-nmz 系 Namazu index を最新に保ちます。
;;
;;; nmz-mew-summary-fixer.pl は Rei FURUKAWA さん <furukawa@tcp-ip.or.jp> 
;;; が作成した message と index の対応更新 Program です。
;;
;;
;;;; 使い方 (まず mew-nmz を使えるようにしておきましょう)
;; (eval-after-load "mew-nmz" '(require 'mew-nmz-fixer))
;;

(eval-when-compile
  (require 'mew)
  (require 'mew-nmz))

;;;; こんな感じで key に bind するも良し。

(add-hook 'mew-summary-mode-hook
 	  (lambda ()
	    (define-key mew-summary-mode-map "zf" 'mew-nmz-fixer-exec)))

;; add-hook
(add-hook 'mew-sort-hook 'mew-nmz-fixer-exec)
(add-hook 'mew-pack-hook 'mew-nmz-fixer-exec)

;; variable
(defvar mew-nmz-prog-fixer "nmz-mew-summary-fixer.pl"
  "*nmz-mew-summary-fixer.pl の Program 名。
Windows の人は \"nmz-mew-summary-fixer\" のように拡張子を無くした方が幸せかも。")

(defvar mew-nmz-prog-fixer-args
  (if (memq system-type '(OS/2 emx windows-nt))
      '("--windows-drive") '())
  "*nmz-mew-summary-fixer.pl に与える option。")

;; internal variable
(defvar mew-nmz-fixer-process nil)
(make-variable-buffer-local 'mew-nmz-fixer-process)

;; function
(defun mew-nmz-fixer-exec (&optional arg)
  "Sort/Pack の後で \"mew-nmz-summary-fixer\" を起動する。
\"Another fixer running.\" といわれて困ったときは、'\\[universal-argument]' 付きで呼びましょう。"
  (interactive "P")
  (mew-summary-only
   (let* ((interactive-p (interactive-p))
	  (fld (if interactive-p
		   (mew-input-folder (mew-sinfo-get-case)
				     (mew-summary-folder-name))
		 (mew-summary-folder-name)))
	  (fixer (if (fboundp 'mew-which-exec)
		     (mew-which-exec mew-nmz-prog-fixer)
		   (mew-which mew-nmz-prog-fixer exec-path)))
	  (args mew-nmz-prog-fixer-args)
	  nmzdir)
     (if arg
	 (progn
	   (setq mew-nmz-fixer-process nil)
	   (and interactive-p
		(message "Set %s to free" mew-nmz-prog-fixer)))
       (if (not (and fld fixer))
	   (and interactive-p
		(message "Cannot exec %s" mew-nmz-prog-fixer))
	 (setq nmzdir (mew-nmz-expand-folder fld))
	 (if mew-nmz-fixer-process
	     (message "Another fixer running")
	   (if (and
		nmzdir
		(file-exists-p nmzdir)
		(file-directory-p nmzdir)
		(file-exists-p (expand-file-name "NMZ.i" nmzdir))
		(file-exists-p (expand-file-name "NMZ.field.message-id" nmzdir))
		(file-exists-p (expand-file-name "NMZ.field.size" nmzdir))
		(file-exists-p (expand-file-name "NMZ.field.date" nmzdir))
		(not (file-name-all-completions "NMZ.lock" nmzdir)))
	       (progn
		 (and interactive-p
		      (message "Exec %s (%s)..." mew-nmz-prog-fixer fld))
		 (mew-nmz-timestamp-new fld)
		 (setq mew-nmz-fixer-process
		       (apply (function start-process)
			      "nmz-fixer" (current-buffer) fixer
			      (append args
				      (list (mew-expand-folder fld) nmzdir))))
		 (set-process-sentinel mew-nmz-fixer-process 'mew-nmz-fixer-sentinel))
	     (and interactive-p
		  (message "Cannot exec %s at %s" mew-nmz-prog-fixer nmzdir)))))))))

(defun mew-nmz-fixer-sentinel (process event)
  (save-excursion
    (when (and (get-buffer (process-buffer process))
	       (buffer-live-p (process-buffer process)))
      (set-buffer (process-buffer process))
      (setq mew-nmz-fixer-process nil)
      (mew-nmz-timestamp-rename (buffer-name))
      (message "Exec %s (%s)...done"
	       mew-nmz-prog-fixer (buffer-name)))))

(provide 'mew-nmz-fixer)

;;; Copyright Notice:

;; Copyright (C) 2001 Mew developing team.
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

;;; mew-nmz-fixer.el ends here
