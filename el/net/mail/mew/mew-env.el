;;; mew-env.el --- Environment setup for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  6, 1997

;;; Code:

(require 'mew)

;; to avoid competition with mh-e.el, sigh.
(let ((ent (rassq 'mh-letter-mode auto-mode-alist)))
  (and ent (setq auto-mode-alist (delq ent auto-mode-alist))))

(defvar mew-connection-type1 nil
  "Connection type for many processes. 't' means PTY and 'nil' means PIPE.
PIPE is usually recommended for speed but some OSes such as Linux 
requires PTY.")

(defvar mew-connection-type2 t
  "Connection type for processes that requires a password.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Must be here for key setup
;;;

(cond
 ((fboundp 'set-keymap-parents)	;; for XEmacs, should be first
  (defalias 'mew-set-keymap-parent 'set-keymap-parents))
 ((fboundp 'set-keymap-parent)	;; for Emacs
  (defalias 'mew-set-keymap-parent 'set-keymap-parent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacsen
;;;

(defvar mew-temacs-p nil)
(defvar mew-gemacs-p nil)
(defvar mew-xemacs-p nil)
(defvar mew-mule-p   nil)
(defvar mew-icon-p   nil)
(defvar mew-mule-ver 0)
(defvar mew-internal-utf-8p nil)

(if (featurep 'mule) (setq mew-mule-p t))

(require 'easymenu) ;; to prepend the loading messages
(require 'pp) ;; to prevent the loading messages
(cond
 ((featurep 'xemacs)
  (setq mew-temacs-p nil)
  (setq mew-gemacs-p nil)
  (setq mew-xemacs-p t)
  (if (and (featurep 'toolbar) (featurep 'xpm))
      (setq mew-icon-p t))
  (require 'mew-key)
  (require 'mew-xemacs)
  (if (null mew-mule-p)
      (require 'mew-mule0)
    (setq mew-mule-ver 3)
    (require 'mew-mule3)))
 (t
  (setq mew-temacs-p t)
  (setq mew-gemacs-p nil)
  (setq mew-xemacs-p nil)
  (require 'mew-key)
  (if (string< emacs-version "21")
      (require 'mew-temacs)
    (if (featurep 'tool-bar) (setq mew-icon-p t))
    (setq mew-gemacs-p t)
    (require 'image) ;; to prepend the loading messages
    (require 'jit-lock) ;; to prepend the loading messages
    (require 'regexp-opt) ;; to prepend the loading messages
    (require 'mew-gemacs)
    (if (fboundp 'utf-translate-cjk-mode) ;; Emacs 21.3.50 or later
	(setq mew-internal-utf-8p t)))
  (setq mew-mule-ver 3)
  (require 'mew-mule3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs vs XEmacs
;;;

(cond 
 (mew-xemacs-p
  (or (find-face 'underline)
      (progn
	(make-face 'underline)
	(set-face-underline-p 'underline t)))
  ;;
  (defsubst mew-mark () (mark t))
  ;;
  (add-hook 'pre-idle-hook 'mew-summary-cook-window)
  ;;
  (defun mew-md5 (str)
    (md5 str nil nil 'binary)))
 ;;
 (mew-temacs-p
  (if window-system (require 'faces))
  ;;
  (require 'easymenu)
  ;;
  (defsubst mew-mark () (marker-position (mark-marker)))
  ;;
  (if (string< emacs-version "21")
      (require 'mew-md5)
    (defun mew-md5 (str)
      (md5 str nil nil 'binary)))))

(if mew-xemacs-p
    (defalias 'mew-window-edges 'window-pixel-edges)
  (defalias 'mew-window-edges 'window-edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun mew-timer (sec func) (run-at-time sec sec func))

(if (fboundp 'characterp)
    (defalias 'mew-characterp 'characterp)
  (defalias 'mew-characterp 'integerp))

(if (fboundp 'mouse-region-match)
    (defalias 'mew-mouse-region-p 'mouse-region-match)
  (defsubst mew-mouse-region-p () nil))

(cond
 ((boundp 'auto-hscroll-mode) ;; Emacs 21.3.50 or later
  (defsubst mew-hscroll ()
    (set (make-local-variable 'auto-hscroll-mode) t)))
 ((boundp 'automatic-hscrolling) ;; Emacs 21.3 or earlier
  (defsubst mew-hscroll ()
    (set (make-local-variable 'automatic-hscrolling) t)))
 (t
  (defsubst mew-hscroll () (auto-show-mode 1))))

(if (fboundp 'minibuffer-prompt-end)
    (defalias 'mew-minibuf-point-min 'minibuffer-prompt-end)
  (defalias 'mew-minibuf-point-min 'point-min))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sticky
;;;

(cond 
 (mew-xemacs-p
  (require 'overlay)
  (defvar mew-fs  'start-closed)
  (defvar mew-fsv t)
  (defvar mew-fn  'start-open)
  (defvar mew-fnv t)
  (defvar mew-rs  'end-open)
  (defvar mew-rsv nil)
  (defvar mew-rn  'end-open)
  (defvar mew-rnv t)
  (defvar mew-local-map 'keymap))
 (mew-temacs-p
  (defvar mew-fs  'front-sticky)
  (defvar mew-fsv t)
  (defvar mew-fn  'front-sticky)
  (defvar mew-fnv nil)
  (defvar mew-rs  'rear-nonsticky)
  (defvar mew-rsv nil)
  (defvar mew-rn  'rear-nonsticky)
  (defvar mew-rnv t)
  (defvar mew-local-map 'local-map)))

(defsubst mew-front-sticky (beg end)
  (put-text-property beg end mew-fs mew-fsv))

(defsubst mew-front-nonsticky (beg end)
  (put-text-property beg end mew-fn mew-fnv))

(defsubst mew-rear-sticky (beg end)
  (put-text-property beg end mew-rs mew-rsv))

(defsubst mew-rear-nonsticky (beg end)
  (put-text-property beg end mew-rn mew-rnv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Overlay
;;;

(defsubst mew-overlay-make (beg end)
  (let ((ovl (make-overlay beg end)))
    (overlay-put ovl 'mew t)
    ovl))

(defsubst mew-overlay-delete (ovl)
  (and (overlay-get ovl 'mew) (delete-overlay ovl)))

(defsubst mew-overlay-delete-region (beg end)
  "Delete overlays in the region."
  (interactive "r")
  (mapcar 'mew-overlay-delete (overlays-in beg end))
  (if (fboundp 'remove-images) (remove-images beg end))) ;; xxx

(defsubst mew-overlay-delete-buffer ()
  (save-restriction
    (widen)
    (mew-overlay-delete-region (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File operations
;;;

(if (or mew-xemacs-p mew-gemacs-p)
    (defalias 'mew-face-spec-set 'face-spec-set)
  ;; face-spec-set on Emacs 20.7 does not effect new frames.
  (defun mew-face-spec-set (face spec)
    "Set FACE's attributes according to the first matching entry in SPEC
on all frames."
    (custom-set-faces `(,face ,spec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File operations
;;;

(cond
 ((fboundp 'make-symbolic-link)
  (defsubst mew-symbolic-link (filename newname &optional OK-IF-ALREADY-EXISTS)
    (if (file-directory-p (file-chase-links filename))
	(error "Cannot make a symbolic link to directory")
      (make-symbolic-link filename newname OK-IF-ALREADY-EXISTS)))
  (defsubst mew-link (filename newname &optional OK-IF-ALREADY-EXISTS)
    (if (file-directory-p (file-chase-links filename))
	(error "Cannot make a link to directory")
      (condition-case nil
	  (add-name-to-file filename newname OK-IF-ALREADY-EXISTS)
	(file-error
	 (copy-file filename newname OK-IF-ALREADY-EXISTS 'keepdate))))))
 (t
  (defsubst mew-symbolic-link (filename newname &optional OK-IF-ALREADY-EXISTS)
    (if (file-directory-p filename)
	(error "Cannot make a copy of directory")
      (copy-file filename newname OK-IF-ALREADY-EXISTS 'keepdate)))
  (defsubst mew-link (filename newname &optional OK-IF-ALREADY-EXISTS)
    (if (file-directory-p filename)
	(error "Cannot make a copy of directory")
      (copy-file filename newname OK-IF-ALREADY-EXISTS 'keepdate)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Property
;;;

(defalias 'mew-buffer-substring 'buffer-substring-no-properties)

(if (fboundp 'match-string-no-properties)
    (defalias 'mew-match-string 'match-string-no-properties)
  (defalias 'mew-match-string 'match-string))

(defsubst mew-insert-buffer-substring (buf beg end)
  (insert (save-excursion (set-buffer buf) (mew-buffer-substring beg end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base64
;;;

(cond
 ((fboundp 'base64-encode-string)
  (defun mew-base64-encode-string (str)
    (base64-encode-string str 'no-line-break)))
 (t
  (defconst mew-base64-char64
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (defun mew-base64-encode-string (str256)
    (let* ((len (length str256))
	   (ret (make-string (* (/ (+ len 2) 3) 4) ?=))
	   (pad (% len 3))
	   (lim (- len pad))
	   (i -1) (j -1) c)
      (while (< (setq i (1+ i)) lim)
	(setq c (logior (lsh (aref str256 i) 16)
			(lsh (aref str256 (setq i (1+ i))) 8)
			(aref str256 (setq i (1+ i)))))
	(aset ret (setq j (1+ j))
	      (aref mew-base64-char64 (lsh c -18)))
	(aset ret (setq j (1+ j))
	      (aref mew-base64-char64 (logand (lsh c -12) 63)))
	(aset ret (setq j (1+ j))
	      (aref mew-base64-char64 (logand (lsh c -6) 63)))
	(aset ret (setq j (1+ j))
	      (aref mew-base64-char64 (logand c 63))))
      (cond
       ((= pad 1)
	(setq c (aref str256 i))
	(aset ret (setq j (1+ j))
	      (aref mew-base64-char64 (lsh c -2)))
	(aset ret (1+ j)
	      (aref mew-base64-char64 (lsh (logand c 3) 4))))
       ((= pad 2)
	(setq c (logior (lsh (aref str256 i) 8)
			(aref str256 (1+ i))))
	(aset ret (setq j (1+ j))
	      (aref mew-base64-char64 (lsh c -10)))
	(aset ret (setq j (1+ j))
	      (aref mew-base64-char64 (logand (lsh c -4) 63)))
	(aset ret (1+ j)
	      (aref mew-base64-char64 (logand (lsh c 2) 63)))))
      ret))))

(cond
 ((fboundp 'base64-decode-string)
  (defun mew-base64-decode-string (str64)
    (condition-case nil
	(let ((r (% (length str64) 4)))
	  (cond
	   ((= r 1)
	    (if (string-match "=$" str64)
		(setq str64 (substring str64 0 (match-beginning 0)))))
	   ((= r 2)
	    (if (string-match "==$" str64)
		(setq str64 (substring str64 0 (match-beginning 0)))
	      (setq str64 (concat str64 "=="))))
	   ((= r 3)
	    (if (string-match "===$" str64)
		(setq str64 (substring str64 0 (match-beginning 0)))
	      (setq str64 (concat str64 "=")))))
	  (base64-decode-string str64))
      (error nil))))
 (t
  (defconst mew-base64-char256
    (let ((i 0) (len (length mew-base64-char64)) (s (make-string 256 0)))
      (while (< i len)
	(aset s (aref mew-base64-char64 i) i)
	(setq i (1+ i)))
      s))
  (defun mew-base64-decode-string (str64)
    (let* ((len (length str64))
	   ret
	   (i 0) (j -1) (padlen 0) c)
      (if (string-match "=+$" str64)
	  (setq padlen (- (match-end 0) (match-beginning 0))))
      (cond
       ((or (string-match "[^a-zA-Z0-9+/=]" str64)
	    (not (zerop (logand len 3)))
	    (< padlen 0)
	    (> padlen 2))
	nil) ;; return value
       ((zerop (setq len (- len padlen))) "")
       (t
	(setq ret (make-string (/ (* len 3) 4) ?a))
	(while 
	    (progn 
	      (setq
	       c (logior
		  (lsh (aref mew-base64-char256 (aref str64 i)) 18)
		  (lsh (aref mew-base64-char256 (aref str64 (setq i (1+ i)))) 12)
		  (lsh (aref mew-base64-char256 (aref str64 (setq i (1+ i)))) 6)
		  (aref mew-base64-char256 (aref str64 (setq i (1+ i))))))
	      (aset ret (setq j (1+ j)) (lsh c -16))
	      (< (setq i (1+ i)) len))
	  (aset ret (setq j (1+ j)) (logand (lsh c -8) 255))
	  (aset ret (setq j (1+ j)) (logand c 255)))
	(if (< padlen 2)
	    (aset ret (1+ j) (logand (lsh c -8) 255)))
	(if (zerop padlen)
	    (aset ret (1+ (1+ j)) (logand c 255)))
	ret))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Language
;;;

;; to load the thread separator
(cond
 ((and (boundp 'current-language-environment)
       (string= current-language-environment "Japanese"))
  (require 'mew-lang-jp))
 ((and (boundp 'current-language-environment)
       (string= current-language-environment "Korean"))
  (require 'mew-lang-kr)))

(provide 'mew-env)

;;; Copyright Notice:

;; Copyright (C) 1997-2005 Mew developing team.
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

;;; mew-env.el ends here
