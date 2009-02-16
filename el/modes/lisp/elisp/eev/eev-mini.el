;;; eev-bounded.el -- support for bounded functions for eev.

;; Copyright (C) 2006 Free Software Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    2006sep19
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-mini.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-mini.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>

;;; Commentary:

;; This file is not really intended to be loaded... it contains
;; simplistic implementations of some of the main functions of eev.
;;
;; If you understand all the functions here and how to use them in
;; your e-scripts then you would have grasped most of the main ideas
;; of eev.
;;
;; This is not ready yet.




;; (find-eevfile "eev.el")




;;;  _               _        _ _       _        
;;; | |__   __ _ ___(_) ___  | (_)_ __ | | _____ 
;;; | '_ \ / _` / __| |/ __| | | | '_ \| |/ / __|
;;; | |_) | (_| \__ \ | (__  | | | | | |   <\__ \
;;; |_.__/ \__,_|___/_|\___| |_|_|_| |_|_|\_\___/
;;;                                              
;;; Basic links: find-fline and find-noe

(defun find-fline (fname &rest pos-spec-list)
  "Hyperlink to a file (or a directory).
This function is similar to `find-file' but it supports a
\"pos-spec-list\" - see `ee-goto-position'.
Examples:\n
  (find-file  \"~/.emacs\")
  (find-fline \"~/.emacs\")
  (find-fline \"~/.emacs\" \"Beginning of the eev block\")"
  (find-file (ee-expand fname))
  (apply 'ee-goto-position pos-spec-list))

(defun find-node (nodestr &rest pos-spec-list)
  "Hyperlink to an info page.
This function is similar to `info' but it supports a
\"pos-spec-list\" - see `ee-goto-position'.
Examples:\n
  (info \"(emacs)Lisp Eval\")
  (find-node \"(emacs)Lisp Eval\" \"C-x C-e\")"
  (Info-goto-node nodestr)
  (apply 'ee-goto-position pos-spec-list))

(defun ee-goto-position (&optional pos-spec &rest rest)
  "Process the \"absolute pos-spec-lists\" arguments in hyperlink functions."
  (when pos-spec
    (cond ((numberp pos-spec)		; pos-spec is a number? (say, 42)
	   (goto-char (point-min))	; jump to the line 42
	   (forward-line (1- pos-spec)))
	  ((stringp pos-spec)		; pos-spec is a string? (say, "foo")
	   (goto-char (point-min))	; jump to the first occurrence of "foo"
	   (search-forward pos-spec))
	  (t (error "This is not a valid pos-spec: %S" pos-spec)))
    (if rest (ee-goto-rest rest))))

(defun ee-goto-rest (list)
  "Process \"relative pos-spec-lists\"."
  (cond ((null list))
	((stringp (car list))		; a string?
	 (search-forward (car list))	; jump to its next occurrence
	 (ee-goto-rest (cdr list)))
	((numberp (car list))		; a number?		 
	 (forward-line (car list))	; advance that many lines
	 (ee-goto-rest (cdr list)))
	((consp (car list))		; a cons?
	 (eval (car list))		; eval it
	 (ee-goto-rest (cdr list)))
	(t (error "Not a valid pos-spec item: %S" (car list)))))




;;;                _                          _ 
;;;   ___ ___   __| | ___        ___       __| |
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |
;;;  \___\___/ \__,_|\___|      \___|     \__,_|
;;;                                             

(defun ee-code-c-d (c d info)
  (format "
    (setq ee-%sdir %S)
    (defun ee-%sfile (str)
      (concat (ee-expand ee-%sdir) str))
    (defun find-%sfile (str &rest pos-spec-list)
      (interactive (list \"\"))
      (apply 'find-fline (ee-%sfile str) pos-spec-list))
    (defun find-%snode (page &rest pos-spec-list)
      (interactive (list \"\"))
      (apply 'find-node (format \"(%s)%%s\" page) pos-spec-list))
    " c d   c c  c c   c (or info "---"))

(defun code-c-d (c d &optional info)
  (eval (read (ee-code-c-d c d info))))

(defun find-code-c-d (c d &optional info)
  "Use this to inspect the code that a `code-c-d' would run."
  (find-estring (ee-code-c-d c d info)))




;;;        _             _         
;;;   __ _| |_   _ _ __ | |__  ___ 
;;;  / _` | | | | | '_ \| '_ \/ __|
;;; | (_| | | |_| | |_) | | | \__ \
;;;  \__, |_|\__, | .__/|_| |_|___/
;;;  |___/   |___/|_|              
;;;
;;; Set just one glyph: the red star

(defface eev-glyph-face-red '((t (:foreground "red")))
  "Face used for the red star glyph (char 15).")

(defun ee-glyph (char &optional face)
  (logior char (ash (if face (face-id face) 0) 19)))

(defun eev-set-glyph (pos &optional char face)
  (aset standard-display-table pos
	(if char (vector (ee-glyph char face)))))

;; Make `^O's appear at red stars.
;; To cancel that run this: (eev-set-glyph ?\^O)
(eev-set-glyph ?\^O ?* 'eev-glyph-face-red)



;;;             __ _           _     
;;;   ___  ___ / _| | __ _ ___| |__  
;;;  / _ \/ _ \ |_| |/ _` / __| '_ \ 
;;; |  __/  __/  _| | (_| \__ \ | | |
;;;  \___|\___|_| |_|\__,_|___/_| |_|
;;;                                  

(defvar ee-flash-spec '(highlight 0.75))

(defun eeflash (s e)
  "Highlight temporarily the region between S and E. See `ee-flash-spec'."
  (interactive "r")
  (if (numberp s)
      (let ((ovl (make-overlay s e))
	    (face     (car  ee-flash-spec))
	    (duration (cadr ee-flash-spec)))
	(overlay-put ovl 'face face)
    (run-at-time duration nil 'delete-overlay ovl)))
  (ee-se-to-string s e))


;;;  _                           _          _    __                 
;;; | |__   ___  _   _ _ __   __| | ___  __| |  / _|_   _ _ __  ___ 
;;; | '_ \ / _ \| | | | '_ \ / _` |/ _ \/ _` | | |_| | | | '_ \/ __|
;;; | |_) | (_) | |_| | | | | (_| |  __/ (_| | |  _| |_| | | | \__ \
;;; |_.__/ \___/ \__,_|_| |_|\__,_|\___|\__,_| |_|  \__,_|_| |_|___/
;;;                                                                 

(defvar eeb-defaults '(eev ee-delimiter-hash))

(defun eeb-default ()
  "Run the default action on a delimited region around point.
The default action is determined by the entries in `eeb-defaults'.
See `eeb-define'."
  (interactive)
  (let* ((fun   (nth 0 eeb-defaults))
	 (delim (nth 1 eeb-defaults))
	 (s     (ee-search-backward edelim))
	 (e     (ee-search-forward  edelim)))
    (eeflash s e)
    (funcall fun s e)))

(defun ee-eeb-define (eeb-fun fun delim)
"Returns code (as as string) to define EEB-FUN as a wrapper around FUN."
  (read (format "
    (defun %S ()
      (interactive)
      (setq eeb-defaults '(%S %S))
      (eeb-default-new))" eeb-fun fun delim))))

(defun eeb-define (eeb-fun fun delim)
"Define EEB-FUN as a wrapper around FUN."
  (eval (read (ee-eeb-define eeb-fun fun delim))))

(eeb-define 'eev-bounded     'eev     "\n#\n")
(eeb-define 'eegdb-bounded   'eegdb   "\n#\n")
(eeb-define 'eelatex-bounded 'eelatex "\n%\n")
(eeb-define 'eeeval-bounded  'eeeval  "\n;;\n")
(eeb-define 'eeb-eval        'eeeval  "\n;;\n")




;;;                                              
;;;  _ __  _ __ ___   ___ ___  ___ ___  ___  ___ 
;;; | '_ \| '__/ _ \ / __/ _ \/ __/ __|/ _ \/ __|
;;; | |_) | | | (_) | (_|  __/\__ \__ \  __/\__ \
;;; | .__/|_|  \___/ \___\___||___/___/\___||___/
;;; |_|                                          

;; "-ne" means "(do) not ee-expand"

(defun ee-split   (str) (if (stringp str) (split-string str "[ \t]+") str))
(defun ee-unsplit (list) (if (listp list) (mapconcat 'identity list " ") list))
(defun ee-split-and-expand (str) (mapcar 'ee-expand (ee-split str)))
(defun ee-no-trailing-nl   (str) (replace-regexp-in-string "\n$" "" str))

(defun find-bgprocess-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (apply 'start-process (car argv) "*Messages*" argv)))

(defun find-callprocess00-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (with-output-to-string
      (with-current-buffer standard-output
	(apply 'call-process (car argv) nil t nil (cdr argv))))))

(defun find-callprocess0-ne (program-and-args)
  (ee-no-trailing-nl (find-callprocess00 program-and-args)))

(defun find-comintprocess-ne (name program-and-args)
  (let ((argv (ee-split program-and-args)))
    (apply 'make-comint name (car argv) nil (cdr argv))
    (switch-to-buffer (format "*%s*" name))))

(defun find-bgprocess     (program-and-args)
  (find-bgprocess-ne      (ee-split-and-expand program-and-args)))
(defun find-callprocess00 (program-and-args)
  (find-callprocess00-ne  (ee-split-and-expand program-and-args)))
(defun find-callprocess0  (program-and-args)
  (find-callprocess0-ne   (ee-split-and-expand program-and-args)))
(defun find-comintprocess (name program-and-args)
  (find-comintprocess-ne   name (ee-split-and-expand program-and-args)))




;;;   __ _           _           _     
;;;  / _(_)_ __   __| |      ___| |__  
;;; | |_| | '_ \ / _` |_____/ __| '_ \ 
;;; |  _| | | | | (_| |_____\__ \ | | |
;;; |_| |_|_| |_|\__,_|     |___/_| |_|
;;;                                    
;; other links: from (find-eev "eev.el")
;; missing: eeman

(defun find-eoutput-rerun (buffer-name code &rest pos-spec-list)
  (if (get-buffer buffer-name)		  ; if the buffer exists
      (if (not (kill-buffer buffer-name)) ; try to kill it; confirm if needed
	  (error "Not killing the buffer %s" buffer-name)))
  (switch-to-buffer buffer-name)	  ; create the buffer
  (eval code)				  ; always run CODE on the empty buffer
  (goto-char (point-min))
  (apply 'ee-goto-position pos-spec-list))

(defun find-eoutput-reuse (buffer-name code &rest pos-spec-list)
  (if (get-buffer buffer-name)		; if the buffer exists
      (switch-to-buffer buffer-name)	; then just switch to it
    (switch-to-buffer buffer-name)	; otherwise switch to it and
    (eval code)				; run CODE to produce its contents
    (goto-char (point-min)))
  (apply 'ee-goto-position pos-spec-list))

(defun find-sh (command &rest pos-spec-list)
  (apply 'find-eoutput-reuse
	 (ee-no-trailing-nl command)
	 `(insert (shell-command-to-string ,command))
	 pos-spec-list))

(defun find-man (manpage &rest pos-spec-list)
  (apply 'find-sh (format "PAGER=cat man %s | col -bx" manpage)
	 pos-spec-list))





;;;                 _                 
;;;   ___  ___  ___| |_ ___ _ __  ___ 
;;;  / _ \/ _ \/ __| __/ _ \ '_ \/ __|
;;; |  __/  __/\__ \ ||  __/ |_) \__ \
;;;  \___|\___||___/\__\___| .__/|___/
;;;                        |_|        
;; Steppers - just one, from: (find-eev "eev-steps.el")
;; eek
;; eek0
;; eesteps

(defun eesteps (list)
  "Set the LIST of steps that `eesteps-do-step' will execute.\n
Here's an example: run\n
  (eesteps '(\"C-x b * scratch * RET   ;;; change to the buffer *scratch*\"
             \"foobar\"
             \"3*<left>\"
             (insert \"!\")))\n
then type \\[eesteps-do-step] four times.\n
Each step is either a string -- meaning a series of keys, in the
format used by `edmacro-mode' -- or a sexp to be evaluated."
  (setq eesteps-pos 0)
  (setq eesteps-list list)
  `(,(length list) steps stored - use <f12> to execute a step))

(defun eek (s &optional e count)
  "Execute the region between S and E (or the string S) as a keyboard macro.
See `edmacro-mode' for the exact format.\n
An example: (eek \"C-x 4 C-h\")"
  (interactive "r")
  (execute-kbd-macro (read-kbd-macro (ee-se-to-string s e)) count))

(defun eek0 (kbmacro &optional count)
  "This is similar to `eek', but uses the low-level formats for macros.
Example: (eek \"\\C-x4\\C-h\")"
  (execute-kbd-macro kbmacro count))

(defun eesteps-perform (step &rest rest)
  (if (stringp step)
      (eek step)
    (eval step))
  (if rest (apply 'eesteps-eval rest)))

(defun eesteps-do-step (&optional arg)
  (interactive "P")
  (if (>= eesteps-pos (length eesteps-list))
      (error "No more steps"))
  (if (eq arg 0)
      (message "Next step: %d = %S" eesteps-pos (nth eesteps-pos eesteps-list))
    (eesteps-perform (nth eesteps-pos eesteps-list))
    (setq eesteps-pos (1+ eesteps-pos))))





;;;  _                                    
;;; | | _____ _   _ _ __ ___   __ _ _ __  
;;; | |/ / _ \ | | | '_ ` _ \ / _` | '_ \ 
;;; |   <  __/ |_| | | | | | | (_| | |_) |
;;; |_|\_\___|\__, |_| |_| |_|\__,_| .__/ 
;;;           |___/                |_|    
;; keymap: (find-eevfile "eev.el" "\n(defvar eev-mode-map")
;; M-e

(defvar eev-mode-map nil)
(if eev-mode-map
    ()
(setq eev-mode-map (make-sparse-keymap))
(define-key eev-mode-map "\M-E" 'eval-last-sexp)        ;     C-x C-e
(define-key eev-mode-map "\M-e" 'ee-eval-sexp-eol)      ; C-e C-x C-e
(define-key eev-mode-map "\M-k" 'kill-this-buffer)      ; convenience
(define-key eev-mode-map "\M-K" 'bury-buffer)           ; convenience
(define-key eev-mode-map [f3]   'eeb-default)
(define-key eev-mode-map [f12]  'eesteps-do-step)
(define-key eev-mode-map "\M-h\M-f" 'find-efunction-links) ; in eev-insert.el
)





;; variables
;; pop-up windows







;; Local Variables:
;; coding:          raw-text-unix
;; no-byte-compile: t
;; End:
