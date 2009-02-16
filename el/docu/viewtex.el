; Todo: 
; 
; Multiple xdvi processes, depending on file name. 
;
; frob TEXINPUTS in process-environment to include original working
; directory of source file, in case viewtex file is elsewhere.
;

;;; viewtex.el -- display regions of TeX files using an X DVI previewer.
;;;
;;; Copyright (C) 1993 Noah S. Friedman
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.
;;;
;;; Please send bug reports, etc. to friedman@prep.ai.mit.edu

(require 'comint)

;; For emacs 19 compatibility.
(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo 'buffer-flush-undo))


;;;###autoload
(defvar viewtex-after-load-hook nil
  "*Hook to run after loading everything else in viewtex.el.

This is obsolesced in Emacs 19 by `after-load-alist'.")

;;;###autoload
(defvar viewtex-mode-hook nil
  "*Hooks to run after setting current buffer to viewtex-mode.")

;;;###autoload
(defvar viewtex-file (expand-file-name "~/tmp/viewtex.tex")
  "*Path of temporary TeX file.")

;;;###autoload
(defvar viewtex-xdvi-command "xdvi"
  "*Name of X DVI previewer (usually xdvi or xtex).")


;; These are not user variables.

;; Unfortunately, tex will not exit with a nonzero exit status when you
;; do an emergency stop.  So to keep the tex sentinel from thinking it
;; should start xdvi, set a flag which indicates that tex meant to exit
;; with a nonzero exit status.
(defvar viewtex-tex-emergency-stop nil nil)

;; Process object for currently-running xdvi, if there is one.
(defvar viewtex-xdvi-process nil nil)

;; Name of temporary buffer for contructing viewtex-file. 
(defconst viewtex-viewtex-buffer " *TeX region*" nil)

;; Name of buffer for displaying errors and other random output from
;; xdvi. 
(defconst viewtex-xdvi-buffer " *xdvi output*" nil)


;;;###autoload
(defun viewtex (&optional command)
  "Document me."
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "viewtex command: " 
                                               "tex")
                       "tex")))
  (let ((process-connection-type nil)
        (cmd-buffer (get-buffer-create " *viewtex*"))
        proc)
    (and (comint-check-proc cmd-buffer)
         (error "viewtex is already running."))
    ;; If called in a noninteractive fashion, `command' may be nil. 
    (or command (setq command "tex"))
    ;; If buffer is narrowed, point-min != 0 or point-max != buffer-size.
    ;; Use all of narrowed region instead of just point+mark. 
    (if (or (/= (point-min) 1) (/= (point-max) (1+ (buffer-size))))
        (viewtex-make-tmpfile (current-buffer) (point-min) (point-max))
      (viewtex-make-tmpfile (current-buffer) (region-beginning) (region-end)))
    (display-buffer cmd-buffer)
    (save-excursion
      (set-buffer cmd-buffer)
      (setq default-directory (viewtex-directory))
      (kill-all-local-variables)
      ;; Don't waste memory on undo information for this buffer.
      (buffer-disable-undo cmd-buffer)
      (widen)
      (erase-buffer)
      (goto-char (point-min))
      (setq viewtex-tex-emergency-stop nil) ; reset
      (comint-exec (current-buffer) (buffer-name cmd-buffer) command nil
                   (list viewtex-file))
      (setq proc (get-buffer-process cmd-buffer))
      (set-marker (process-mark proc) (point-max))
      (set-process-sentinel proc 'viewtex-tex-sentinel)
      (set-process-filter proc 'viewtex-tex-filter)
      (viewtex-mode))))

;;;###autoload
(defun viewtex-mode ()
  "Set major mode for viewtex sessions. 
If `viewtex-mode-hook' is set, run it."
  (interactive)
  (comint-mode)
  ;; Good enough for now.  Might find one specially tuned to tex prompts
  ;; eventually.  
  (setq comint-prompt-regexp shell-prompt-pattern)
  (setq major-mode 'viewtex-mode)
  (setq mode-name "viewtex")
  (run-hooks 'viewtex-mode-hook))


;; Create a temporary buffer, copy region and check that all inputs are
;; present, adding them if necessary.  Save buffer in temporary file
;; defined by `viewtex-file'.
(defun viewtex-make-tmpfile (orig-buffer region-beg region-end)
  (let* ((orig-match-data (match-data))
         (buffer-name viewtex-viewtex-buffer)
         (buffer (get-buffer-create buffer-name))
         region
         inputs)
    (unwind-protect
        (save-excursion
          (save-restriction
            (set-buffer orig-buffer)
            (setq region (buffer-substring region-beg region-end))
            (widen)
            (goto-char (point-min))
            ;; Build list of input files.  Potentially useful input files
            ;; are anything from 0 to region-end (any inputs afterward are
            ;; unlikely to have any effect on the current region).
            (while (re-search-forward "\\\\input[ \t]+[^ \t\n]+" (point-max) t)
              (setq inputs (cons (buffer-substring (match-beginning 0) (match-end 0)) inputs)))
            (setq inputs (nreverse inputs)))
          (set-buffer buffer)
          (widen)
          (erase-buffer)
          (goto-char (point-min))
          (while inputs
            (insert (car inputs) "\n")
            (setq inputs (cdr inputs)))
          (insert region)
          ;; It doesn't hurt to have an extra `\end' and TeX will barf if
          ;; it's missing, so always add it. 
          (insert "\n\\end\n")
          (write-region (point-min) (point-max) viewtex-file))
      (store-match-data orig-match-data)
      (kill-buffer buffer))))

;; Return the directory where `viewtex-file' resides.
(defun viewtex-directory ()
  (let ((orig-match-data (match-data))
        (directory default-directory))
    (unwind-protect
        (and (string-match "[^/\C-j]+$" viewtex-file)
             (setq directory (substring viewtex-file 0 (match-beginning 0))))
      (store-match-data orig-match-data))
    directory))

(defun viewtex-substitute-file-extension (file &optional ext)
  (let ((orig-match-data (match-data)))
    (unwind-protect
        (progn
          (and (string-match "\\.[^/.]*$" file)
               (setq file (substring file 0 (match-beginning 0))))
          (or ext (setq ext ""))
          (or (string-match "^\\." ext) (setq ext (concat "." ext)))
          (setq file (concat file ext)))
      (store-match-data orig-match-data)))
  file)


;; tex process frobnicators.

(defun viewtex-tex-filter (proc string)
  ;; Unfortunately, tex will not exit with a nonzero exit status when you
  ;; do an emergency stop.  So to keep the tex sentinel from thinking it
  ;; should start xdvi, set a flag which indicates that tex is really
  ;; misbehaving. 
  (let ((orig-match-data (match-data)))
    (unwind-protect
        (and (or (string-match "! Emergency stop" string)
                 (string-match "No pages of output" string))
             (setq viewtex-tex-emergency-stop t))
      (store-match-data orig-match-data)))
  ;; Now actually insert the text in the right buffer. 
  (let (proc-mark region-begin window)
    (save-excursion
      (set-buffer (process-buffer proc))
      (setq proc-mark (process-mark proc)
            region-begin (point)
            ;; If process mark is at window start, insert-before-markers
            ;; will insert text off-window since it's also inserting before
            ;; the start window mark.  Make sure we can see the most recent
            ;; text.  (note: it's a buglet that this isn't necessary if
            ;; scroll-step is 0, but that works to our advantage since it
            ;; makes the filter a little faster.)
            window (and (/= 0 scroll-step)
                        (get-buffer-window (current-buffer))))
      (goto-char proc-mark)
      (insert-before-markers string))
    ;; Frob window-start outside of save-excursion so it works whether the
    ;; current buffer is the process buffer or not.
    (and window
         (>= (window-start window) region-begin)
         (set-window-start window region-begin 'noforce))))

(defun viewtex-tex-sentinel (proc string)
  (let ((proc-status (process-status proc)))
    (cond ((eq proc-status 'exit)
           (save-excursion
             (set-buffer (process-buffer proc))
             ;; Don't use (process-mark proc), because after the process
             ;; has exited, it is an error to reference the process mark.
             (goto-char (point-max))
             (if viewtex-tex-emergency-stop
                 (insert-before-markers (format "\n\nProcess %s made emergency exit.\n" proc))
               (insert-before-markers (format "\n\nProcess %s exited normally.\n" proc)))
             (delete-process proc))
           (or viewtex-tex-emergency-stop (viewtex-make-xdvi-process)))
          ((eq proc-status 'signal)
           (message "Process %s received event: %s" proc (substring string 0 -1))))))


;; xdvi process frobnicators.

(defun viewtex-make-xdvi-process ()
  (let (xdvi-buffer
        xdvi-file
        xdvi-process)
    (if viewtex-xdvi-process
        (message "(xdvi is already running)")
      (message "(starting xdvi)")
      (setq xdvi-buffer (get-buffer-create viewtex-xdvi-buffer)
            xdvi-file (viewtex-substitute-file-extension viewtex-file ".dvi"))
      (save-excursion
        (set-buffer xdvi-buffer)
        (widen)
        (erase-buffer)
        (setq mode-line-process '(": %s")
              buffer-read-only t
              major-mode 'fundamental-mode))
      (start-process "xdvi" xdvi-buffer viewtex-xdvi-command xdvi-file)
      (setq xdvi-process (get-buffer-process xdvi-buffer))
      (set-process-filter xdvi-process 'viewtex-xdvi-filter)
      (set-process-sentinel xdvi-process 'viewtex-xdvi-sentinel)
      (setq viewtex-xdvi-process xdvi-process))))

;; Like default filter, but use insert-before-markers and make sure buffer
;; is displayed if any output is sent to it. 
(defun viewtex-xdvi-filter (proc string)
    (save-excursion
      (set-buffer (process-buffer proc))
      (let ((buffer-read-only nil))
        (insert-before-markers string)))
    (display-buffer (process-buffer proc)))

(defun viewtex-xdvi-sentinel (proc string)
  (let ((proc-status (process-status proc)))
    (cond ((or (eq proc-status 'exit) (eq proc-status 'signal))
           (message "viewtex: xdvi process exited.")
           (setq viewtex-xdvi-process nil)
           ;(kill-buffer viewtex-xdvi-buffer)
           (delete-process proc)))))


;; Provide before running hooks, just in case something in hooks
;; generate an error. 
(provide 'viewtex)

;; This is obsolesced in Emacs 19 by `after-load-alist'.
(run-hooks 'viewtex-after-load-hook)

;; viewtex.el ends here.
