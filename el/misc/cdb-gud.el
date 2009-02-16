;;; cdb-gud.el --- Grand Unified Debugger mode for running CDB

;; Author: Stephan Doll <stephan_doll at dantz.com>
;; Version: 1.0 (January 30, 2002)

;; This file is NOT part of GNU Emacs but the same permissions apply.
;; This is free software (needed for emacswiki upload.pl)
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides Emacs (GUD) integration for Microsoft's CDB
;; debugger.  (CDB is the text-mode version of WinDbg).  For more
;; details about the Emacs' debugger integration, read "Running
;; Debuggers Under Emacs" in the Emacs manual.
;;
;; To install this package:
;;
;;    - Download and install the latest version of the `Debugging Tools
;;      for Windows' from http://www.microsoft.com/ddk/debugging/.  Add it
;;      to your PATH environment.
;;
;;    - Put the following into your .emacs file:
;;
;;      (load "cdb-gud")
;;
;;    - You can customize `gud-cdb-directories' to help GUD find your source
;;      files.
;;
;;    - If you want key bindings similar to MS' GUI debuggers, add the
;;      following:
;;
;;      (global-set-key [f5]    'gud-cont)
;;      (global-set-key [f7]    'gud-tbreak)
;;      (global-set-key [f8]    'gud-step)
;;      (global-set-key [f9]    'gud-break)
;;      (global-set-key [f10]   'gud-next)
;;      (global-set-key [f11]   'gud-finish)
;;

;;; Here is a simple tutorial:

;; In Emacs, run

;;    	M-x cdb
;;     "Run cdb (like this):" cdb <name of your exe>

;; This will open a new Emacs buffer "*gud-xxx*".  In it you will get a
;; CDB command prompt '0:000> '.  (CDB commands are documented in the
;; 'Debugging tools for Windows' online help).  To get to the begin of
;; your code, type:

;;      'g main' <Enter> (or 'g WinMain' if you have a GUI application).

;; CDB will load the application and break at your main() function.
;; Emacs should open another window with your main() source file and show
;; a little '>' were the debugger stopped.  You now can set more
;; breakpoints in your sources, single-step, etc.  To use the common VC++

;; You can also issue additional commands from the CDB command prompt --
;; e.g.:

;;     - 'dv'  Displays local variables
;;     - 'dt' or '??' shows the content of a single variable.

;; To get the current stack trace, either use the 'k' command or execute
;; "M-x speedbar".  The later will display the calling stack in a
;; additional Emacs frame and you can use the mouse to switch between
;; stack frames.

;; If the little GUD source line marker '>' is hard to follow, add the
;; following to your .emacs:

;; ;;-------------------------------------------------------------
;; ;; Add color to the current GUD line
;; ;;
;; (defvar gud-overlay
;;   (let* ((ov (make-overlay (point-min) (point-min))))
;;     (overlay-put ov 'face 'secondary-selection)
;;     ov)
;;   "Overlay variable for GUD highlighting.")

;; (defadvice gud-display-line (after my-gud-highlight act)
;;   "Highlight current line."
;;   (let* ((ov gud-overlay)
;;          (bf (gud-find-file true-file)))
;;     (save-excursion
;;       (set-buffer bf)
;;       (move-overlay ov (line-beginning-position) (line-end-position) (current-buffer)))))

;; (defun gud-kill-buffer ()
;;   (if (eq major-mode 'gud-mode)
;;        (delete-overlay gud-overlay)))

;; (add-hook 'kill-buffer-hook 'gud-kill-buffer)
;; ;;-------------------------------------------------------------

;; Have fun,
;; -Stephan
    
;;; Code:

(require 'gud)

;;; History of argument lists passed to cdb.
(defvar gud-cdb-history nil)

(defcustom gud-cdb-directories nil
  "*A list of directories that cdb should search for source code.
If nil, only source files in the program directory
will be known to cdb.

The file names should be absolute, or relative to the directory
containing the executable being debugged."
  :type '(choice (const :tag "Current Directory" nil)
                 (repeat :value ("")
                         directory))
  :group 'gud)

(defun gud-cdb-massage-args (file args)
  (cons "-c" (cons "l+*;l-s" (cons "-lines" args))))

(defun gud-cdb-file-name (f)
  "Transform a relative file name to an absolute file name, for cdb."
  (let ((result nil))
    (if (file-exists-p f)
        (setq result (expand-file-name f))
      (let ((directories gud-cdb-directories))
        (while directories
          (let ((path (concat (car directories) "/" f)))
            (if (file-exists-p path)
                (setq result (expand-file-name path)
                      directories nil)))
          (setq directories (cdr directories)))))
    result))

(defvar gud-marker-acc "")
(make-variable-buffer-local 'gud-marker-acc)

(defun gud-cdb-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output ""))

;; Process all the complete markers in this chunk.  This regex might catch
    ;; too munch, but that is the debugger's fault ...
    (while (string-match "^\\([-A-Za-z_\.:\\]*\\)(\\([0-9]*\\))\n" gud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-marker-acc (match-beginning 1) (match-end 1))
             (string-to-int (substring gud-marker-acc
                                       (match-beginning 2)
                                       (match-end 2))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
                      (substring gud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" gud-marker-acc)
        (progn
        ;; Everything before the potential marker start can be output.
          (setq output (concat output (substring gud-marker-acc
                                                 0 (match-beginning 0))))

          ;; Everything after, we save, to combine with later input.
          (setq gud-marker-acc
                (substring gud-marker-acc (match-beginning 0))))
      (setq output (concat output gud-marker-acc)
            gud-marker-acc ""))
    output))

(defun gud-cdb-find-file (f)
  (save-excursion
    (let ((realf (gud-cdb-file-name f)))
      (if realf
          (find-file-noselect realf)
        (find-file-noselect f 'nowarn)
        ))))

(defun cdb-simple-send (proc string)
  (if (string-match "^[ \t]*[Qq][ \t]*" string)
      (kill-buffer gud-comint-buffer))
  (comint-send-string proc (concat string " \n")))

(defun cdb (command-line)
  "Run cdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'cdb)))

  (gud-common-init command-line 'gud-cdb-massage-args
                   'gud-cdb-marker-filter 'gud-cdb-find-file)
  (set (make-local-variable 'gud-minor-mode) 'cdb)

  (gud-def gud-break  "bp `%d%f:%l` " "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "g `%d%f:%l` "  "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-step   "t "            "\C-s" "Step one source line with display.")
  (gud-def gud-next   "p "            "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "g "            "\C-r" "Continue with display.")
  (gud-def gud-finish "g @$ra "       "\C-f" "Finish executing current function.")
  (gud-def gud-print  "?? %e "        "\C-p" "Evaluate C expression at point.")

  (setq comint-prompt-regexp "^[0-9a-f]:[0-9a-f][0-9a-f][0-9a-f]> ")
  (setq comint-input-sender 'cdb-simple-send)
  (setq paragraph-start comint-prompt-regexp)

  (run-hooks 'cdb-mode-hook))

;; cdb speedbar functions

(defun gud-cdb-goto-stackframe (text token indent)
  "Goto the stackframe described by TEXT, TOKEN, and INDENT."
  (speedbar-with-attached-buffer
   (gud-display-line (nth 2 token) (string-to-int (nth 3 token)))
   (gud-basic-call (concat ".frame " (nth 1 token)))))

(defvar gud-cdb-complete-in-progress)

(defvar gud-cdb-fetched-stack-frame nil
  "Stack frames we are fetching from CDB.")

(defvar gud-cdb-fetched-stack-frame-list nil
  "List of stack frames we are fetching from CDB.")

(defun gud-cdb-get-stackframe (buffer)
  "Extract the current stack frame out of the GUD CDB BUFFER."
  (let ((newlst nil)
        (gud-cdb-fetched-stack-frame-list nil))
    (gud-cdb-run-command-fetch-lines "kn " buffer)
    (if (and (car gud-cdb-fetched-stack-frame-list)
             (string-match "No stack" (car gud-cdb-fetched-stack-frame-list)))
        ;; Go into some other mode???
        nil
      (while gud-cdb-fetched-stack-frame-list
        (let ((e (car gud-cdb-fetched-stack-frame-list))
              (name nil) (num nil))
          (if (not (string-match "^\\([0-9a-f]+\\) [0-9a-f]* [0-9a-f]* \\([[a-zA-Z_0-9:$~!+]*\\).*$" e))
              nil
            (setq num (match-string 1 e)
                  name (match-string 2 e))
            (setq newlst
                  (cons
                   (if (string-match
                        "\\([-0-9a-zA-Z\\_.:]+\\) @ \\([0-9]+\\)" e)
                       (list name num (match-string 1 e)
                             (match-string 2 e))
                     (list name num))
                   newlst))))
        (setq gud-cdb-fetched-stack-frame-list
              (cdr gud-cdb-fetched-stack-frame-list)))
      (nreverse newlst))))

(defun gud-cdb-run-command-fetch-lines (command buffer)
  "Run COMMAND, and return when `gud-cdb-fetched-stack-frame-list' is full.
BUFFER is the GUD buffer in which to run the command."
  (save-excursion
    (set-buffer buffer)
    (if (save-excursion
          (goto-char (point-max))
          (forward-line 0)
          (not (looking-at comint-prompt-regexp)))
        nil
  ;; Much of this copied from CDB complete, but I'm grabbing the stack
      ;; frame instead.
      (let ((gud-marker-filter 'gud-cdb-speedbar-stack-filter))
        ;; Issue the command to CDB.
        (gud-basic-call command)
        (setq gud-cdb-complete-in-progress t)
        ;; Slurp the output.
        (while gud-cdb-complete-in-progress
          (accept-process-output (get-buffer-process gud-comint-buffer) 15))
        (setq gud-cdb-fetched-stack-frame nil
              gud-cdb-fetched-stack-frame-list
              (nreverse gud-cdb-fetched-stack-frame-list))))))

(defun gud-cdb-speedbar-stack-filter (string)
  ;; checkdoc-params: (string)
  "Filter used to read in the current CDB stack."
  (setq string (concat gud-cdb-fetched-stack-frame string))
  (while (string-match "\n" string)
    (setq gud-cdb-fetched-stack-frame-list
          (cons (substring string 0 (match-beginning 0))
                gud-cdb-fetched-stack-frame-list))
    (setq string (substring string (match-end 0))))
  (if (string-match comint-prompt-regexp string)
      (progn
        (setq gud-cdb-complete-in-progress nil)
        string)))

(defun gud-speedbar-buttons (buffer)
  "Create a speedbar display based on the current state of GUD.
If the GUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode."
  (if (and (save-excursion (goto-char (point-min))
                           (looking-at "XXX")) ; *SD* Always update ...
           (equal gud-last-last-frame gud-last-speedbar-stackframe))
      nil
    (setq gud-last-speedbar-buffer buffer)
    (let* ((ff (save-excursion (set-buffer buffer) gud-find-file))
       ;;(lf (save-excursion (set-buffer buffer) gud-last-last-frame))
           (frames
            (cond ((eq ff 'gud-gdb-find-file)
                   (gud-gdb-get-stackframe buffer)
                   )
                  ;; *SD* ++
                  ((eq ff 'gud-cdb-find-file)
                   (gud-cdb-get-stackframe buffer)
                   )
                  ;; *SD* --
                  ;; Add more debuggers here!
                  (t
                   (speedbar-remove-localized-speedbar-support buffer)
                   nil))))
      (erase-buffer)
      (if (not frames)
          (insert "No Stack frames\n")
        (insert "Current Stack:\n"))
      (while frames
        (insert (nth 1 (car frames)) ":\n")
        (if (= (length (car frames)) 2)
            (progn
              (speedbar-insert-button (car (car frames))
                                      'speedbar-directory-face
                                      nil nil nil t))
          (speedbar-insert-button (car (car frames))
                                  'speedbar-file-face
                                  'speedbar-highlight-face
                                  (cond ((eq ff 'gud-gdb-find-file)
                                         'gud-gdb-goto-stackframe)
                                        ((eq ff 'gud-cdb-find-file)
                                         'gud-cdb-goto-stackframe)
                                        (t (error "Should never be here")))
                                  (car frames) t))
        (setq frames (cdr frames)))
      )
    (setq gud-last-speedbar-stackframe gud-last-last-frame)))

;;; cdb-gud.el ends here
