;;; Saved through ges-version 0.3.3dev at 2004-02-20 13:02
;;; From: gse@antisleep.com (Scott Evans)
;;; Subject: gse-locate: an emacs front end to "locate"
;;; Newsgroups: gnu.emacs.sources
;;; Date: 17 Jan 2004 23:40:29 -0800
;;; Organization: http://groups.google.com

;; gse-locate.el
;;   Interface to the 'locate' command found on many linux systems.
;;
;; Author:         Scott Evans <gse@antisleep.com>
;; CVS version:    $Revision: 1.6 $
;; Latest version: http://www.antisleep.com/elisp
;;
;; Commentary:
;;   A handy Emacs interface to 'locate', which I first encountered on
;;   Linux a few years ago.  Having this interface around is great --
;;   it gives you a one-step way to get around a filesystem, and spend
;;   a lot less time navigating directory trees.
;;
;;   I lifted a lot of the major mode code from Steve Molitor's
;;   rec-files.el (or at least from my modified version).
;;
;;   I spend most of my time in Windows now, and porting slocate to
;;   Cygwin didn't go real well so I wrote my own simple 'locate'
;;   replacement for Cygwin (see  http://www.antisleep.com/software/loc).
;;
;;   Written for/using XEmacs on Windows.  Changes are welcome.
;;
;; Installation:
;;   (require 'gse-locate)
;; and if you like:
;;   (global-set-key '[f10] 'gse-locate)
;;
;;---------------------------------------------------------------------------
;; Change Log
;; ----------
;; 2004.01.18 Use loc -c, take out backslash replacing.
;; 2004.01.18 Add gse-locate-execute-file.
;; 2004.01.18 Change to major mode with keybindings.  Add some customization.
;; 2002.04.08 Created.
;;---------------------------------------------------------------------------


;; Unix (and native Cygwin) users can probably just set this
;; to "locate".
(defvar gse-locate-command "bash /c/gse/bin/shell/loc -c"
  "*\"locate\" shell command.  This will be used as an argument to
shell-command, with the search pattern concatenated to it.

This specified program should take a string (or regular expression, if
you intend to use them) as a parameter, and return a list of files to
stdout, one per line.")

(defvar gse-locate-hooks nil
  "List of functions to call when entering gse-locate-mode")

(defvar gse-locate-regexp-list
  (list "\\.elc"
        "\\.class"
        "#"
        )
  "A list of regular expressions that match \"uninteresting\" filenames.
These will be stripped from the locate list before it is displayed.")

;;---------------------------------------------------------------------------

(defvar gse-locate-prev-wconfig nil)
(defvar gse-locate-buf nil)
(defvar gse-locate-history nil)

;;---------------------------------------------------------------------------

(defun gse-locate (pattern)
  "Lightweight interface to locate.  PATTERN is the string or regexp
that will be passed to locate (see gse-locate-command).

If exactly one file matches, it will be opened.  Otherwise a list
of files will be presented (see gse-locate-mode)."
  (interactive
   (list
    (read-from-minibuffer "locate pattern: " nil nil nil
                          'gse-locate-history)))

  (set-buffer (get-buffer-create "*locate*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (shell-command (concat gse-locate-command " " pattern) t)

  ;; Clean up stuff we don't want to see.
  (let* ((i 0))
    (while (< i (length gse-locate-regexp-list))
      (let ((cur-regexp (nth i gse-locate-regexp-list)))
        (goto-char (point-min))
        (delete-matching-lines cur-regexp))
      (setq i (+ i 1))))

  (let ((number-matches (count-lines (point-min) (point-max))))
    (cond
     ((= number-matches 0)
      (message "No matches."))
     ((= number-matches 1)
      ;; One match.  Open the file.
      (find-file (buffer-substring
                  (point-min)
                  (progn
                    (goto-char (point-min))
                    (end-of-line)
                    (point))))
      )
     (t
      ;; Multiple matches.
      (setq gse-locate-prev-wconfig (current-window-configuration))
      (goto-char (point-min))
      (switch-to-buffer (current-buffer))
      (setq gse-locate-buf (current-buffer))
      (gse-locate-mode))
    ))
  )

;;---------------------------------------------------------------------------

(defvar gse-locate-mode-map nil
  "Keymap for gse-locate-mode.")

(if gse-locate-mode-map
    ()
  (setq gse-locate-mode-map (make-sparse-keymap))
  (define-key gse-locate-mode-map "v"    'gse-locate-select-this-window)
  (define-key gse-locate-mode-map "\C-m" 'gse-locate-select-this-window)
  (define-key gse-locate-mode-map "o"    'gse-locate-select-other-window)
  (define-key gse-locate-mode-map " "    'next-line)
  (define-key gse-locate-mode-map "n"    'next-line)
  (define-key gse-locate-mode-map "p"    'previous-line)
  (define-key gse-locate-mode-map "q"    'gse-locate-quit)
  (define-key gse-locate-mode-map "1"    'gse-locate-select-1-window)
  (define-key gse-locate-mode-map "?"    'describe-mode)

  (when (functionp 'mswindows-shell-execute)
    (define-key gse-locate-mode-map "X"  'gse-locate-execute-file))
  )

;;---------------------------------------------------------------------------

(defun gse-locate-mode ()
  "Lightweight major mode to select a file from \"locate\" output.

Special keys:
\\{gse-locate-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gse-locate-mode)
  (setq mode-name "Locate")
  (use-local-map gse-locate-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'gse-locate-hooks))

;;---------------------------------------------------------------------------

(defun gse-locate-current-file ()
  (save-excursion
    (beginning-of-line)
    (buffer-substring
     (point)
     (search-forward-regexp "$"))))

;;---------------------------------------------------------------------------

(defun gse-locate-find-file (file-name &optional find-function)
  "Open file if it exists."
  (when (not find-function)
    (setq find-function 'find-file))

  (if (file-exists-p file-name)
        (funcall find-function file-name)
    (error "%s%s%s" "File '" file-name "' does not exist!")))

;;---------------------------------------------------------------------------

(defun gse-locate-select-this-window ()
  "Select this line's file in this window."
  (interactive)
  (gse-locate-find-file (gse-locate-current-file)))

;;---------------------------------------------------------------------------

(defun gse-locate-select-other-window ()
  "Select this line's file in this window."
  (interactive)
  (gse-locate-find-file (gse-locate-current-file) 'find-file-other-window))

;;---------------------------------------------------------------------------

(defun gse-locate-select-1-window ()
  "Select this line's buffer, alone, in full frame."
  (interactive)
  (gse-locate-find-file (gse-locate-current-file))
  (bury-buffer (other-buffer))
  (delete-other-windows))

;;---------------------------------------------------------------------------

(defun gse-locate-quit ()
  "Close gse-locate buffer."
  (interactive)
  (let ((buf (get-buffer gse-locate-buf)))
    (and buf (bury-buffer buf)))
  (set-window-configuration gse-locate-prev-wconfig))

;;---------------------------------------------------------------------------

(defun gse-locate-execute-file ()
  "Launch this line's file.  Currently windows-only (and probably
XEmacs-only)."
  (interactive)

  (let ((file-name (gse-locate-current-file)))
    (if (file-exists-p file-name)
        (mswindows-shell-execute nil file-name)
      (error "%s%s%s" "File '" file-name "' does not exist!"))))

;;---------------------------------------------------------------------------

(provide 'gse-locate)

