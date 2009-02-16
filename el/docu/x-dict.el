;;; x-dict.el --- emacs interface for several online dictionaries

;; Copyright (C) 2005-2006 by Stefan Reichoer

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; x-dict.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; x-dict.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; x-dict.el provides an Emacs interface for the following dictionaries:
;;  URL            language
;;  dict.leo.org : english <-> german
;;  www.dict.cc  : english <-> german

;; x-dict.el needs my python script 'x-dict' to interact with the web interface
;; x-dict can be found here: http://www.xsteve.at/prg/python

;; The latest version of x-dict.el can be found at:
;; http://www.xsteve.at/prg/emacs/

;; Here is a nice tip from Harald Maier to select a specific coding
;; system for the x-dict interaction:
;; (add-to-list 'process-coding-system-alist '("x-dict" . latin-9))

;; Comments and/or patches are very welcome!

;;; History:
;;

;;; Code:

(defconst  xdict-dictionaries '(leo dict-cc))

(defvar xdict-use-pymacs nil)
(defvar xdict-program-name "x-dict") ;; On windows: python c:/utils/python/x-dict
(defvar xdict-column-width 60)
(defvar xdict-seperator-string "------------------------------------------------------------------")

(defvar xdict-dictionary 'leo "Actual dictionary used for queries. One of `xdict-dictionaries'")

(defvar xdict-buffer-name "*LEO dictionary*")

;;; internal variables
(defvar xdict-previous-window-configuration nil)

(defvar xdict-goto-end-of-buffer-after-query nil
"If non-nil, go to the end of the buffer after processing. If nil, go
to the beginning of the queried x-dict entry.")

(defvar xdict-mode-map () "Keymap used in the `xdict-mode' buffer.")
(when (not xdict-mode-map)
  (setq xdict-mode-map (make-sparse-keymap))
  (suppress-keymap xdict-mode-map)
  (define-key xdict-mode-map [remap undo] 'xdict-undo)
  (define-key xdict-mode-map "X" 'xdict-erase-buffer)
  (define-key xdict-mode-map "x" 'xdict-delete-entry)
  (define-key xdict-mode-map "p" 'xdict-previous-entry)
  (define-key xdict-mode-map "n" 'xdict-next-entry)
  (define-key xdict-mode-map "." 'xdict-query-with-word-at-point)
  (define-key xdict-mode-map "l" 'xdict-query)
  (define-key xdict-mode-map "w" 'xdict-copy-translation)
  (define-key xdict-mode-map (kbd "TAB") 'xdict-toggle-column)
  (define-key xdict-mode-map "q" 'xdict-quit))

(easy-menu-define xdict-mode-menu xdict-mode-map
"`xdict-mode' menu"
                  '("XDict"
                    ["Query" xdict-query t]
                    ["Query word at point" xdict-query-with-word-at-point t]
                    ["Toggle column" xdict-toggle-column t]
                    ["Copy translation" xdict-copy-translation t]
                    ["Delete entry" xdict-delete-entry t]
                    ["Erase buffer" xdict-erase-buffer t]
                    ))

(defun xdict-mode ()
  "Major mode to display translation results for various online dictionaries."
  (interactive)
  (kill-all-local-variables)
  (use-local-map xdict-mode-map)
  (setq major-mode 'xdict-mode)
  (setq mode-name "x-dict")
  (toggle-read-only 1))

(defun xdict ()
  "Display the dictionary buffer."
  (interactive)
  (setq xdict-previous-window-configuration (current-window-configuration))
  (pop-to-buffer xdict-buffer-name)
  (xdict-mode))

(defun xdict-select-dictionary ()
  "Select the dictionary for the next query."
  (interactive)
  (setq xdict-dictionary (intern (completing-read
                                  "Dictionary: "
                                  (map t 'symbol-name xdict-dictionaries) nil t
                                  (symbol-name xdict-dictionary)))))


(defun xdict-run-query (word)
  "Queries a dictionary for WORD, return the result as string."
  (if xdict-use-pymacs
      (xdict-py-search word)
    (with-temp-buffer
      (call-process
       xdict-program-name
       nil t t
       "--column-width" (number-to-string xdict-column-width)
       (cadr (assoc xdict-dictionary '((leo "--leo") (dict-cc "--dict_cc"))))
       word)
      (buffer-substring (point-min) (point-max)))))

(defun xdict-ressource-name()
  "Return the url that is used to look up the next query."
  (cadr (assoc xdict-dictionary '((leo "dict.leo.org") (dict-cc "www.dict.cc")))))

(defun xdict-query (word)
  "Query dict.leo.org for WORD.
This calls my python script x-dict (it can be found at: http://www.xsteve.at/prg/python)"
  (interactive (list (unless (eq current-prefix-arg 0)
                       (read-string (concat "Lookup word at " (xdict-ressource-name) ": ")
                                    (substring-no-properties (or (thing-at-point 'word) ""))))))
  (cond ((eq word nil)
         (xdict))
        ((> (length word) 0)
         (xdict)
         (goto-char (point-max))
         (let ((buffer-read-only nil))
           (newline)
           (insert xdict-seperator-string)
           (newline)
           (insert (concat "Lookup <" (xdict-ressource-name) "> for '" word "':"))
           (newline)
           (insert (xdict-run-query word))
           (if xdict-goto-end-of-buffer-after-query
               (end-of-buffer)
             (re-search-backward
              (concat "^" xdict-seperator-string) nil t)
             (forward-line 2)
             (recenter 1))))))

(defun xdict-query-with-word-at-point ()
  "Run `xdict-query' for the word at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (message (concat "Querying " (xdict-ressource-name) " for '" word "'"))
    (xdict-query word)))

(defun xdict-quit ()
  "Close the dictionary buffer and restore the window configuration."
  (interactive)
  (bury-buffer)
  (when xdict-previous-window-configuration
    (set-window-configuration xdict-previous-window-configuration)
    (setq xdict-previous-window-configuration nil)))

(defun xdict-previous-entry ()
  "Move point to the previous dictionary lookup."
  (interactive)
  (forward-line -1)
  (search-backward xdict-seperator-string nil t)
  (forward-line 1)
  (beginning-of-line))

(defun xdict-next-entry ()
  "Move point to the next dictionary lookup."
  (interactive)
  (when (search-forward xdict-seperator-string nil t)
    (forward-line 1)
    (beginning-of-line)))

(defun xdict-delete-entry ()
  "Delete the displayed entry at point."
  (interactive)
  (let ((buffer-read-only nil)
        (start (progn (forward-paragraph) (point)))
        (end (progn (backward-paragraph) (point))))
    (delete-region start end)))

(defun xdict-erase-buffer ()
  "Erase the whole x-dict buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)))

(defun xdict-undo ()
  "Undo the last edit operation in the dictionary buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (undo)))

(defun xdict-toggle-column ()
  "Switch between the two language columns."
  (interactive)
  (let ((column (+
                 (line-beginning-position)
                 (if (< (current-column) xdict-column-width)
                     xdict-column-width
                   0))))
    (when (< column (line-end-position))
      (goto-char column))))

(defun xdict-copy-translation ()
  "Copy the translation at point to the kill ring"
  (interactive)
  (let* ((col1 (< (current-column) xdict-column-width))
         (start-pos (if col1 (line-beginning-position) (+ (line-beginning-position) xdict-column-width)))
         (end-pos (if col1 (+ (line-beginning-position) xdict-column-width) (line-end-position)))
         (line (buffer-substring-no-properties start-pos end-pos)))
    (string-match "\\(.+?\\)[ ]*$" line)
    (kill-new (match-string 1 line))
    (message "Copied '%s'" (match-string 1 line))))

(provide 'x-dict)

;;; arch-tag: daca3e4a-f131-45d2-9946-1f9c6a88fa97
;;; x-dict.el ends here


