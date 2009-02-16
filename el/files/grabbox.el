;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; grabbox.el --- Keep a bookmark file for often used files/text snippets

;; Copyright (C) 2000 - 2001 by Stefan Reichoer

;; Emacs Lisp Archive Entry
;; Filename: grabbox.el
;; Author: Stefan Reichoer, <reichoer@web.de>
;; Version: 1.2

;; $Id: grabbox.el,v 1.5 2002/03/28 14:50:41 reichr Exp $

;; grabbox.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; grabbox.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; Keep a bookmark file for often used files and text snippets
;; The bookmark file is divided into sections. The section name is put
;; in square brackets (e.g. [Emacs stuff])
;; The command grabbox-section-menu enables you to navigate through
;; the sections via completion
;; An entry in the grabbox-file can have one of the following formats
;; F <name> file-path
;;   open the file file-path via the function find-file
;;
;; T <name> textsnippet
;;   insert the textsnippet in the buffer that was active before grabbox was
;;   invoked
;;
;; P <name> file-name-of-ede-project-file
;;   load an ede project file
;;
;; L <name> file-path
;;   On windows: launch file-path (open the file with the default application)
;;

;; ;Example .grabbox file
;; [Emacs Lisp]
;; F <.emacs> ~/.emacs
;; F <Grabbox>  c:/emacs-20.7/site-lisp/grabbox.el
;;
;; [Text snippets]
;; T <reichoer> reichoer@web.de
;;
;; [Documentation]
;; L <paralist> c:/localtexmf/tex/latex/paralist/paralist.dvi
;;
;; ;An EDE project
;; P <LEWIS> e:/projects/LewisProgrammer/Project.ede

;; The latest version of grabbox.el can be found at:
;;   http://xsteve.nit.at/prg/emacs/grabbox.el

;; Comments / suggestions welcome!

;;; Code:

(defvar grabbox-file "~/.grabbox" "The grab box")

;;; End of user settings

(require 'cl)

(defvar grabbox-mode-syntax-table nil
  "Syntax table in use in grabbox-mode buffers.")

(if grabbox-mode-syntax-table
    ()
  (setq grabbox-mode-syntax-table (make-syntax-table))

  ; ";" starts a comment
  ;(modify-syntax-entry ?\; "<" grabbox-mode-syntax-table)
  (modify-syntax-entry ?\; ". 12" grabbox-mode-syntax-table)
  ;; and \n and \^M end a comment
  (modify-syntax-entry ?\n ">"    grabbox-mode-syntax-table)
  (modify-syntax-entry ?\^M ">"   grabbox-mode-syntax-table)

  (modify-syntax-entry ?\" "."   grabbox-mode-syntax-table)

  (modify-syntax-entry ?_ "w"    grabbox-mode-syntax-table))


(defvar grabbox-font-lock-keywords nil "Expressions to highlight in grabbox mode.")

(setq grabbox-font-lock-keywords
  (list
   (cons (concat "^;\.*")
	 'font-lock-comment-face)
   (cons (concat "\\sw+: ")
	 'font-lock-keyword-face)
   (cons "^[ \t]*\\[\.+\\]" 'font-lock-function-name-face) ;font-lock-constant-face)
   (cons "<\.+>" 'font-lock-constant-face)))

(defvar grabbox-mode-map () "Keymap used in GrabBox buffers.")

(cond ((not grabbox-mode-map)
       (setq grabbox-mode-map (make-sparse-keymap))
       (define-key grabbox-mode-map "m" 'grabbox-section-menu)
       (define-key grabbox-mode-map "g" 'grabbox-goto)
       (define-key grabbox-mode-map "i" 'grabbox-insert-last-buffer-file-name)
       (define-key grabbox-mode-map "p" 'grabbox-paste)
       (define-key grabbox-mode-map ";" 'grabbox-comment-uncomment-line)
       (define-key grabbox-mode-map "s" 'save-buffer)
       (define-key grabbox-mode-map "q" 'grabbox-bury-buffer)
       (define-key grabbox-mode-map [return] 'grabbox-examine-line)
       (define-key grabbox-mode-map [mouse-2] 'grabbox-examine-line)))

(defvar grabbox-edit-mode-map () "Keymap used in GrabBox-Edit buffers.")

(cond ((not grabbox-edit-mode-map)
       (setq grabbox-edit-mode-map (make-sparse-keymap))))


;; Menu
(easy-menu-define grabbox-mode-menu grabbox-mode-map
"'grabbox-mode' menu"
                  '("Grabbox"
                    ["Insert Shortcut to last visited buffer" grabbox-insert-last-buffer-file-name t]
                    ;["grabbox-section-menu" grabbox-section-menu t]
                    ["Edit this buffer" grabbox-mode t]
                    ["Comment/Uncomment current line" grabbox-comment-uncomment-line t]
                    ["Execute Shortcut on current line" grabbox-examine-line t]
                    ;"---"
                    ))

(easy-menu-define grabbox-edit-mode-menu grabbox-edit-mode-map
"'grabbox-mode' menu"
                  '("Grabbox-Edit"
                    ["Insert Shortcut to last visited buffer" grabbox-insert-last-buffer-file-name t]
                    ;["grabbox-section-menu" grabbox-section-menu t]
                    ["Unedit this buffer" grabbox-mode t]
                    ["Comment/Uncomment current line" grabbox-comment-uncomment-line t]
                    ["Execute Shortcut on current line" grabbox-examine-line t]
                    ;"---"
                    ))

(setq grabbox-keep-other-windows t)
(defun grabbox-mode ()
  "Major mode for editing grabbox files. Upon startup grabbox-mode-hook is run.
grabbox-mode toggles between using and editing the grabbox-file.
The modeline shows the actual status (either GrabBox-Edit or GrabBox).

;Example .grabbox file
\[Emacs Lisp]
F <.emacs> ~/.emacs
F <Grabbox>  c:/emacs-20.7/site-lisp/grabbox.el

\[documentation]
L <paralist> c:/localtexmf/tex/latex/paralist/paralist.dvi

\[Text snippets]
T <reichoer> reichoer@web.de

;An EDE project
P <LEWIS> e:/projects/LewisProgrammer/Project.ede

The following commands are available in the GrabBox mode:
'\\[grabbox-mode]' - switch to GrabBox-Edit mode
'\\[grabbox-examine-line]' - Examine the actual line and execute the appropriate function
'\\[grabbox-section-menu]' - Jump to a section with completion
'\\[grabbox-goto]' - Jump to an entry with completion
'\\[grabbox-insert-last-buffer-file-name]' - Insert a link to the file from which grabbox was invoked
'\\[grabbox-bury-buffer]' - Bury the grabbox buffer
'\\[grabbox-paste]' - Select a text snippet and paste it at the position from which grabbox was invoked
'\\[grabbox-comment-uncomment-line]' - Comment or uncomment a line

The following command is available in the GrabBox-Edit mode:
'\\[grabbox-mode]' - Save the grabbox-file and switch to GrabBox mode

"
  (interactive)
  (let ((buffer-name (file-name-nondirectory grabbox-file))
        (was-in-grabbox (memq major-mode (list 'grabbox-mode 'grabbox-edit-mode)))
        (mode))
    (if (get-buffer-window buffer-name)
        (select-window (get-buffer-window buffer-name))
      (when (or (not grabbox-keep-other-windows)
                (< (window-height) (* 2 window-min-height)))
        (delete-other-windows))
      (setq grabbox-last-window-height (window-height))  ; remember
      (split-window)
      (find-file grabbox-file))
    (setq mode major-mode)
    (kill-all-local-variables)
    (cond
     ((and (eq mode 'grabbox-mode) was-in-grabbox)
      (toggle-read-only nil)
      (use-local-map grabbox-edit-mode-map)
      (setq major-mode 'grabbox-edit-mode)
      (setq mode-name "GrabBox-Edit"))
     (t
      (toggle-read-only t)
      (when (buffer-modified-p) (save-buffer))
      (use-local-map grabbox-mode-map)
      (setq major-mode 'grabbox-mode)
      (setq mode-name "GrabBox")))
    (set-syntax-table grabbox-mode-syntax-table)

    (setq imenu-create-index-function 'grabbox-create-index-function)

    (set (make-local-variable 'comment-start) ";")
    (set (make-local-variable 'comment-end) "")
    (set (make-local-variable 'comment-multi-line) nil)

     ;; Font lock support
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(grabbox-font-lock-keywords nil t))
    (run-hooks 'grabbox-mode-hook)))

(defun grabbox-getline-info ()
  (let ((nice-name)
        (file-name)
        (txt))
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (cond
         ((looking-at "T \\(<\\(.+\\)>[ \t]*\\)?\\(.+\\)")
          (setq nice-name (match-string-no-properties 2))
          (setq txt (format "%s" (match-string 3)))
          (list 'paste-txt txt nice-name))
         ((looking-at "F \\(<\\(.+\\)>[ \t]*\\)?\\(.+\\)")
          (setq nice-name (match-string-no-properties 2))
          (setq file-name (format "%s" (match-string 3)))
          (list 'find-file file-name nice-name))
         ((looking-at "L \\(<\\(.+\\)>[ \t]*\\)?\\(.+\\)")
          (setq nice-name (match-string-no-properties 2))
          (setq file-name (format "%s" (match-string 3)))
          (list 'launch-file file-name nice-name))
         ((looking-at "I \\(<\\(.+\\)>[ \t]*\\)?\\(.+\\)")
          (setq nice-name (match-string-no-properties 2))
          (setq file-name (format "%s" (match-string 3)))
          (list 'info-goto file-name nice-name))
         ((and (looking-at "P \\(<\\(.+\\)>[ \t]*\\)?\\(.+\\)") (fboundp 'ede-load-project-file))
          (setq nice-name (format "%s" (match-string 2)))
          (setq file-name (format "%s" (match-string 3)))
          (list 'load-project file-name nice-name)))))))


(defun grabbox-examine-line ()
  (interactive)
  (let* ((info (grabbox-getline-info))
         (cmd (nth 0 info)))
    (cond
     ((eq cmd 'paste-txt)
      (or (one-window-p) (delete-window))
      (insert (nth 1 info)))
     ((eq cmd 'find-file)
      (grabbox-bury-buffer)
      (find-file (nth 1 info)))
     ((eq cmd 'launch-file)
      (grabbox-bury-buffer)
      (if (fboundp 'w32-shell-execute)
          (w32-shell-execute "open" (nth 1 info))
        (shell-command (concat "\"" (nth 1 info) "\" &"))))
     ((eq cmd 'info-goto)
      (grabbox-bury-buffer)
      (Info-goto-node (nth 1 info)))
     ((eq cmd 'load-project)
      (when (fboundp 'ede-load-project-file)
        (message "Load project file %s" (nth 1 info))
        (ede-load-project-file (nth 1 info))
        (grabbox-bury-buffer)
        (speedbar-get-focus)
        (speedbar-refresh))))))

(defun grabbox-goto ()
  "Select an entry from the grabbox-file with completion"
  (interactive)
  (let ((running t)
        (info)
        (completions)
        (line-cache (make-hash-table))
        (node))
    (save-excursion
      (goto-char (point-max))
      (while running
        (setq info (grabbox-getline-info))
        (when (and info (memq (nth 0 info) (list 'find-file 'launch-file)) (nth 2 info))
          (add-to-list 'completions (nth 2 info))
          (cl-puthash (intern (nth 2 info))
                      (+ 1 (count-lines (point-min) (point)))
                      line-cache))
        (setq running (eq 0 (forward-line -1)))))
    (setq node (completing-read "Goto: " (mapcar 'list completions)))
    (goto-line (gethash (intern node) line-cache))
      ;(message "%S" (grabbox-getline-info))
    (grabbox-examine-line)))

(defun grabbox-re-enlarge ()
  ;; Enlarge window to a remembered size
  (enlarge-window
   (max 0 (- (or grabbox-last-window-height (window-height))
	     (window-height)))))

(defun grabbox-bury-buffer ()
  "Bury the grabbox buffer and delete the corresponding window."
  (interactive)
  (bury-buffer)
  (or (one-window-p) (delete-window))
  (grabbox-re-enlarge))

(defun grabbox-paste ()
  "Select a text snip and paste it at point in the buffer from which you started grabbox"
  (interactive)
  (let ((running t)
        (info)
        (completions)
        (line-cache (make-hash-table))
        (node))
    (save-excursion
      (goto-char (point-max))
      (while running
        (setq info (grabbox-getline-info))
        (when (and info (eq (nth 0 info) 'paste-txt) (nth 2 info))
          (add-to-list 'completions (nth 2 info))
          (cl-puthash (intern (nth 2 info))
                      (+ 1 (count-lines (point-min) (point)))
                      line-cache))
        (setq running (eq 0 (forward-line -1)))))
    (setq node (completing-read "Paste: " (mapcar 'list completions)))
    (goto-line (gethash (intern node) line-cache))
      ;(message "%S" (grabbox-getline-info))
    (grabbox-examine-line)))

(defun grabbox-section-menu ()
  "Jump to a grabbox categorie with completion"
  (interactive)
  (let ((running t)
        (group)
        (completions)
        (line-cache (make-hash-table))
        (node))
    (save-excursion
      (goto-char (point-max))
      (while running
        (beginning-of-line)
        (when (looking-at "\\[\\(.+\\)\\]")
          (setq group (match-string-no-properties 1))
          (add-to-list 'completions group)
          (cl-puthash (intern group)
                      (+ 1 (count-lines (point-min) (point)))
                      line-cache))
        (setq running (eq 0 (forward-line -1)))))
    (setq node (completing-read "Goto Group: " (mapcar 'list completions)))
    (goto-line (gethash (intern node) line-cache))
    (forward-line 1)
    (recenter 3)))

(defun grabbox-comment-uncomment-line ()
  "Comment/Uncomment current line"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((arg
           (if (looking-at "[ \t]*;")
               '(1)
             1)))
      (toggle-read-only nil)
      (comment-region (line-beginning-position) (line-end-position) arg)
      (toggle-read-only t))))

(defun grabbox-insert-last-buffer-file-name ()
  "Insert a link to the last buffer-file-name in the grabbox-file.
If the last buffer was an info node: insert a link to this info node"
  (interactive)
  (let* ((buf-list (copy-alist (buffer-list)))
         (buffer (progn (delq (get-buffer " *Minibuf-0*") buf-list)
                        (delq (get-buffer " *Minibuf-1*") buf-list)
                        (nth 1 buf-list)))
         (file-name (buffer-file-name buffer))
         (comment (if file-name (read-string (format "Alias for %s: " file-name))))
         (buffer-read-only nil)
         (info-node nil)
         (buf-mode nil))
    (save-excursion
      (set-buffer buffer)
      (setq buf-mode major-mode)
      (when (eq buf-mode 'Info-mode)
        (setq info-node (concat "(" Info-current-file ")" Info-current-node))))
    (if file-name
        (progn
          (when (> (length comment) 0) (setq comment (format "<%s> " comment)))
          (insert (format "F %s%s\n" comment file-name))
          (save-buffer)
          )
      (if (eq buf-mode 'Info-mode)
          (progn
            (setq comment (read-string (format "Alias for info node '%s': " info-node)))
            (when (> (length comment) 0) (setq comment (format "<%s> " comment)))
            (insert (format "I %s%s\n" comment info-node))
            (save-buffer))
        (message "Buffer %s has no file-name" buffer)))))
;(Info-goto-node "(c:/emacs-20.7/site-lisp/info/pcl-cvs)Viewing differences")

(defun grabbox-create-index-function ()
  "Create a section index for imenu"
  (let ((running t)
        (info)
        (imenu-alist)
        (line-cache (make-hash-table)))
    (save-excursion
      (goto-char (point-max))
      (while running
        (setq info (grabbox-getline-info))
        (when (looking-at "\\[\\(.+\\)\\]")
          (add-to-list 'imenu-alist (cons (match-string-no-properties 1) (point))))
        ;(when (and info (eq (nth 0 info) 'find-file) (nth 2 info))
        ;  (add-to-list 'imenu-alist (cons (nth 2 info) (point))))
        (setq running (eq 0 (forward-line -1)))))
    imenu-alist))

(provide 'grabbox)
