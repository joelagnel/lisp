;;; mac-key-mode.el --- provide mac-style key bindings on Carbon Emacs

;; Copyright (C) 2004-2006  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: v20060423
;; Keywords: tools, mac
;; Created: 2004-12-27
;; Compatibility: Mac OS X (Carbon Emacs)
;; URL(jp): http://macwiki.sourceforge.jp/cgi-bin/wiki.cgi?MacKeyMode
;; URL(en): http://homepage.mac.com/zenitani/comp-e.html

;; Contributors: Tetsuro Kurita, Nozomu Ando

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides mac-key-mode, a minor mode that provides
;; mac-like key bindings and relevant elisp functions.
;;
;; To use this package, add these lines to your .emacs file:
;;
;; ;;    (require 'redo)
;;     (require 'mac-key-mode)
;;     (mac-key-mode 1)
;;
;; Note that mac-key-mode requires redo.el and that it turns on
;; pc-selection-mode.
;; In order to set additional key bindings,
;; modify mac-key-mode-map in your .emacs file:
;;
;;     (require 'mac-key-mode)
;;     (define-key mac-key-mode-map [(alt l)] 'goto-line)
;;
;; When mac-key-mode is on, command key is recognized as 'alt' key,
;; but option (alt) key is also recognized as 'alt' key.
;; If you would like to use option (alt) key as meta key,
;; add the below line to your .emacs.el.
;;
;; (add-hook 'mac-key-mode-hook
;;        (lambda()
;;          (interactive)
;;          (if mac-key-mode
;;              (setq mac-option-modifier 'meta)
;;              (setq mac-option-modifier nil)
;;              )))


;;; Code:

;; requires redo
(require 'redo)
(require 'pc-select)

(defgroup mac-key-mode nil
  "Mac-style key-binding mode."
  :group 'mac
  :version "22.1")
(defcustom mac-key-mode-lighter
  (concat " " (char-to-string 323935)) ;; the Apple mark
;;  (concat " " (char-to-string (ucs-to-char 63743))) ;; the Apple mark
  "A lighter string which is displayed in the modeline
when `mac-key-mode' is on."
  :group 'mac-key-mode
  :type 'string)
(defcustom mac-key-mode-hook nil
  "The hook to run when mac-key-mode is toggled."
  :type 'hook
  :group 'mac-key-mode)
(defcustom mac-key-remote-file-regexp "\\`/[^/:]+:"
  "*Regular expression matching file names on the remote host."
  :group 'mac-key-mode
  :type 'string)
(defcustom mac-key-modify-file-menu-p t
  "If non-nil, `mac-key-mode' addes several menu items to the File menu
and the Edit menu in the menu bar."
  :group 'mac-key-mode
  :type 'boolean)

(defvar mac-key-backup-command-modifier nil
  "Do not use this variable.")
(defvar mac-key-backup-pc-selection-mode nil
  "Do not use this variable.")

(defvar mac-key-smartactivate-command nil
  "Path to the activate command, provided by Kurita-san's SmartActivate package.
Visit http://homepage.mac.com/tkurita/scriptfactory/ for more details.
If non-nil, `mac-key-show-in-finder' takes advandage of it.")


(defvar mac-key-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(alt c)] 'kill-ring-save)
    (define-key map [(alt v)] 'yank)
    (define-key map [(alt x)] 'kill-region)
    (define-key map [(alt a)] 'mark-whole-buffer)
    (define-key map [(alt z)] 'undo)
    (define-key map [(alt shift z)] 'redo) ; requires redo
    (define-key map [(alt f)] 'isearch-forward)
    (define-key map [(alt meta f)] 'occur)
    (define-key map [(alt g)] 'isearch-repeat-forward)
    (define-key map [(alt shift g)] 'isearch-repeat-backward)
    (define-key map [(alt o)] 'mac-key-open-file)
    (define-key map [(alt s)] 'save-buffer)
    (define-key map [(alt w)] 'kill-this-buffer)
    (define-key map [(alt m)] 'iconify-frame)
    (define-key map [(alt q)] 'save-buffers-kill-emacs)
    (define-key map [(alt \`)] 'other-frame)
    (define-key map [(alt p)] 'print-buffer)
    (define-key map [(alt i)] 'mac-key-show-in-finder)
    (define-key map [(alt .)] 'keyboard-quit)
    (define-key map [(alt up)] 'beginning-of-buffer)
    (define-key map [(alt down)] 'end-of-buffer)
    (define-key map [(alt left)] 'beginning-of-line)
    (define-key map [(alt right)] 'end-of-line)
    (define-key map [A-mouse-1] 'browse-url-at-mouse)
    map)
  "Keymap for `mac-key-mode'.")

;;;###autoload
(define-minor-mode mac-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on if arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'mac-key-mode
  :lighter mac-key-mode-lighter
  :keymap 'mac-key-mode-map
  (if mac-key-mode
      (progn

        (setq mac-key-backup-command-modifier mac-command-modifier)
        (setq mac-key-backup-pc-selection-mode pc-selection-mode)

        (setq mac-command-modifier 'alt)
        (setq pc-select-selection-keys-only t) ; M-DEL [macemacsjp-english 459]
        (pc-selection-mode 1)

        (when mac-key-modify-file-menu-p
          (define-key-after menu-bar-file-menu [mac-file-separator]
            '("--" . nil) 'recover-session)
          (define-key-after menu-bar-file-menu [mac-show-in-finder]
            '("Show In Finder" . mac-key-show-in-finder) 'mac-file-separator)
          (define-key-after menu-bar-file-menu [mac-open-terminal]
            '("Open Terminal" . mac-key-open-terminal) 'mac-show-in-finder)
          (define-key-after menu-bar-edit-menu [mac-redo]
            '("Redo" . redo) 'undo)
          (define-key-after menu-bar-edit-menu [mac-edit-separator]
            '("--" . nil) 'mac-redo)
          )
        )
    (progn

      (if mac-key-backup-pc-selection-mode
          (pc-selection-mode 1)
          (pc-selection-mode 0))
      (setq mac-command-modifier mac-key-backup-command-modifier)

      (define-key global-map [menu-bar file mac-file-separator] nil)
      (define-key global-map [menu-bar file mac-show-in-finder] nil)
      (define-key global-map [menu-bar file mac-open-terminal] nil)
      (define-key global-map [menu-bar edit mac-redo] nil)
      (define-key global-map [menu-bar edit mac-edit-separator] nil)

      ))
  )


;; courtesy of Lawrence Akka (EmacsWiki: MacOSTweaks)
(defun mac-key-open-file (filename &optional wildcards)
  "Open a file using standard file open dialog."
  (interactive
   (let ((last-nonmenu-event nil)) 
     (find-file-read-args "Find existing file: " t)))
  (find-file-existing filename wildcards)
  )

;; (defun mac-key-open-file ()
;;   "Document forthcoming..."
;;   (interactive)
;;   (let ((file (do-applescript "try
;; POSIX path of (choose file)
;; end try")))
;;     (if (> (length file) 3)
;;         (if (equal current-language-environment "Japanese")
;;             (setq file
;;                   (replace-regexp-in-string
;;                    "\\\\\\(.\\)" "\\1"
;;                    (decode-coding-string
;;                     (substring file 1 (- (length file) 1))
;;                     'sjis-mac)))
;;           (setq file
;;                 (substring file 1 (- (length file) 1)))
;;           ))
;;     (if (and (not (equal file ""))(file-readable-p file))
;;         (find-file file)
;;       (beep))
;;     ))


;; utf8 code by Ando-san
(defun mac-key-applescript-utf8data (str)
  (let ((len (length str))
        (len1 31) ;XXX: 254/2/4. utf-8 is 4byte per code point at most.
        (reslist '(")"))
        pos epos)
    (setq pos len)
    (while (> pos 0)
      (setq epos pos)
      (setq pos (max (- pos len1) 0))
      (setq reslist (cons " & (\307data utf8"
                          (cons (mapconcat (lambda (ch) (format "%02X" ch))
                                           (encode-coding-string
                                            (substring str pos epos)
                                            'utf-8) "")
                                (cons "\310 as Unicode text)"
                                      reslist)))))
    (apply 'concat "(\"\"" reslist)))


;; Show In Finder

(defun mac-key-show-in-finder ()
  "Document forthcoming..."
  (interactive)
  (let ((mode nil)
        (file nil))

    (cond
     ((eq major-mode 'dired-mode)
      (cond
       ((string-match mac-key-remote-file-regexp dired-directory)
        (error "Remote directories not supported"))
       ((string-match "\\.app/\\'" dired-directory)
        (setq file (expand-file-name dired-directory))
        (setq mode 'info))
       (t
        (setq mode 'dir))
       ))
     ((and (boundp 'buffer-file-name)
           (stringp buffer-file-name))
      (cond
       ((string-match mac-key-remote-file-regexp buffer-file-name)
        (error "Remote files not supported"))
;;        ((string-match "^\\." (file-name-nondirectory buffer-file-name))
;;         (message "cannot show dot file"))
       (t
        (setq file buffer-file-name)
        (setq mode 'info))
      ))
     )

    (cond
     ((eq mode 'info)
      ;; applescript error handling
      ;; ref. http://fobj.com/hisa/w/CarbonEmacs.html
      (condition-case err
          (progn
            (do-applescript
             (concat
              "tell application \"Finder\" to select ("
              (mac-key-applescript-utf8data file)
              " as POSIX file)"))
;;              (format "tell application \"Finder\" to select (\"%s\" as POSIX file)"
;;                      (if (eq selection-coding-system 'sjis-mac)
;;                          (replace-regexp-in-string
;;                           "\\\\" "\\\\\\\\"
;;                           (encode-coding-string file selection-coding-system))
;;                        (encode-coding-string file selection-coding-system))
;;                      ))
            (if mac-key-smartactivate-command
                (shell-command
                 (concat mac-key-smartactivate-command
                         " -i com.apple.finder"))
              (do-applescript "tell application \"Finder\" to activate"))
            )
        (error err))
      )
      ((eq mode 'dir)
       (shell-command "/usr/bin/open ."))

    )))


;; Open Terminal.app

(defun mac-key-open-terminal ()
  "Document forthcoming..."
  (interactive)
  (let ((dir nil))

    (cond
     ((eq major-mode 'dired-mode)
      (cond
       ((string-match mac-key-remote-file-regexp dired-directory)
        (error "Remote directories not supported"))
       (t
        (setq dir (expand-file-name dired-directory)))
       ))
     ((and (boundp 'buffer-file-name)
           (stringp buffer-file-name))
      (cond
       ((string-match mac-key-remote-file-regexp buffer-file-name)
        (error "Remote files not supported"))
       (t
        (setq dir (file-name-directory buffer-file-name)))
      ))
     )

    (if (file-directory-p dir)
        (condition-case err
            (progn
              (do-applescript
               (concat "tell application \"Terminal\" to do script"
                       " with command \"cd \" & quoted form of "
                       (mac-key-applescript-utf8data dir)))
;;                (format "tell application \"Terminal\" to do script with command \"cd %s\"" dir))
              (if mac-key-smartactivate-command
                  (shell-command
                   (concat mac-key-smartactivate-command
                           " -i com.apple.Terminal"))
                (do-applescript "tell application \"Terminal\" to activate"))
              )
          (error err)))
    ))

(provide 'mac-key-mode)

;;; mac-key-mode.el ends here.
