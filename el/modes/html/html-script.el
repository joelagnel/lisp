;; html-script.el -- narrow/widen on blocks of php, css, javascript
;;                   and visual basic embedded in (X)HTML
;;
;; Copyright (C) 2005 by P J Heslin
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; URL: http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/html-script.el
;; Version: 2.1
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; When using html-mode or nxml-mode to edit (X)HTML, this file
;; enables you to narrow the visible buffer to show just a
;; php/javascript/visual basic script block, or a css style block, and
;; then to switch from html-mode or nxml-mode to the appropriate mode
;; for that block.  When finished editing that block, it enable you
;; easily to show the whole buffer and switch back to the original
;; mode again.
;;
;; The user interface consists of a single keystroke: by default, F12.
;; To change it, customize the value of 'html-script-key.  In html-mode
;; or nxml-mode, this key looks to see if the cursor is inside a
;; script/style block, and if it is, it narrows the buffer to that
;; block and switches to the appropriate mode.  Hit the same key again
;; to go back to editing the whole file in the original mode.
;;
;; This file used to be called nxml-script.el, when it only supported
;; nxml-mode; now it has been completely rewritten and made a bit more
;; general, so it can be used with any major mode for editing (X)HTML.
;;
;; The original idea was taken from html-helper-mode.el

;;; Installation:
;;
;; To install, put this file in your load-path, and require it via
;; your .emacs.
;;
;;  (require 'html-script)
;;
;; There are various customizations available, including a list of
;; modes in which to install html-script (defaults to html-mode and
;; nxml-mode). 
;;
;; You also need to have the relevant autoloads set up for the script
;; modes you want to use, like so:
;;
;;  (autoload 'php-mode "php-mode" "PHP mode" t)
;;
;; Supported major modes for script/style blocks include php-mode,
;; css-mode, javascript-generic-mode, jde-mode, visual-basic-mode.
;;
;; You might also need to tell emacs to open .php, .jsp and such files
;; in html-mode or nxml-mode, like so:
;;
;;  (setq auto-mode-alist (cons '("\\.php[34]?$" . nxml-mode) auto-mode-alist))
;;
;; BUGS:
;;
;; I regularly use only CSS and PHP, very occasionally Javascript, and
;; Visual Basic not at all, and so the code for these last two has not
;; been tested.
;;
;; Changes:
;;
;; 1.0 First public release of nxml-script.el
;;
;; 2.0 Changed name to html-script.el and completely rewritten.
;; Removed 'nxml-script-function and 'nxml-script-region-function, as
;; too confusing.  Various user customization variables have changed.
;;
;; 2.1 Bugfix for javascript and vbscript and added ecmascript mode --
;; thanks to Mark Takacs.

(defgroup html-script nil
  "Narrow (X)HTML documents to script blocks and widen again"
  :tag "Narrow (X)HTML documents to script blocks and widen again"
  :group 'hypermedia
  :prefix "html-script-")

(defcustom html-script-install-modes '(nxml-mode html-mode)
  "A list of modes in which to install a binding for html-script.
The key to bind is defined by html-script-key")

(defcustom html-script-key [f12]
  "Key to use to toggle narrowing and widening")

(defcustom html-script-regions
  '(("<%" "%>" visual-basic-mode)
    ("<\\?" "\\?>" php-mode c-mode)
    ("<style[ \t]+type=\"text/css\"" "</style>" css-mode c-mode)
    ("<style[ \t]+type=\"css\"" "</style>" css-mode c-mode)
    ("<script[ \t]+language=\"vbscript\"[ \t]*>" "</script>" visual-basic-mode)
    ("<script[ \t]+type=\"text/vbscript\"[ \t]*>" "</script>" visual-basic-mode)
    ("<script[ \t]+language=\"javascript\"[ \t]*>" "</script>"
     ecmascript-mode jde-mode java-mode javascript-generic-mode c-mode)
    ("<script[ \t]+type=\"text/javascript\"[ \t]*>" "</script>"
     ecmascript-mode jde-mode java-mode javascript-generic-mode c-mode))
  "Define script regions.  Each entry should consist of a list of
  a starting regexp, an ending regexp, and a list of modes to try
  for that region in that order.")

(defvar html-script-start-regexp
  (concat "\\(" (mapconcat (lambda (x) (car x)) html-script-regions "\\|") "\\)"))

(defvar html-script-original-mode nil)
(make-variable-buffer-local 'html-script-original-mode)
;; This immunizes it against kill-all-local-variables, which is run
;; when the new major-mode is called.
(put 'html-script-original-mode 'permanent-local t)

(require 'cl)

(defun html-script-narrow ()
  (interactive)
  (let* ((orig (point))
         (case-fold-search t)
         (handler-list
          (if (re-search-backward html-script-start-regexp nil t)
              (loop for x in html-script-regions
                    when (looking-at (car x)) return (cdr x)
                    finally do (error "html-script: regexp mismatch"))
            (message "Not in a script region.")
            nil)))
    (if handler-list
        (html-script-narrow-handler orig handler-list)
      (goto-char orig))))

(defun html-script-narrow-handler (orig arg-list)
  (beginning-of-line)
  (let ((ending-re (car arg-list))
        (modes (cdr arg-list))
        (beg (point))
        (case-fold-search t))
    (if (re-search-forward ending-re nil t)
        (if (>= (point) orig)
            (progn
              (setq html-script-original-mode major-mode)
              (narrow-to-region beg (point))
              (loop for x in modes when (fboundp x) do (funcall x) and return nil
                    finally do (error "html-script: no relevant mode found."))
              (html-script-install-widen-key))
          (message "Not in a script region."))
      (message "End of script region not found."))
    (goto-char orig)))
     

(defun html-script-widen ()
  (interactive)
  (widen)
  (funcall html-script-original-mode))

(defun html-script-install-widen-key ()
  (local-set-key html-script-key 'html-script-widen))

(defun html-script-install-narrow-key ()
  (local-set-key html-script-key 'html-script-narrow))

(dolist (x html-script-install-modes)
  (add-hook 
   (intern (concat (symbol-name x) "-hook"))
   'html-script-install-narrow-key))

(provide 'html-script)

