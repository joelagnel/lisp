;;; color-theme-hober2.el --- Edward O'Connor's second color theme.

;; Copyright (C) 1997 -- 2006 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: local

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:

;; This is my second color theme. Apologies for the formatting; this
;; file is automatically extracted from a LaTeX master file.

;;; Code:

(require 'color-theme)

(defun hober2-use-underline-p (&optional display)
  "Non-nil if DISPLAY supports underlining.
This is only sometimes accurate."
  (if (fboundp 'display-supports-face-attributes-p)
      (display-supports-face-attributes-p '(:underline) display)
    window-system))


(defvar hober2-insignificant-face
  (if window-system
     '(:foreground "gray30")
   '()))

;; Because of the backquote in `color-theme-hober2', your
;; `max-lisp-eval-depth' must be at least 488. Sorry.
(when (< max-lisp-eval-depth 488)
  (setq max-lisp-eval-depth 488))

(defun color-theme-hober2 ()
  "Color theme by Edward O'Connor <ted@oconnor.cx>.

Like my previous theme, I've attempted to make sure things look
reasonably similar under console or window system. The colors of
`color-theme-hober2' are often washed-out variants of the colors
of `color-theme-hober'.

Includes antlr, apropos, AUCTeX (font-latex), bbdb, Buffer-menu,
calendar, change-log, comint, compilation, cperl, custom, darcsum,
diary, diff, dired, ediff, erc, eshell, filladapt, font-lock, forms,
gnus, goto-address, help, highline, hl-line, holiday, html-helper,
hyper-apropos, ibuffer, Info, isearch, jde, makefile, Man, message,
multi-region, nxml, psvn, sgml, sh, show-paren, show-tabs, slime, sldb,
speedbar, tex, view, viper, w3m, widget, woman"
  (interactive)
  (color-theme-install
   `(color-theme-hober2
     ;; Frame parameters
     ((foreground-color . "light gray")
      (background-color . "gray4")
      (mouse-color      . "khaki")
      (cursor-color     . "khaki")
      (border-color     . "gray4")
      (background-mode  . dark))
     ;; Face variables
     ((Man-overstrike-face . font-lock-variable-name-face)
      (Man-underline-face  . font-lock-string-face)

      (apropos-symbol-face     . font-lock-keyword-face)
      (apropos-keybinding-face . font-lock-constant-face)
      (apropos-label-face      . font-lock-type-face)
      (apropos-property-face   . font-lock-string-face)
      (apropos-match-face      . font-lock-function-name-face)

      (cperl-pod-face      . font-lock-doc-string-face)
      (cperl-pod-head-face . font-lock-variable-name-face)
      (cperl-here-face     . font-lock-doc-string-face)
      (cperl-invalid-face  . font-lock-warning-face)

      (dired-header-face    . dired-header)
      (dired-mark-face      . dired-mark)
      (dired-marked-face    . dired-marked)
      (dired-flagged-face   . dired-flagged)
      (dired-warning-face   . dired-warning)
      (dired-directory-face . dired-directory)
      (dired-symlink-face   . dired-symlink)
      (dired-ignored-face   . dired-ignored)

      (filladapt-debug-indentation-face-1 . highlight)
      (filladapt-debug-indentation-face-2 . secondary-selection)
      (filladapt-debug-paragraph-face     . bold)

      (forms-ro-face . custom-comment-tag-face)
      (forms-rw-face . custom-comment-face)

      (goto-address-url-face        . font-lock-string-face)
      (goto-address-url-mouse-face  . highlight)
      (goto-address-mail-face       . font-lock-string-face)
      (goto-address-mail-mouse-face . highlight)

      (help-highlight-face . font-lock-variable-name-face)

      (hl-line-face . fringe)

      (ibuffer-deletion-face . font-lock-warning-face)
      (ibuffer-filter-group-name-face . bold)
      (ibuffer-marked-face . font-lock-comment-face)
      (ibuffer-title-face . dired-header)

      (list-matching-lines-face             . bold)
      (list-matching-lines-buffer-name-face . font-lock-type-face)

      (view-highlight-face . highlight)

      (viper-replace-overlay-cursor-color . "pale violet red") ;%
      (viper-insert-state-cursor-color    . "light pink") ;%
      (viper-vi-state-cursor-color        . "khaki")

      (widget-mouse-face . highlight))
     ;; Faces
     (default ((t (:background "gray4" :foreground "light gray"
                   :stipple nil :box nil :strike-through nil :overline nil
                   :underline nil :slant normal :weight normal
                   :inverse-video nil))))

     (antlr-font-lock-keyword-face ((t (:foreground "LightSteelBlue4")))) ;%
     (antlr-font-lock-literal-face ((t (:foreground "pale violet red"))))
     (antlr-font-lock-ruledef-face ((t (:foreground "sea green"))))
     (antlr-font-lock-ruleref-face ((t (:foreground "steel blue"))))
     (antlr-font-lock-tokendef-face ((t (:foreground "khaki"))))
     (antlr-font-lock-tokenref-face ((t (:foreground "LightSteelBlue4"))))

     (bbdb-company ((t (:foreground "indian red")))) ;%
     (bbdb-field-name ((t (:foreground "steel blue"))))
     (bbdb-field-value ((t (:foreground "lemon chiffon")))) ;%
     (bbdb-name ((t (:foreground "cadet blue"))))

     (bg:erc-color-face0 ((t (:background "light gray"))))
     (bg:erc-color-face1 ((t (:background "gray30"))))
     (bg:erc-color-face2 ((t (:background "dark slate blue"))))
     (bg:erc-color-face3 ((t (:background "sea green"))))
     (bg:erc-color-face4 ((t (:background "indian red"))))
     (bg:erc-color-face5 ((t (:background "sandy brown"))))
     (bg:erc-color-face6 ((t (:background "medium orchid")))) ;%
     (bg:erc-color-face7 ((t (:background "orange red"))))
     (bg:erc-color-face8 ((t (:background "lemon chiffon"))))
     (bg:erc-color-face9 ((t (:background "medium sea green")))) ;%
     (bg:erc-color-face10 ((t (:background "LightSteelBlue4"))))
     (bg:erc-color-face11 ((t (:background "cadet blue"))))
     (bg:erc-color-face12 ((t (:background "steel blue"))))
     (bg:erc-color-face13 ((t (:background "pale violet red"))))
     (bg:erc-color-face14 ((t (:background "gray50"))))
     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t :slant italic))))

     (border ((t nil)))

     (Buffer-menu-buffer-face ((t (:foreground "lemon chiffon")))) ;%

     (calendar-today-face ((t (:foreground "khaki")))) ;%

     (change-log-date-face ((t (:foreground "lemon chiffon"))))
     (change-log-name-face ((t (:foreground "khaki"))))
     (change-log-email-face ((t (:foreground "indian red"))))
     (change-log-file-face ((t (:foreground "cadet blue"))))
     (change-log-list-face ((t (:foreground "LightSteelBlue4"))))
     (change-log-conditionals-face ((t (:foreground "indian red"))))
     (change-log-function-face ((t (:foreground "indian red"))))
     (change-log-acknowledgement-face ((t (:background "cadet blue" :foreground "gray4"))))

     (comint-highlight-input ((t (:bold t))))
     (comint-highlight-prompt ((t (:foreground "pale violet red"))))

     (compilation-warning-face ((t (:background "indian red" :foreground "white"))))
     (compilation-warning ((t (:background "indian red" :foreground "white"))))
     (compilation-info-face ((t (:foreground "dark orchid"))))
     (compilation-info ((t (:foreground "dark orchid"))))
     (compilation-error-face ((t (:foreground "dark orchid"))))
     (compilation-error ((t (:foreground "dark orchid"))))

     (cperl-array-face ((t (:foreground "pale violet red")))) ;%
     (cperl-array ((t (:foreground "pale violet red")))) ;%
     (cperl-nonoverridable-face ((t (:foreground "sandy brown"))))
     (cperl-nonoverridable ((t (:foreground "sandy brown"))))
     (cperl-hash-face ((t (:foreground "medium orchid"))))
     (cperl-hash ((t (:foreground "medium orchid"))))

     (css-selector ((t (:foreground "indian red"))))
     (css-property ((t (:foreground "light sea green"))))

     (cursor ((t (:background "khaki" :foreground "gray4"))))

     (custom-button-face ((t (:foreground "gray4" :background "light gray" :bold t :underline t))))
     (custom-button-pressed-face ((t (:foreground "gray4" :background "light gray" :bold t
                                      :underline t))))
     (custom-face-tag-face ((t (:bold t))))
     (custom-variable-tag-face ((t (:bold t))))
     (custom-state-face ((t (:foreground "medium sea green"))))

     (darcsum-header-face ((t (:foreground "lemon chiffon")))) ;%
     (darcsum-marked-face ((t (:bold t))))
     (darcsum-need-action-face ((t (:foreground "orange red"))))
     (darcsum-need-action-marked-face ((t (:foreground "orange red" :bold t))))
     (darcsum-filename-face ((t (:foreground "light sea green"))))
     (darcsum-changed-line-face ((t (:background "gray30" :foreground "white"))))

     (diary-face ((t (:foreground "indian red"))))
     (diary-button-face ((t (:foreground "gray4"
                             :background "light gray"
                             :bold t :underline t))))

     (diff-hunk-header-face ((t (:background "gray30" :foreground "white"))))

     (dired-flagged ((t (:background "indian red" :foreground "white"))))
     (dired-face-flagged ((t (:background "indian red" :foreground "white"))))
     (dired-header ((t (:foreground "medium sea green"))))
     (dired-face-header ((t (:foreground "medium sea green"))))
     (dired-mark ((t (:foreground "khaki"))))
     (dired-marked ((t (:background "cadet blue" :foreground "gray4"))))
     (dired-face-marked ((t (:background "cadet blue" :foreground "gray4"))))
     (dired-warning ((t (:background "indian red" :foreground "white"))))

     (dired-directory ((t (:foreground "khaki"))))
     (dired-face-directory ((t (:foreground "khaki"))))
     (dired-face-executable ((t (:foreground "medium sea green"))))
     (dired-ignored ((t ,hober2-insignificant-face)))
     (dired-face-boring ((t ,hober2-insignificant-face)))
     (dired-face-permissions ((t ,hober2-insignificant-face)))
     (dired-face-setuid ((t (:bold t :foreground "medium sea green"))))
     (dired-face-socket ((t ,hober2-insignificant-face)))
     (dired-symlink ((t (:foreground "cadet blue"))))
     (dired-face-symlink ((t (:foreground "cadet blue"))))
     (ediff-current-diff-face-A ((t (:background "cadet blue" :foreground "gray4"))))
     (ediff-current-diff-face-Ancestor ((t (:background "IndianRed1"
                                            :foreground "light gray"))))
     (ediff-current-diff-face-B ((t (:background "lemon chiffon" :foreground "gray30"))))
     (ediff-current-diff-face-C ((t (:background "medium sea green"
                                     :foreground "light gray"))))
     (ediff-even-diff-face-A ((t (:background "cadet blue" :foreground "gray4" :bold t))))
     (ediff-even-diff-face-Ancestor ((t (:background "IndianRed1"
                                         :foreground "light gray"
                                         :bold t))))
     (ediff-even-diff-face-B ((t (:background "lemon chiffon" :foreground "gray30"
                                  :bold t))))
     (ediff-even-diff-face-C ((t (:background "medium sea green" :foreground "light gray"
                                  :bold t))))
     (ediff-fine-diff-face-A ((t (:background "steel blue" :foreground "gray4"))))
     (ediff-fine-diff-face-Ancestor ((t (:background "orange red"
                                         :foreground "gray4"))))
     (ediff-fine-diff-face-B ((t (:background "LightSteelBlue4" :foreground "white"))))
     (ediff-fine-diff-face-C ((t (:background "sea green"
                                  :foreground "gray4"))))
     (ediff-odd-diff-face-A ((t (:background "steel blue" :foreground "gray4"
                                 :bold t))))
     (ediff-odd-diff-face-Ancestor ((t (:background "orange red"
                                        :foreground "gray4"
                                        :bold t))))
     (ediff-odd-diff-face-B ((t (:background "LightSteelBlue4" :foreground "white" :bold t))))
     (ediff-odd-diff-face-C ((t (:background "sea green"
                                 :foreground "gray4" :bold t))))

     (erc-action-face ((t (:bold t))))
     (erc-bold-face ((t (:bold t))))
     (erc-command-indicator-face ((t (:bold t))))
     (erc-current-nick-face ((t (:foreground "light sea green"))))
     (erc-default-face ((t ())))
     (erc-direct-msg-face ((t (:foreground "cadet blue")))) ;%
     (erc-error-face ((t (:background "indian red" :foreground "white" :bold t))))
     (erc-fool-face ((t ,hober2-insignificant-face)))
     (erc-input-face ((t (:foreground "lemon chiffon"))))
     (erc-inverse-face ((t (:inverse-video t))))
     (erc-keyword-face ((t (:foreground "medium orchid" :bold t))))
     (erc-nick-default-face ((t (:foreground "cadet blue"))))
     (erc-nick-msg-face ((t (:foreground "pale violet red" :bold t))))
     (erc-notice-face ((t (:foreground "steel blue"))))
     (erc-pal-face ((t (:foreground "pale violet red"))))
     (erc-prompt-face ((t (:foreground "pale violet red")))) ;%
     (erc-timestamp-face ((t (:foreground "medium sea green")))) ;%
     (erc-underline-face ((t (:underline t)))) ;%

     (escape-glyph ((t ,hober2-insignificant-face)))
     (eshell-prompt-face ((t (:foreground "pale violet red"))))
     (eshell-prompt ((t (:foreground "pale violet red"))))

     (eshell-ls-archive-face ((t (:foreground "indian red")))) ;%
     (eshell-ls-archive ((t (:foreground "indian red")))) ;%
     (eshell-ls-backup-face ((t (:foreground "LightSteelBlue4"))))
     (eshell-ls-backup ((t (:foreground "LightSteelBlue4"))))
     (eshell-ls-clutter-face ((t (:background "indian red" :foreground "white"))))
     (eshell-ls-clutter ((t (:background "indian red" :foreground "white"))))
     (eshell-ls-directory-face ((t (:foreground "khaki"))))
     (eshell-ls-directory ((t (:foreground "khaki"))))
     (eshell-ls-executable-face ((t (:foreground "medium sea green"))))
     (eshell-ls-executable ((t (:foreground "medium sea green"))))
     (eshell-ls-missing-face ((t (:background "indian red" :foreground "white" :bold t))))
     (eshell-ls-missing ((t (:background "indian red" :foreground "white" :bold t))))
     (eshell-ls-product-face ((t ,hober2-insignificant-face)))
     (eshell-ls-product ((t ,hober2-insignificant-face)))
     (eshell-ls-readonly-face ((t ,hober2-insignificant-face)))
     (eshell-ls-readonly ((t ,hober2-insignificant-face)))
     (eshell-ls-special-face ((t (:foreground "pale violet red")))) ;%
     (eshell-ls-special ((t (:foreground "pale violet red")))) ;%
     (eshell-ls-symlink-face ((t (:foreground "cadet blue"))))
     (eshell-ls-symlink ((t (:foreground "cadet blue"))))
     (eshell-ls-unreadable-face ((t (:background "indian red" :foreground "white" :bold t))))
     (eshell-ls-unreadable ((t (:background "indian red" :foreground "white" :bold t))))

     (fg:erc-color-face0 ((t (:foreground "light gray"))))
     (fg:erc-color-face1 ((t (:foreground "gray30")))) ;%
     (fg:erc-color-face2 ((t (:foreground "dark slate blue"))))
     (fg:erc-color-face3 ((t (:foreground "sea green"))))
     (fg:erc-color-face4 ((t (:foreground "indian red"))))
     (fg:erc-color-face5 ((t (:foreground "sandy brown"))))
     (fg:erc-color-face6 ((t (:foreground "medium orchid"))))
     (fg:erc-color-face7 ((t (:foreground "orange red"))))
     (fg:erc-color-face8 ((t (:foreground "lemon chiffon")))) ;%
     (fg:erc-color-face9 ((t (:foreground "medium sea green")))) ;%
     (fg:erc-color-face10 ((t (:foreground "LightSteelBlue4"))))
     (fg:erc-color-face11 ((t (:foreground "cadet blue"))))
     (fg:erc-color-face12 ((t (:foreground "steel blue"))))
     (fg:erc-color-face13 ((t (:foreground "pale violet red")))) ;%
     (fg:erc-color-face14 ((t (:foreground "gray50"))))
     (fg:erc-color-face15 ((t (:foreground "gray90"))))

     (file-name-shadow ((t ,hober2-insignificant-face)))
     (font-latex-bold-face ((t (:bold t :foreground "medium sea green"))));%
     (font-latex-italic-face ((t (:slant italic :foreground "medium sea green"))));%
     (font-latex-math-face ((t (:foreground "sandy brown"))))
     (font-latex-sectioning-0-face ((t (:foreground "khaki"))))
     (font-latex-sectioning-1-face ((t (:foreground "khaki"))))
     (font-latex-sectioning-2-face ((t (:foreground "khaki"))))
     (font-latex-sectioning-3-face ((t (:foreground "khaki"))))
     (font-latex-sectioning-4-face ((t (:foreground "khaki"))))
     (font-latex-sedate-face ((t ,hober2-insignificant-face)))
     (font-latex-string-face ((t (:foreground "lemon chiffon"))))
     (font-latex-subscript-face ((t ())))
     (font-latex-title-1-face ((t (:foreground "LightSteelBlue4"))))
     (font-latex-title-2-face ((t (:foreground "LightSteelBlue4"))));%
     (font-latex-title-3-face ((t (:foreground "LightSteelBlue4"))))
     (font-latex-title-4-face ((t (:foreground "LightSteelBlue4"))))
     (font-latex-verbatim-face ((t ())))
     (font-latex-warning-face ((t (:background "indian red" :foreground "white"))))


     (font-lock-builtin-face ((t (:foreground "sandy brown")))) ;%
     (font-lock-color-constant-face ((t (:foreground "khaki")))) ;%
     (font-lock-comment-face ((t (:foreground "dark orchid"))))
     (font-lock-comment-delimiter-face ((t (:foreground "medium orchid"))))
     (font-lock-constant-face ((t (:foreground "pale violet red"))))
     (font-lock-doc-face ((t (:foreground "khaki")))) ;%
     (font-lock-doc-string-face ((t ,(if (featurep 'xemacs)
                                         '(:foreground "khaki")
                                       '(:foreground "medium orchid")))))
     (font-lock-function-name-face ((t (:foreground "cadet blue"))))
     (font-lock-keyword-face ((t (:foreground "steel blue"))))
     (font-lock-preprocessor-face ((t (:foreground "khaki"))))
     (font-lock-reference-face ((t (:foreground "khaki"))))
     (font-lock-string-face ((t (:foreground "lemon chiffon"))))
     (font-lock-type-face ((t (:foreground "medium sea green")))) ;%
     (font-lock-variable-name-face ((t (:foreground "IndianRed1"))))
     (font-lock-warning-face ((t (:background "indian red" :foreground "white"))))

     (fringe ((t (:background "gray8" :foreground "white"))))

     (gnus-cite-attribution-face ((t ())))
     (gnus-cite-attribution ((t ())))
     (gnus-cite-face-1 ((t (:foreground "lemon chiffon"))))
     (gnus-cite-1 ((t (:foreground "lemon chiffon"))))
     (gnus-cite-face-2 ((t (:foreground "khaki"))))
     (gnus-cite-2 ((t (:foreground "khaki"))))
     (gnus-cite-face-3 ((t (:foreground "cadet blue"))))
     (gnus-cite-3 ((t (:foreground "cadet blue"))))
     (gnus-cite-face-4 ((t (:foreground "indian red"))))
     (gnus-cite-4 ((t (:foreground "indian red"))))
     (gnus-cite-face-5 ((t (:foreground "pale violet red"))))
     (gnus-cite-5 ((t (:foreground "pale violet red"))))
     (gnus-cite-face-6 ((t (:foreground "sea green"))))
     (gnus-cite-6 ((t (:foreground "sea green"))))
     (gnus-cite-face-7 ((t (:foreground "LightSteelBlue4")))) ;%
     (gnus-cite-7 ((t (:foreground "LightSteelBlue4"))))
     (gnus-cite-face-8 ((t (:foreground "gray"))))
     (gnus-cite-8 ((t (:foreground "gray"))))
     (gnus-cite-face-9 ((t (:foreground "steel blue"))))
     (gnus-cite-9 ((t (:foreground "steel blue"))))
     (gnus-cite-face-10 ((t (:foreground "lemon chiffon"))))
     (gnus-cite-10 ((t (:foreground "lemon chiffon"))))
     (gnus-cite-face-11 ((t (:foreground "khaki"))))
     (gnus-cite-11 ((t (:foreground "khaki"))))

     (gnus-emphasis-bold ((t (:bold t))))
     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))
     (gnus-emphasis-italic ((t (:italic t))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))
     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t))))
     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))

     (gnus-header-content-face ((t ,hober2-insignificant-face)))
     (gnus-header-content ((t ,hober2-insignificant-face)))
     (gnus-header-from-face ((t (:height 1.2 :foreground "DarkSeaGreen2"))))
     (gnus-header-from ((t (:height 1.2 :foreground "DarkSeaGreen2"))))
     (gnus-header-name-face ((t (:foreground "cadet blue")))) ;%
     (gnus-header-name ((t (:foreground "cadet blue"))))
     (gnus-header-newsgroups-face ((t (:foreground "medium sea green"))))
     (gnus-header-newsgroups ((t (:foreground "medium sea green"))))
     (gnus-header-subject-face ((t (:height 1.2 :foreground "lemon chiffon"))))
     (gnus-header-subject ((t (:height 1.2 :foreground "lemon chiffon"))))
     (gnus-signature-face ((t ,hober2-insignificant-face)))
     (gnus-signature ((t ,hober2-insignificant-face)))
     (gnus-summary-cancelled-face ((t (:foreground "khaki"))))
     (gnus-summary-cancelled ((t (:foreground "khaki"))))
     (gnus-summary-high-read-face ((t (:foreground "khaki"))))
     (gnus-summary-high-read ((t (:foreground "khaki"))))
     (gnus-summary-high-ticked-face ((t (:foreground "indian red"))))
     (gnus-summary-high-ticked ((t (:foreground "<<medium red>"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "IndianRed1"))))
     (gnus-summary-normal-ticked ((t (:foreground "IndianRed1"))))
     (gnus-summary-low-read-face ((t (:foreground "khaki"))))
     (gnus-summary-low-read ((t (:foreground "khaki"))))
     (gnus-summary-selected-face ((t (:background "gray30" :foreground "white"))))
     (gnus-summary-selected ((t (:background "gray30" :foreground "white"))))

     (header-line ((t (:box (:line-width 2
                             :color "gray8"
                             :style nil)
                       :background "gray8" :foreground "white"))))

     (help-argument-name ((t (:foreground "IndianRed1"))))

     (html-helper-bold-face ((t ())))
     (html-helper-italic-face ((t ())))
     (html-helper-underline-face ((t ())))
     (html-helper-strikethrough-face ((t ())))
     (html-helper-link-face ((t ())))
     (html-helper-significant-tag-face ((t (:foreground "cadet blue")))) ;%

     (hyper-apropos-documentation ((t (:foreground "white")))) ;%
     (hyper-apropos-hyperlink ((t (:underline t))))
     (hyper-apropos-major-heading ((t (:foreground "lemon chiffon")))) ;%
     (hyper-apropos-section-heading ((t (:foreground "khaki"))))
     (hyper-apropos-apropos-heading ((t (:foreground "sandy brown"))))
     (hyper-apropos-apropos-warning ((t (:background "indian red" :foreground "white"))))


     (highlight ((t (:background "sea green" :foreground "light gray" :underline t))))

     (highline-face ((t (:background "gray30" :foreground "white"))))
     (highline-vertical-face ((t (:background "gray30" :foreground "white"))))

     (holiday-face ((t (:foreground "pale violet red"))))

     (Info-title-1-face ((t (:foreground "IndianRed1"))))
     (Info-title-2-face ((t (:foreground "IndianRed1"))))
     (Info-title-3-face ((t (:foreground "IndianRed1"))))
     (Info-title-4-face ((t (:foreground "IndianRed1"))))
     (info-header-node ((t (:foreground "IndianRed1"))))
     (info-header-xref ((t (:foreground "cadet blue"))))
     (info-menu-5 ((t ())))
     (info-menu-star ((t ())))
     (info-menu-header ((t (:foreground "IndianRed1"))))
     (info-node ((t (:foreground "IndianRed1"))))
     (info-title-1 ((t (:foreground "IndianRed1"))))
     (info-title-2 ((t (:foreground "IndianRed1"))))
     (info-title-3 ((t (:foreground "IndianRed1"))))
     (info-title-4 ((t (:foreground "IndianRed1"))))
     (info-xref ((t (:foreground "cadet blue"))))

     (isearch ((t (:background "steel blue" :foreground "lemon chiffon"))))
     (isearch-lazy-highlight-face ((t (:background "indian red"
                                       :foreground "lemon chiffon"))))
     (lazy-highlight ((t (:background "indian red" :foreground "lemon chiffon"))))
     (isearch-secondary ((t (:background "indian red" :foreground "lemon chiffon"))))

     (iswitchb-current-match ((t (:foreground "cadet blue"))))
     (iswitchb-single-match ((t (:foreground "medium sea green"))))
     (iswitchb-invalid-regexp ((t (:background "indian red" :foreground "white" :bold t))))
     (iswitchb-virtual-matches ((t (:foreground "sandy brown"))))

     (italic ((t (:slant italic))))

     (jde-bug-breakpoint-cursor ((t (:background "cyan"))))
     (jde-db-active-breakpoint-face ((t (:background "cyan"))))
     (jde-db-requested-breakpoint-face ((t (:background "cyan"))))
     (jde-db-spec-breakpoint-face ((t (:background "cyan"))))
     (jde-java-font-lock-api-face ((t (:background "cyan"))))
     (jde-java-font-lock-bold-face ((t (:bold t :foreground "dark orchid"))))
     (jde-java-font-lock-code-face ((t (:foreground "indian red" ;%
                                        :background "cadet blue"))))
     (jde-java-font-lock-constant-face ((t (:foreground "white"))))
     (jde-java-font-lock-doc-tag-face ((t (:foreground "khaki"
                                           :background "cadet blue"))))
     (jde-java-font-lock-italic-face ((t (:slant italic :foreground "dark orchid"))))
     (jde-java-font-lock-link-face ((t (:background "cyan"))))
     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue4")))) ;%
     (jde-java-font-lock-number-face ((t (:foreground "khaki"))))
     (jde-java-font-lock-operator-face ((t (:background "cyan"))))
     (jde-java-font-lock-package-face ((t (:foreground "khaki"))))
     (jde-java-font-lock-pre-face ((t (:background "cyan"))))
     (jde-java-font-lock-underline-face ((t (:underline t :foreground "dark orchid"))))

     (makefile-shell-face ((t ())))
     (makefile-space-face ((t (:background "indian red" :foreground "white"))))
     (makefile-targets-face ((t ())))

     (match ((t (:background "DodgerBlue3" :foreground "light gray")))) ;%

     (menu ((t (:background "light gray" :foreground "gray4"))))

     (message-cited-text-face ((t (:foreground "indian red")))) ;%
     (message-cited-text ((t (:foreground "indian red")))) ;%

     (message-header-cc-face ((t (:foreground "medium sea green"))))
     (message-header-cc ((t (:foreground "medium sea green"))))
     (message-header-name-face ((t (:foreground "medium sea green"))))
     (message-header-name ((t (:foreground "medium sea green"))))
     (message-header-newsgroups-face ((t (:foreground "medium sea green"))))
     (message-header-newsgroups ((t (:foreground "medium sea green"))))
     (message-header-other-face ((t (:foreground "cadet blue"))))
     (message-header-other ((t (:foreground "cadet blue"))))
     (message-header-subject-face ((t (:height 1.2 :foreground "lemon chiffon"))))
     (message-header-subject ((t (:height 1.2 :foreground "lemon chiffon"))))
     (message-header-to-face ((t (:foreground "cadet blue"))))
     (message-header-to ((t (:foreground "cadet blue"))))
     (message-header-xheader-face ((t (:foreground "cadet blue"))))
     (message-header-xheader ((t (:foreground "cadet blue"))))

     (message-separator-face ((t (:background "indian red" :foreground "white" :bold t))))
     (message-separator ((t (:background "indian red" :foreground "white" :bold t))))

     (minibuffer-prompt ((t (:foreground "pale violet red"))))
     (mm-uu-extract ((t (:background "gray8"
                         :foreground "white"))))


     (mode-line ((t (:foreground "light gray" :background "dark slate blue" :inverse-video nil :box (:line-width 2 :style released-button)))))
     (modeline ((t (:foreground "light gray" :background "dark slate blue" :inverse-video nil :box (:line-width 2 :style released-button)))))
     (mode-line-inactive ((t (:foreground "light gray" :background "gray30"
                              :inverse-video nil :box (:line-width 2 :style released-button)))))
     (mode-line-highlight ((t (:foreground "black" :background "light sky blue"
                               :inverse-video nil :box (:line-width 2 :style released-button)))))
     (modeline-buffer-id ((t (:foreground "khaki" :background "dark slate blue" :inverse-video nil :box (:line-width 2 :style released-button)))))
     (modeline-mousable ((t (:foreground "khaki" :background "dark slate blue" :inverse-video nil :box (:line-width 2 :style released-button)))))
     (modeline-mousable-minor-mode ((t (:foreground "khaki" :background "dark slate blue" :inverse-video nil :box (:line-width 2 :style released-button)))))
     (mouse ((t (:foreground "khaki" :background "gray4"))))

     (multi-region-face ((t (:background "LightSteelBlue4" :foreground "white"))))

     ;; (nxml-heading-face ((t ())))
     ;; (nxml-outline-indicator-face ((t ())))
     ;; (nxml-outline-active-indicator-face ((t ())))
     ;; (nxml-outline-ellipsis-face ((t ())))
     ;; (nxml-delimited-data-face ((t ())))
     ;; (nxml-name-face ((t ())))
     ;; (nxml-ref-face ((t ())))
     ;; (nxml-delimiter-face ((t ())))
     (nxml-text-face ((t ())))
     (nxml-comment-content-face ((t (:foreground "dark orchid"))))
     (nxml-comment-delimiter-face ((t (:foreground "medium orchid"))))
     (nxml-processing-instruction-delimiter-face ((t ,hober2-insignificant-face)))
     (nxml-processing-instruction-target-face ((t (:foreground "steel blue"))))
     (nxml-processing-instruction-content-face ((t ())))
     (nxml-cdata-section-delimiter-face ((t ,hober2-insignificant-face)))
     (nxml-cdata-section-CDATA-face ((t (:foreground "cadet blue"))))
     (nxml-cdata-section-content-face ((t ())))
     (nxml-char-ref-number-face ((t (:foreground "IndianRed1"))))
     (nxml-char-ref-delimiter-face ((t (:foreground "IndianRed1"))))
     (nxml-entity-ref-name-face ((t (:foreground "IndianRed1"))))
     (nxml-entity-ref-delimiter-face ((t (:foreground "IndianRed1"))))
     (nxml-tag-delimiter-face ((t ,hober2-insignificant-face)))
     (nxml-tag-slash-face ((t ,hober2-insignificant-face)))
     (nxml-element-prefix-face ((t (:foreground "steel blue")))) ;%
     (nxml-element-colon-face ((t (:foreground "steel blue"))))
     (nxml-element-local-name-face ((t (:foreground "cadet blue"))))
     (nxml-attribute-prefix-face ((t (:foreground "sandy brown"))))
     (nxml-attribute-colon-face ((t (:foreground "sandy brown"))))
     (nxml-attribute-local-name-face ((t (:foreground "sandy brown"))))
     (nxml-namespace-attribute-xmlns-face ((t (:foreground "sandy brown"))))
     (nxml-namespace-attribute-colon-face ((t (:foreground "sandy brown"))))
     (nxml-namespace-attribute-prefix-face ((t (:foreground "sandy brown")))) ;%
     (nxml-attribute-value-face ((t (:foreground "lemon chiffon"))))
     (nxml-attribute-value-delimiter-face ((t (:foreground "lemon chiffon"))))
     ;; (nxml-namespace-attribute-value-face ((t ())))
     ;; (nxml-namespace-attribute-value-delimiter-face ((t ())))
     (nxml-prolog-literal-delimiter-face ((t (:foreground "lemon chiffon"))))
     (nxml-prolog-literal-content-face ((t (:foreground "lemon chiffon"))))
     (nxml-prolog-keyword-face ((t (:foreground "cadet blue"))))
     (nxml-markup-declaration-delimiter-face ((t ,hober2-insignificant-face)))
     ;; (nxml-hash-face ((t ())))
     ;; (nxml-glyph-face ((t ())))

     (paren-face ((t ,hober2-insignificant-face)))

     (primary-selection ((t (:background "dark slate blue" :foreground "white"))))
     (svn-status-marked-face ((t ())))
     (svn-status-marked-popup-face ((t ())))
     (svn-status-update-available-face ((t ())))
     (svn-status-directory-face ((t (:foreground "khaki"))))
     (svn-status-filename-face ((t (:foreground "cadet blue"))))
     (svn-status-locked-face ((t ())))
     (svn-status-switched-face ((t ())))

     (rcirc-my-nick ((t (:foreground "light sea green"))))
     (rcirc-mode-line-nick ((t (:foreground "medium orchid" :bold t))))
     (rcirc-other-nick ((t (:foreground "cadet blue"))))
     (rcirc-nick-in-message ((t (:foreground "pale violet red" :bold t))))
     (rcirc-server ((t (:foreground "steel blue"))))
     (rcirc-prompt ((t (:foreground "pale violet red")))) ;%

     (region ((t (:background "dark slate blue" :foreground "white"))))
     (rng-error-face ((t ,(if (hober2-use-underline-p)
                              '(:underline "red")
                            '(:background "indian red" :foreground "white" :bold t)))))


     (scroll-bar ((t (:background "light gray" :foreground "gray4"))))

     (secondary-selection ((t (:background "LightSteelBlue4" :foreground "white"))))

     (sgml-namespace-face ((t (:foreground "steel blue")))) ;%

     (shadow ((t ,hober2-insignificant-face)))
     (sh-heredoc-face ((t (:foreground "khaki"))))
     (sh-quoted-exec ((t (:foreground "khaki"))))

     (show-paren-match-face ((t (:background "cadet blue" :foreground "gray4"))))
     (show-paren-mismatch-face ((t (:background "indian red" :foreground "white" :bold t))))

     (show-tabs-space-face ((t (:background "indian red" :foreground "white"))))
     (show-tabs-tab-face ((t (:background "indian red" :foreground "white"))))

     (slime-error-face ((t (:background "indian red" :foreground "white" :bold t))))
     (slime-warning-face ((t (:background "indian red" :foreground "white"))))
     (slime-style-warning-face ((t (:background "indian red" :foreground "white"))))
     (slime-note-face ((t (:background "indian red" :foreground "white"))))
     (slime-highlight-face ((t (:background "indian red" :foreground "white"))))

     (sldb-catch-tag-face ((t (:foreground "khaki")))) ;%
     (sldb-condition-face ((t (:foreground "light sea green"))))
     (sldb-detailed-frame-line-face ((t ())))
     (sldb-frame-label-face ((t (:foreground "dark orchid"))))
     (sldb-frame-line-face ((t ())))
     (sldb-local-name-face ((t (:foreground "IndianRed1"))))
     (sldb-local-value-face ((t (:foreground "lemon chiffon"))))
     (sldb-reference-face ((t (:background "cyan"))))
     (sldb-restart-type-face ((t (:foreground "medium orchid"))))
     (sldb-restart-face ((t (:foreground "pale violet red"))))
     (sldb-restart-number-face ((t (:foreground "dark orchid"))))
     (sldb-section-face ((t (:foreground "sandy brown"))))
     (sldb-topline-face ((t (:foreground "IndianRed1"))))

     (slime-repl-prompt-face ((t (:foreground "pale violet red"))))
     (slime-repl-output-face ((t (:foreground "dark orchid"))))
     (slime-repl-input-face ((t (:foreground "pale violet red"))))
     (slime-repl-result-face ((t (:foreground "medium orchid"))))

     (slime-inspector-topline-face ((t (:background "gray30" :foreground "white"))))
     (slime-inspector-label-face ((t (:foreground "indian red")))) ;%
     (slime-inspector-value-face ((t (:foreground "light pink"))))
     (slime-inspector-action-face ((t (:background "cyan"))))
     (slime-inspector-type-face ((t (:foreground "light sea green")))) ;%

     (slime-reader-conditional-face ((t ,hober2-insignificant-face)))

     (speedbar-button-face ((t (:foreground "medium sea green"))))
     (speedbar-directory-face ((t (:foreground "khaki"))))
     (speedbar-file-face ((t (:foreground "cadet blue"))))
     (speedbar-highlight-face ((t (:background "sea green" :foreground "light gray" :underline t))))
     (speedbar-selected-face ((t (:background "cadet blue" :foreground "gray4")))) ;%
     (speedbar-tag-face ((t (:foreground "khaki"))))

     (subscript ((t ())))
     (superscript ((t ())))
     (tex-math-face ((t (:foreground "sandy brown"))))
     (tex-math ((t (:foreground "sandy brown"))))
     (tex-verbatim-face ((t ())))
     (tex-verbatim ((t ())))

     (tool-bar ((t (:background "light gray"
                    :foreground "gray4"
                    :inverse-video nil
                    :underline t))))

     (trailing-whitespace ((t (:background "indian red" :foreground "white"))))

     (underline ((t (:underline t))))

     (viper-search-face ((t (:background "steel blue" :foreground "lemon chiffon"))))
     (viper-replace-overlay-face ((t (:foreground "gray4" :bold t
                                      :background "light pink"))))
     (viper-minibuffer-emacs-face ((t (:foreground "gray4"
                                       :background "DarkSeaGreen2"))))
     (viper-minibuffer-insert-face ((t (:foreground "gray4" ;%
                                        :background "light pink"))))
     (viper-minibuffer-vi-face ((t (:foreground "gray4" ;%
                                    :background "light sea green"))))

     (w3m-anchor-face ((t (:foreground "light sea green" :underline t)))) ;%
     (w3m-arrived-anchor-face ((t (:foreground "cadet blue" :underline t))))
     (w3m-bitmap-image-face ((t (:foreground "gray4"
                                 :background "white"))))
     (w3m-bold-face ((t (:foreground "medium sea green"))))
     (w3m-current-anchor-face ((t (:foreground "light sea green" :bold t
                                   :underline t))))
     (w3m-form-button-face ((t (:foreground "gray4" :background "light gray" :bold t :underline t))))
     (w3m-form-button-mouse-face ((t (:background "sea green" :foreground "light gray" :underline t))))
     (w3m-form-button-pressed-face ((t (:foreground "gray4" :background "light gray" :bold t :underline t))))
     (w3m-form-face ((t (:foreground "gray4" :background "light gray" :bold t))))
     (w3m-header-line-location-title-face ((t (:foreground "cadet blue")))) ;%
     (w3m-header-line-location-content-face ((t (:foreground "khaki"))))
     (w3m-history-current-url-face ((t (:foreground "lemon chiffon"))))
     (w3m-image-face ((t (:foreground "DarkSeaGreen2" :bold t))))
     (w3m-link-numbering-face ((t (:foreground "medium sea green" :bold t)))) ;%
     (w3m-strike-through-face ((t (:strike-through t :strikethru t))))
     (w3m-underline-face ((t (:underline t))))

     (whitespace-highlight-face ((t (:background "indian red" :foreground "white"))))
     (widget-button-face ((t (:bold t :underline t))))
     (widget-button-pressed-face ((t (:bold t :underline t))))
     (widget-documentation-face ((t (:foreground "medium sea green")))) ;%
     (widget-field-face ((t (:background "gray30" :foreground "white"))))
     (widget-inactive-face ((t ,hober2-insignificant-face)))
     (widget-single-line-field-face ((t (:background "gray30" :foreground "white"))))

     (woman-italic-face ((t (:slant italic))))
     (woman-bold-face ((t (:bold t))))
     (woman-unknwon-face ((t (:foreground "cadet blue")))) ;%
     (woman-addition-face ((t (:foreground "medium sea green"))))

     (zmacs-region ((t (:background "dark slate blue" :foreground "white")))))))

(provide 'color-theme-hober2)

;;; color-theme-hober2.el ends here
