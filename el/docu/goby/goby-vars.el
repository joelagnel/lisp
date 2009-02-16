;; Goby: goby-vars.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug  9, 2003

;;; Commentary:

;; Home page: http://www.mew.org/~kazu/proj/goby/

;;; Code:

(defvar goby-home-page "http://www.mew.org/~kazu/proj/goby/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hooks
;;;

(defvar goby-decorate-initial-frame-hook nil
  "*A hook to be called when a Goby frame is created.")
(defvar goby-view-mode-enter-hook nil
  "*A hook to be called when entering View mode.")
(defvar goby-view-mode-enter-hook2 nil
  "*A hook to be called when entering View mode.")
(defvar goby-view-mode-exit-hook nil
  "*A hook to be called when exiting View mode.")
(defvar goby-view-mode-exit-hook2 nil
  "*A hook to be called when exiting View mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defgroup goby nil
  "Support for large TrueType fonts and images"
  :prefix "goby-"
  :group 'editing)

(defcustom goby-major-mode 'text-mode
  "*Major mode for the Goby edit mode."
  :type 'symbol
  :group 'goby)

(defcustom goby-minor-mode-prefix "\C-c;"
  "*Prefix key to use for the Goby minor mode"
  :type 'string
  :group 'goby)

(easy-mmode-defmap goby-edit-mode-map
 '(("f" . goby-face-next-font-region)
   ("r" . goby-face-next-color-region)
   ("p" . goby-face-increase-ratio-region)
   ("n" . goby-face-decrease-ratio-region)
   ("m" . goby-face-math-region)
   ("^" . goby-face-math-power-region)
   ("_" . goby-face-math-aux-region)
   ("\C-p" . goby-face-math-raise-region)
   ("\C-n" . goby-face-math-lower-region)
   ("y" . goby-highlight-region)
   ("2" . goby-face-math-1/2)
   ("3" . goby-face-math-3/4)
   ("4" . goby-face-math-1/4)
   ("." . goby-face-math-dot)
   ("x" . goby-face-math-times)
   ("/" . goby-face-math-divide)
   ("i" . goby-insert-image)
   ("s" . goby-change-scale)
   ("c" . goby-center-line)
   ("-" . goby-insert-bar)
   ("*" . goby-insert-item)
   ("\"" . goby-insert-pause)
   ("I" . goby-insert-item-region)
   ("v" . goby-view-mode)
   ("@" . goby-dump-screen)
   ("#" . goby-make-ps)
   ("l" . goby-insert-newpage)
   ("\C-l" . goby-top-line))
 "Keymap for Goby edit mode")

(easy-mmode-defmap goby-mode-map
 `((,goby-minor-mode-prefix . ,goby-edit-mode-map))
 "Keymap for Goby minor mode")

(defvar goby-frame  "*goby*")
(defvar goby-buffer "*goby scratch*")

(defvar goby-helvetica  "helvetica")
(defvar goby-times      "times")
(defvar goby-courier    "courier")
(defvar goby-gothic     "gothic")
(defvar goby-mincho     "mincho")
(defvar goby-math       "italic")

(defvar goby-tab-width 5)

(defvar goby-window-manager-top-position 0)
(defvar goby-window-manager-left-position 0)

(defvar goby-window-manager-view-top-position -22)
(defvar goby-window-manager-view-left-position 0)

(defvar goby-window-manager-bottom-margin 5) ;; # of line
(defvar goby-window-manager-bottom-search-margin 3) ;; # of line

(defvar goby-theme 'light)

;; to pacify the byte-compiler
(defmacro goby-def-theme (dark light)
  `(if (eq goby-theme 'dark) ,dark ,light))

(defvar goby-foreground-color          (goby-def-theme "white" "black"))
(defvar goby-background-color          (goby-def-theme "black" "white"))
(defvar goby-cursor-color              (goby-def-theme "white" "black"))
(defvar goby-pointer-color             (goby-def-theme "white" "black"))
(defvar goby-view-pointer-color        (goby-def-theme "gray75" "gray25"))
(defvar goby-view-bar-color            (goby-def-theme "gray75" "gray25"))
(defvar goby-view-pause-color          (goby-def-theme "gray35" "gray65"))
(defvar goby-fringe-foreground-color   (goby-def-theme "black" "white"))
(defvar goby-fringe-background-color   (goby-def-theme "black" "white"))
(defvar goby-modeline-foreground-color (goby-def-theme "black" "white"))
(defvar goby-modeline-background-color (goby-def-theme "black" "white"))

(defvar goby-view-mouse-face-foreground-color
  (goby-def-theme "#ccaa00" "#003355")) ;; 553300
(defvar goby-view-mouse-face-background-color   
  (goby-def-theme "#3355ff" "#ffccaa")) ;;aaccff

(defface goby-view-mouse
  `((t (:foreground ,goby-view-mouse-face-foreground-color
	:background ,goby-view-mouse-face-background-color)))
  "Mouse face for Goby view mode"
  :group 'faces)

(defvar goby-tab-spec
  (goby-def-theme
   '((0 6 "gray75"  "gray50")
     (1 5 "#ffbf7f" "#ff8f7f")
     (2 4 "#ff9f7f" "#ff6f7f")
     (3 3 "#ff7f7f" "#ff4f7f"))
   '((0 6 "gray25"  "gray50")
     (1 5 "#004080" "#007080")
     (2 4 "#006080" "#009080")
     (3 3 "#008080" "#00b080"))))

(defun goby-get-tab-spec (num)
  (or (assq num goby-tab-spec)
      (nth (1- (length goby-tab-spec)) goby-tab-spec)))
(defun goby-get-tab-num   (ent) (nth 0 ent))
(defun goby-get-tab-ratio (ent) (nth 1 ent))
(defun goby-get-tab-color (ent) (nth 2 ent))
(defun goby-get-tab-tbclr (ent) (nth 3 ent))

(defvar goby-properties-string "[properties]")
(defvar goby-centering-string "[]")
(defvar goby-bar-string "[-]")
(defvar goby-item-string "[*]")
(defvar goby-pause-string "[#]")
(defvar goby-image-format  "[image \"%s\"]")
(defvar goby-image-format2 "[image \"%s\" %s]")
(defvar goby-image-regex "\\[image \"\\([^\"]+\\)\" ?\\([^]]+\\)?\\]")

(defvar goby-image-spec
  '(("\\.pnm$" t)
    ("\\.jpe?g$" "jpegtopnm")
    ("\\.tif?f$" "tifftopnm")
    ("\\.png$"   "pngtopnm")
    ("\\.gif$"   "giftopnm")
    ("\\.xbm$"   "xbmtopnm")
    ("\\.xpm$"   "xpmtopnm")
    ("\\.ps$"    "pstopnm")))

(defun goby-get-image-suffix  (ent) (nth 0 ent))
(defun goby-get-image-program (ent) (nth 1 ent))

(defvar goby-view-pointer-shape 2)

(defvar goby-item-ratio 40)

(defvar goby-bar-size-pixel-margin 32)

(defvar goby-use-bold t)

(defvar goby-mode-lighter " Goby")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View mode
;;;

(defcustom goby-use-advanced-window-manager nil
  "*If non-nil, View mode uses the advanced protocol of X window system.
Set this to non-nil when you are using XDE or Gnome."
  :type 'boolean
  :group 'goby)

(defvar goby-view-mode-name "View")

(defvar goby-view-mode-map nil
  "*Keymap for Goby view mode")

(unless goby-view-mode-map
  (setq goby-view-mode-map (make-sparse-keymap))
  (define-key goby-view-mode-map " "         'goby-next-page)
  (define-key goby-view-mode-map "n"         'goby-next-page)
  (define-key goby-view-mode-map "p"         'goby-prev-page)
  (define-key goby-view-mode-map [down]      'goby-next-page)
  (define-key goby-view-mode-map [up]        'goby-prev-page)
  (define-key goby-view-mode-map [mouse-1]   'goby-next-page)
  (define-key goby-view-mode-map [mouse-3]   'goby-prev-page)
  (define-key goby-view-mode-map [delete]    'goby-prev-page)
  (define-key goby-view-mode-map [backspace] 'goby-prev-page)
  (define-key goby-view-mode-map "<"         'goby-first-page)
  (define-key goby-view-mode-map ">"         'goby-last-page)
  (cond
   ((featurep 'xemacs)
    (define-key goby-view-mode-map '(shift button2) 'browse-url-at-mouse))
   (t
    (define-key goby-view-mode-map [S-mouse-2] 'browse-url-at-mouse)))
  (define-key goby-view-mode-map "i"         'iconify-frame)
  (define-key goby-view-mode-map "q"         'goby-view-quit)
  (define-key goby-view-mode-map "\C-s"      'goby-isearch-forward)
  (define-key goby-view-mode-map "\C-r"      'goby-isearch-backward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PostScript
;;;

(defvar goby-ps-use-bold nil)

(defvar goby-ps-font-alist
  `((,goby-gothic    "FG")
    (,goby-mincho    "FM")
    (,goby-helvetica "FH")
    (,goby-courier   "FC")
    (,goby-times     "FT")
    (,goby-math      "FI")))

(defvar goby-ps-frame 2) ;; point

(defvar goby-ps-item-gray-scale 0.4)

(defvar goby-ps-bar-gray-scale 0.4)
(defvar goby-ps-bar-height 2) ;; point

(defvar goby-ps-gap-magic-number 2) ;; point
(defvar goby-ps-left-fringe-pixel-magic-number 16)
(defvar goby-ps-tab-width goby-tab-width)

(defvar goby-ps-epsfile-suffix ".eps")

;;
;;   +---+
;;   |   |
;;   +---+
;;<a><-b->
;;<---c---->
(defvar goby-ps-item-base-ratio 20)  ;; a
(defvar goby-ps-item-ratio 40)       ;; b
(defvar goby-ps-item-width-ratio 60) ;; c

(defvar goby-ps-version "2.0") ;; xxx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Screen Dump
;;;

(defvar goby-dump-index-file "index.html")
(defvar goby-dump-html-file  "%03d.html")
(defvar goby-dump-large-file "%03d.png")
(defvar goby-dump-small-file "%03ds.png")

(defvar goby-dump-small-width  256)
(defvar goby-dump-small-height 192)
(defvar goby-dump-large-width  640)
(defvar goby-dump-large-height 480)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programs
;;;

(defvar goby-prog-ppmtopgm  "ppmtopgm")
(defvar goby-prog-pnmscale  "pnmscale")
(defvar goby-prog-pnmtojpeg "pnmtojpeg")
(defvar goby-prog-pnmtopng  "pnmtopng")
(defvar goby-prog-pnmtops   "pnmtops")

(cond
 ((eq system-type 'darwin)
  (defvar goby-prog-capture "screencapture") 
  (defvar goby-prog-capture-args 'goby-prog-capture-args-for-darwin))
 (t
  (defvar goby-prog-capture "import")
  (defvar goby-prog-capture-args 'goby-prog-capture-args-for-unix)))

(provide 'goby-vars)

;;; Copyright Notice:

;; Copyright (C) 2003 Kazu Yamamoto
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
;; 3. Neither the name of the author nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; goby-vars.el ends here
