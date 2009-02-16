;; eev-ttp.el: support for storing hyperlinks targets inside text properties.
;; Copyright (C) 2005 Free Software Foundation, Inc.
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Author:     Eduardo Ochs <edrx@mat.puc-rio.br>
;; Maintainer: Eduardo Ochs <edrx@mat.puc-rio.br>
;; Version:    2005may20
;; Keywords:   e-scripts, hyperlinks, hypertext

;;; Commentary:

;; Danger! Danger! This is a piece of VERY IMMATURE EXPERIMENTAL
;; SOFTWARE! I'm only including it in the eev package because it has
;; some interesting ideas that I would like to make public and to
;; discuss with some of the people from the mailing list!

;; The main idea, short form: there are several packages for hypertext
;; for emacs around - planner, howm, what else? - and each one of them
;; offers a few kinds of hyperlinks, which implement a limited class
;; of "actions" that are performed when we "follow" these
;; links... actually these packages also implement "lisp://" or
;; "%lisp%" hyperlinks that can hold arbitrary Lisp code, and so links
;; can do anything; but what happens if we try a simpler solution, in
;; which we implement only one kind of hyperlink, whose action is to
;; evaluate a piece of Lisp? Also, what happens if the same process
;; that associates some Lisp code to a region of text can also
;; associate arbitrary text properties? Think on an enriched mode on
;; steroids, with tools for making parts of the text "active"...

;; The main idea, long form: text with arbitrary text properties can
;; be stored in files if the properties are converted to an "unpacked"
;; form; files in a certain format where the text properties appear
;; textually and side-by-side with the text that they apply to can be
;; "packed" into Emacs's notion of "text with properties". An example:
;;
;;                                  unpack         
;;                                 <------
;;   ^Asome text^B:file "~/foo"^C  ------>   some text
;;                                  pack   (ttp-src ":file \"~/foo\""
;;                                          ttp-action (find-fline "~/foo")
;;                                        ttp-message "(find-fline \"~/foo\")"
;;                                        category ttp-link)
;;
;; I'm writing "^A", "^B" and "^C" for what are actually "\^A", "\^B"
;; and "\^C" respectively, i.e., chars 1, 2, 3.
;;
;; Note the property "(category ttp-link)" in the "some text" above; by
;; doing
;;
;;   (setplist 'ttp-link '(face ...))
;;
;; we can make it display in another face; there are also some overlay
;; tricks that I'm not going to describe now (the code is not ready!)
;; that let us use the packed thing as a hyperlink.

;; About the data structures: internally we deal with "^A...^B...^C"s
;; chunks in a buffer by passing around 6-uples of the form (SA EA SB
;; EB SC EC) that tell the starting and ending positions of the ^A, ^B
;; and ^C; I call these structures "seabc"s in the code. Similarly, a
;; "packed chunk" is represented by a pair (S E). The (slightly
;; outdated) chart below shows some of the main conversions that
;; appear in the code:
;;
;;     sa,ea,sb,eb,sc,ec <-- sa <-- point
;;     :        |   ^
;;     :        v   :                            | ^
;;     :    propstr-+                            | |
;;     :      |   ^ :                     packing| |unpacking
;;     :      v   | :                            | |
;;     : proplist | :                            v |
;;     :   :      | :                             
;;     :   :      | :
;;     \...+....> s,e <---- s <---- point

;; The code is divided into four main sections: (1) packing, (2)
;; unpacking, (3) overlays (which are used for highlighting packed
;; hyperlinks, displaying their targets in the echo area, and using a
;; special keymap when point is over a packed hyperlink), and (4)
;; ttp-mode, which activates overlays and displays targets.

;; As the code is still very messy there's a section (5) too, which
;; holds the "rest".

;; Problems: no way to pack and unpack many "links" at once (coming
;; soon!) - save a buffer with packed links while they are packed and
;; you lose the information about their properties - the message that
;; shows the target of the link is always displayed, ideally it should
;; only be displayed when it will not obscure important messages - the
;; packer is still very dumb, it is tuned to produce only hyperlinks
;; (not arbitrary text properties; but this will be fixed soon).

;; (find-eCvariable 'post-command-hook)
;; (find-eCvariable 'echo-area-clear-hook)
;; (find-ecvssrcfile "keyboard.c" "\ncommand_loop_1" "Clear the echo area.")
;; (find-eetcfile "NEWS" "echo-area-clear-hook")



;;;;;
;;
;; Section 1: packing, i.e., working on `seabc's, and converting
;; "^Atext^Bpropstr^C"s to "text"s (with properties).
;;
;;;;;

(defun ttp-point-to-sa ()
  "If point is under a ^A then return point, otherwise search for the
closest ^A before point and return its position (error if not found)."
  (if (eq (char-after) ?\^A) (point)
    (save-excursion (search-backward "\^A") (point))))

(defun ttp-sa-to-seabc (sa)
  "If SA points to the beginning of an expression like
\"^Aoriginaltext^Bpropstr^C\" then return (SA EA SB EB SC EC);
otherwise return nil."
  (save-excursion
    (goto-char sa)
    (if (looking-at "\\(\^A\\)[^\^A\^B\^C]*\\(\^B\\)[^\^A\^B\^C]*\\(\^C\\)")
	(list (match-beginning 1) (match-end 1)
	      (match-beginning 2) (match-end 2)
	      (match-beginning 3) (match-end 3)))))

(defun ttp-seabc-to-propstr (seabc)
  "SEABC is a list (SA EA SB EB SC EC); return the text between EB and SC."
  (buffer-substring-no-properties (nth 3 seabc) (nth 4 seabc)))

(defun ttp-propstr-to-proplist (propstr)
  "This function is very dumb now - just a `read'. That should change soon."
  (read propstr))

(defun ttp-seabc-proplist-pack (seabc proplist)
  "SEABC is a list (SA EA SB EB SC EC) pointing to a text like
\"^Aoriginaltext^Bpropstr^C\" and PROPLIST is a plist; keep just
\"originaltext\", setting its properties to proplist."
  (delete-region (nth 2 seabc) (nth 5 seabc))
  (set-text-properties (nth 1 seabc) (nth 2 seabc) proplist)
  (delete-region (nth 0 seabc) (nth 1 seabc)))


;;;;;
;;
;; Section 2: unpacking, i.e., converting `se's to `seabc's, and
;; "text"s to "^Atext^Bpropstr^C"s:
;;
;;;;;

(defun ttp-point-to-s ()
  "A stubbish function - this is no real good."
  (let* ((pos (1+ (point)))
	 (s (previous-single-property-change pos 'ttp-src)))
    s))

(defun ttp-s-to-se (s)
  "Convert S to a structure (S E), indicating a range with constant
ttp-src property. If E would be equal to S then return nil instead."
  (let ((e (next-single-property-change s 'ttp-src)))
    (if e (list s e)
      (if (> (point-max) s) (list s (point-max))))))

(defun ttp-se-to-propstr (se)
  "SE is a pair (S E); return the text property ttp-src at S." 
  (get-text-property (car se) 'ttp-src))

(defun ttp-se-propstr-unpack (se propstr)
  "SE is a pair (S E) and PROPSTR is a string; insert stuff before S and
after E such that \"originaltext\" becomes \"^Aoriginaltext^Bpropstr^C\".
This function removes all text properties on originaltext."
  (save-excursion
    (goto-char (cadr se))
    (insert-before-markers "\^B" propstr "\^C"))
  (set-text-properties (car se) (cadr se) nil)
  (save-excursion
    (goto-char (car se))
    (insert-before-markers "\^A")))



;;;;;
;;
;; Section 3: overlays
;;
;;;;;

(defvar ttp-overlay nil
  "The one (global) overlay used for the active link.")
(defvar ttp-active-keymap nil "The keymap for active links.")
(defvar ttp-active-plist  nil "The overlay property list for active links.")

(defun ttp-overlay-destroy ()
  "Destroy the current ttp-overlay."
  (when ttp-overlay (delete-overlay ttp-overlay) (setq ttp-overlay nil)))

(defun ttp-overlay-create (se plist messagefmt &rest args)
  "Destroy the current ttp-overlay, create a new one, and issue a message."
  (ttp-overlay-destroy)
  (setq ttp-overlay (make-overlay (car se) (cadr se)))
  (while plist
    (overlay-put ttp-overlay (car plist) (cadr plist))
    (setq plist (cddr plist)))
  (let ((message-log-max nil)
	(message-truncate-lines nil))
    (apply 'message messagefmt args)))

;; (if ttp-active-keymap () ...)
(setq ttp-active-keymap (make-sparse-keymap))
(define-key ttp-active-keymap "\M-e" 'ttp-do-action)

(setq ttp-active-plist `(face (:underline t) keymap ,ttp-active-keymap))

;; A test:
(defun ttp-foo () (interactive)
  (ttp-overlay-create (list (point) (mark)) ttp-active-plist "-> ttp-foo..."))


;; (defun ttp-overlay-create-from-se (se)
;; 
;; (defun ttp-overlay-update ()
;;   (ttp-overlay-destroy)
;;   (if (text-prop-ttp-src)
;;       (let ((se ttp-point-to-se)
;; 	    (

;; Not being used yet:
;;
(defun ttp-set-category (symbol plist)
  (set symbol plist)
  (setplist symbol plist))

(ttp-set-category 'ttp-active
		  `(face (:underline t)
                    keymap ,ttp-active-keymap))





;;;;;
;;
;; Section 4: ttp-mode
;;
;;;;;

(defvar ttp-overlay-mode-map nil)
(setq ttp-overlay-mode-map (make-sparse-keymap))
;; (define-key ttp-overlay-mode-map "\M-t"  'ttp-overlay-mode-update)
(define-key ttp-overlay-mode-map "\C-cp"    'ttp-pack)
(define-key ttp-overlay-mode-map "\C-c\C-p" 'ttp-pack)
(define-key ttp-overlay-mode-map "\C-cu"    'ttp-unpack)
(define-key ttp-overlay-mode-map "\C-c\C-u" 'ttp-unpack)

(define-minor-mode ttp-overlay-mode
  "Toggles ttp link tracking and ttp link packing/unpacking keys."
  :init-value t :global t)

(defun ttp-on-link-p (&optional pos)
  "Tells if POS (or point) is on a ttp link.
This function just returns the value of the ttp-src property at POS."
  (get-text-property (or pos (point)) 'ttp-src))

(defun ttp-overlay-mode-update ()
  "Update the ttp overlay and the link message shown in the echo area.
If there's no link at point then just make sure there's no ttp-overlay."
  (interactive)
  (let ((ttp-src (ttp-on-link-p)))
    (if ttp-src
	(ttp-overlay-create (ttp-s-to-se (ttp-point-to-s))
			    ttp-active-plist
			    "-> %s" ttp-src)
      (ttp-overlay-destroy))))

(defun ttp-overlay-mode-update-maybe ()
  "This function is run at post-command-hook.
If ttp-overlay-mode is off it is a no-op; overlays (and messages) are only
updated when ttp-overlay-mode is on."
  (if ttp-overlay-mode (ttp-overlay-mode-update)))



;;;;;
;;
;; Misc things
;;
;;;;;

(defun ttp-propstr-to-proplist-1 (propstr)
  "Convert a propstr from a ^Atext^Bpropstr^C to a plist (in a certain way)"
  (let* ((lst (read propstr)))		; was (concat "(" propstr "\n)")
    `(face (:foreground "green")
      ttp-action ,lst
      ttp-src ,propstr)))

(defun ttp-point-to-seabc ()
  "Return a seabc for the ^A...^B...^C around point, aborts if there is none."
  (let* ((sa (ttp-point-to-sa))
	 (seabc (ttp-sa-to-seabc sa)))
    (if (not seabc)
	(error "No ^A...^B...^C structure around or before point"))
    (if (<= (nth 5 seabc) (point))
	(error "No ^A...^B...^C structure around point"))
    seabc))

(defun ttp-pack ()
  "Pack the ^A...^B...^C around point, aborts if there is none."
  (interactive)
  (let* ((seabc (ttp-point-to-seabc))
	 (propstr (ttp-seabc-to-propstr seabc))
	 (proplist (ttp-propstr-to-proplist-1 propstr)))
    (ttp-seabc-proplist-pack seabc proplist)))

(defun ttp-unpack ()
  "Unpack the ttp link around point into an ^A...^B...^C."
  (interactive)
  (let ((propstr (get-text-property (point) 'ttp-src)))
    (if (not propstr)
	(error "Point is not on a ttp link"))
    (ttp-se-propstr-unpack (ttp-s-to-se (ttp-point-to-s)) propstr)))

(defun ttp-do-action ()
  "Execute (with eval) with ttp-action property of the char at point."
  (interactive)
  (eval (get-text-property (point) 'ttp-action)))

;; An early test: use propertize to make a string with ttp-src message
;; and a colored face.
' (insert
   "\n;; ab"
   (propertize "0123456"
	       'face '(:foreground "orange")
	       'ttp-src "Foo!")
   "cdef")

;; For taking a screenshot...
;; (eev "emacs-cvs ~/elisp/eev-ttp.el -geometry 36x16 &")

;; For debugging and convenience...
;; (find-efunctiondescr 'edmacro-mode)
;; (add-hook    'post-command-hook 'ttp-overlay-mode-update-maybe)
;; (remove-hook 'post-command-hook 'ttp-overlay-mode-update-maybe)
;; (add-hook    'post-command-hook 'ttp-overlay-mode-update-maybe nil 'local)
;; (remove-hook 'post-command-hook 'ttp-overlay-mode-update-maybe 'local)
;; (setq debug-on-error t)
;; (setq debug-on-error nil)

;; The "real" test. Run these things to understand what this is all about.
;; A screenshot (of the demo below):
;; (find-eimage0 "doc/shot-eev-ttp.png" 1 3 t)
' (load-file (buffer-file-name))
' (add-hook    'post-command-hook 'ttp-overlay-mode-update-maybe)
' (eesteps '((find-estring
	      (concat " \^Afstab\^B(find-fline \"/etc/fstab\")\^C\n"
		      " \^Aetc\^B(find-fline \"/etc/\")\^C\n"))
	     "<right>" "<<ttp-pack>>"
	     "<down>"  "<<ttp-pack>>"
	     "<up>" "<right>"
	     "<<ttp-do-action>>""<<kill-this-buffer>>"
	     "<<ttp-unpack>>"
	     ))



;; Local Variables:
;; coding:          raw-text-unix
;; modes:           (fundamental-mode emacs-lisp-mode)
;; End:
