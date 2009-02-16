;; htmlize-eev.el - htmlize files containing glyphs and elisp hyperlinks.

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.
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
;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    2006nov13
;; Keywords:   e-scripts, glyphs, html, hyperlinks

;;; Commentary:

;; WARNING! WARNING! This file was put up in a hurry to generate the
;; html files in the eev package - it is not intended for public
;; consumption (yet!) and it may depend on functions that you don't
;; have because they are only in my .emacs, and on features that are
;; only on CVS GNU Emacs.

;; (find-angg ".emacs" "emacs-ascii-screenshots")

;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
(require 'htmlize)
(require 'jit-lock)


;; This is just for eev-ttp.el, I think...
;; (find-angg ".emacs" "modes-safe-local")
;;
(put 'modes 'safe-local-variable 'listp)




;;;;;
;;
;; Some basic tools
;;
;;;;;

(defun ee-kill-buffer (buffer-or-name &rest rest)
  "For each BUFFER-OR-NAME in the list of arguments kills it if it exists"
  (if (get-buffer buffer-or-name) (kill-buffer buffer-or-name))
  (if rest (apply 'ee-kill-buffer rest)))

(defun my-replace-regexps-with-inheritance (regexp to &rest rest)
  (save-excursion
    (while (re-search-forward regexp nil 'no-error)
      (replace-match to 'fixedcase nil)))
  (if rest (apply 'my-replace-regexps-with-inheritance rest)))

(defun my-replace-strings-with-inheritance (from to &rest rest)
  (save-excursion
    (while (search-forward from nil 'no-error)
      (delete-region (match-beginning 0) (match-end 0))
      (insert-and-inherit to))))

(defun my-replace-strings (from to &rest rest)
  (save-excursion
    (while (search-forward from nil 'no-error)
      (delete-region (match-beginning 0) (match-end 0))
      (insert to))))



;;;        _             _         
;;;   __ _| |_   _ _ __ | |__  ___ 
;;;  / _` | | | | | '_ \| '_ \/ __|
;;; | (_| | | |_| | |_) | | | \__ \
;;;  \__, |_|\__, | .__/|_| |_|___/
;;;  |___/   |___/|_|              
;;;
;;; Dealing with glyphs - for example, converting ^Os to (real) red stars

(defun ee-faceid-to-face (faceid facelist)
  (if facelist
      (if (= faceid (face-id (car facelist)))
	  (car facelist)
	(ee-faceid-to-face faceid (cdr facelist)))))

(defun glyphs-faceglyph-to-facechar (code)
  "Convert a character associated to a glyph to a propertized string.
CODE is an integer - the code of the character (e.g. 15 for ^O)."
  (let* ((n (aref (aref standard-display-table code) 0))
	 (faceid (ash n -19))
	 (face (ee-faceid-to-face faceid (face-list)))
	 (char (logand n 524287))
	 (charstr (format "%c" char)))
    (add-text-properties 0 1 (list 'face face) charstr)
    charstr))

(defun my-replace-glyphs (&optional glyphsstr)
  (or glyphsstr (setq glyphsstr "\f›«»")) ; nil means use the default list
  (if (> (length glyphsstr) 0)
      (let* ((code      (aref glyphsstr 0))
	     (firstchar (substring glyphsstr 0 1))
	     (rest      (substring glyphsstr 1 nil))
	     (unglyphstr (glyphs-faceglyph-to-facechar code)))
	(my-replace-strings firstchar unglyphstr)
	(my-replace-glyphs  (substring glyphsstr 1 nil)))))




;;;                        _ _       _        
;;;  ___  _____  ___ __   | (_)_ __ | | _____ 
;;; / __|/ _ \ \/ / '_ \  | | | '_ \| |/ / __|
;;; \__ \  __/>  <| |_) | | | | | | |   <\__ \
;;; |___/\___/_/\_\ .__/  |_|_|_| |_|_|\_\___/
;;;               |_|                         
;;; htmlizing elisp hyperlinks

;; variables used by the functions that htmlize elisp hyperlinks.
;; We use dynamic scoping here: sometimes we make them local with
;; `let' and then some subfunction will set them.

(defvar eeh-text   "(text bar)")	; overridden by `let's
(defvar eeh-sexp   '(sexp bar))		; overridden by `let's
(defvar eeh-target "http://url/bar")	; overridden by `let's
(defvar eeh-html   "<it>(foo bar not converted)</it>") ; same


;; modified hyperlink functions - these return either nil (meaning "do
;; not htmlize in any special way"), or a target for a html hyperlink;
;; for the really special cases - like inlined images - these
;; functions change the variable `eeh-html' and put the resulting html
;; there; when `eeh-html' is non-nil this overrides the target and all
;; the rest.

;; Hyperlinks to plain files and to files with anchors
;; (find-elnode "File Name Expansion" "Function: file-relative-name")

(defun eeh-find-anchor (fname &optional anchor &rest rest)
  (concat (file-relative-name fname)
	  ".html"
	  (if anchor (format "#%s" anchor) "")))

(defun eeh-find-eev (fname &optional anchor &rest rest)
  (eeh-find-anchor (ee-eevfile fname) anchor))
(defun eeh-find-eevex (fname &optional anchor &rest rest)
  (eeh-find-anchor (ee-eevexfile fname) anchor))
(defun eeh-find-angg (fname &optional anchor &rest rest)
  (eeh-find-anchor (ee-anggfile fname) anchor))

(defun eeh-find-eevfile (fname &rest rest)
  (file-relative-name (ee-eevfile fname)))
(defun eeh-find-eevexfile (fname &rest rest)
  (file-relative-name (ee-eevexfile fname)))
(defun eeh-find-anggfile (fname &rest rest)
  (file-relative-name (ee-anggfile fname)))

;; Hyperlinks to images
;; (find-eevfile "README.html")

(defun eeh-find-eimage0 (fname &optional nlines nchars perc &rest ignore)
"Htmlize hyperlinks to images. Supports inlining and scaling.
NLINES and NCHARS are ignored, PERC controls whether to inline or scale.
Examples (try with \\[eeh-1flash]):\n
  (find-eimage0 \"doc/shot-f3.png\")
    -> just a link to the image\n
  (find-eimage0 \"doc/shot-f3.png\" nil nil 'anything-not-nil-or-string)
    -> \"<img src=\"doc/shot-f3.png\" border=0>\"\n
  (find-eimage0 \"doc/shot-f3.png\" nil nil \"40%\")
    -> <a href=\"doc/shot-f3.png\">
       <img src=\"doc/shot-f3.png\" width=\"40%\" height=\"40%\" border=0>
       </a>"
  (cond ((null perc) fname)		; nil:   no inline image, just the link
	((stringp perc)			; "nn%": inline a miniature,
	 (setq eeh-html			;        link to full image
	       (format (concat
			"<a href=\"%s\">"
			"<img src=\"%s\" width=\"%s\" height=\"%s\" border=0>"
			"</a>")
		       fname fname perc perc)))
	(t (setq eeh-html (format "<img src=\"%s\" border=0>" fname)))))

;; Hyperlinks to info nodes

(defun eeh-info-dash (url node)
  (concat url (replace-regexp-in-string "[ &<>/]" "-" node) ".html"))

(defun eeh-info (manual-and-node)
  "Htmlize some hyperlinks to info nodes.
Examples: (eeh-info \"(emacs)Lisp Eval\")
          (eeh-info \"(elisp)Scope\")
          (eeh-info \"(eintr)Buffer Names\")"
  (if (string-match "^(\\([^()]+\\))\\(.*\\)" manual-and-node)
      (let ((manual (match-string 1 manual-and-node))
	    (node   (match-string 2 manual-and-node)))
	(cond
	 ((equal manual "emacs")
	  (eeh-info-dash "http://www.gnu.org/software/emacs/manual/html_node/" node))
	 ((equal manual "elisp")
	  "http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_toc.html")
	 ((member manual '("eintr" "elintro"))
	  (eeh-info-dash "http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/" node))))))

(defun eeh-find-node (node &rest rest)
  (eeh-info node))
(defun eeh-find-enode (node &rest rest)
  (eeh-info (format "(emacs)%s" node)))
(defun eeh-find-elnode (node &rest rest)
  (eeh-info (format "(elisp)%s" node)))
(defun eeh-find-elinode (node &rest rest)
  (eeh-info (format "(eintr)%s" node)))

;; Hyperlinks to swf animations

(defun eeh-find-eevanim (fname width height &rest rest)
  (setq eeh-html (ee-swf-html-embed fname width height)))

;; Take a hyperlink sexp or its textual representation and call the
;; modified hyperlink functions to obtain its htmlization

(defvar eeh-functions nil)
(setq eeh-functions
      '((find-eimage0   . eeh-find-eimage0)
	(find-eev       . eeh-find-eev)
	(find-eevfile   . eeh-find-eevfile)
	(find-eevex     . eeh-find-eevex)
	(find-eevexfile . eeh-find-eevexfile)
	(find-angg      . eeh-find-angg)
	(find-anggfile  . eeh-find-anggfile)
	(info           . eeh-info)
	(find-node      . eeh-find-node)
	(find-enode     . eeh-find-enode)
	(find-elnode    . eeh-find-elnode)
	(find-eevanim   . eeh-find-eevanim)
	))

(defun eeh-sexp-to-html (sexp)
  "Take a hyperlink sexp and htmlize it."
  (let* ((eeh-target nil)
	 (eeh-html nil)
	 (f     (car sexp))
	 (eeh-f (cdr (assoc f eeh-functions))))
    (if eeh-f (setq eeh-target (apply eeh-f (cdr sexp))))
    (or eeh-html
	(if eeh-target
	    (format "<a href=\"%s\">%s</a>" eeh-target eeh-text)
	  eeh-text))))

(defun eeh-text-to-html (text)
  "Take the textual representation of a hyperlink sexp and htmlize it.
This function invokes `eeh-sexp-to-html', which in its turn uses
the table `eeh-functions' to pass the control to the modified
hyperlink functions (find-node -> eeh-find-node, etc); we use
variables with dynamic scoping to make the definitions of the
modified hyperlink functions shorter."
  (let ((eeh-text text)
	(eeh-sexp (read text)))
    (eeh-sexp-to-html eeh-sexp)))

;; Htmlize all the elisp hyperlinks at once, and some debugging functions.
;; We do this in two separate steps to overcome limitations of htmlize.el.
;; (find-elnode "Regexp Backslash" "shy group")

(defvar eeh-regexp-no-space  "^\\(;;\\|#\\) +\\((find-[^\n]+\\)")
(defvar eeh-regexp-space "^\\(;;\\|#\\| \\) +\\((find-[^\n]+\\)")
(defvar eeh-regexp eeh-regexp-space)
(defvar eeh-regexp-sexp-n 2)
(defvar eeh-pairs nil)
(defvar eeh-n 0)

(defun eeh-flash ()
  "Show which hyperlink sexps after point will be processed"
  (interactive)
  (save-excursion
    (while (re-search-forward eeh-regexp nil t)
      (let* ((b (match-beginning eeh-regexp-sexp-n))
	     (e (match-end       eeh-regexp-sexp-n)))
	(eeflash b e)))))

(defun eeh-1flash ()
  "Show the next hyperlink sexp after point and how will be htmlized"
  (interactive)
  (if (re-search-forward eeh-regexp nil t)
      (let* ((b (match-beginning eeh-regexp-sexp-n))
	     (e (match-end       eeh-regexp-sexp-n))
	     (str (buffer-substring-no-properties b e)))
	(eeflash b e)
	(message "%s" (eeh-text-to-html str)))))
  
(defun eeh-encode-sexps ()
  "Transform all the htmlizable hyperlink sexps after point into \"@nn@\"s"
  (interactive)
  (setq eeh-pairs nil)
  (setq eeh-n 0)
  (while (re-search-forward eeh-regexp nil t)
    (let* ((b (match-beginning eeh-regexp-sexp-n))
           (e (match-end       eeh-regexp-sexp-n))
           (str (buffer-substring-no-properties b e))
           (html (eeh-text-to-html str)))
      (when (not (equal str html))
	(setq eeh-n (1+ eeh-n))
	(let ((newstr (format "@%d@" eeh-n)))
	  (delete-region b e)
	  (insert-and-inherit newstr)
	  (setq eeh-pairs `((,newstr . ,html) . ,eeh-pairs)))))))

(defun eeh-unencode-sexps ()
"Transform all the \"@nn@\"s after point into the corresponding htmlized sexps"
  (interactive)
  (let ((pairs-left (reverse eeh-pairs)))
    (while pairs-left
      (let* ((encstr (caar pairs-left))
	     (html   (cdar pairs-left)))
	(search-forward encstr)
	(delete-region (match-beginning 0) (match-end 0))
	(insert-and-inherit html)
	(setq pairs-left (cdr pairs-left))))))




;;;  _     _             _ _           _            __  __               
;;; | |__ | |_ _ __ ___ | (_)_______  | |__  _   _ / _|/ _| ___ _ __ ___ 
;;; | '_ \| __| '_ ` _ \| | |_  / _ \ | '_ \| | | | |_| |_ / _ \ '__/ __|
;;; | | | | |_| | | | | | | |/ /  __/ | |_) | |_| |  _|  _|  __/ |  \__ \
;;; |_| |_|\__|_| |_| |_|_|_/___\___| |_.__/ \__,_|_| |_|  \___|_|  |___/
;;;                                                                      
;;; htmlize full buffers

;; font-lock-support-mode

(defun my-fontify-whole-buffer ()
  "Fontify the whole buffer. BTW, it took me *ages* to discover how to do this."
  (interactive)
  (font-lock-mode 1)
  (jit-lock-fontify-now))

(defun some-extra-replacements ()
  (interactive)
  (my-replace-regexps-with-inheritance  
   (concat "\\(<span class=\"\\(function\\|variable\\)-name\">\\)"
	   "\\([-_A-Za-z0-9]+\\)"
	   "\\(</span>\\)")
   "<a name=\"\\3\">\\1\\3\\4</a>"))

(defun my-htmlize-buffer (&optional bufname glyphsstr)
  (interactive)
  (or bufname (setq bufname (buffer-name)))
  (let ((ee-buffer-name (concat bufname " (htmlized)")))
    (find-estring (buffer-substring (point-min) (point-max)))
    (kill-region (point-min) (point-max)) ; for converting classes to props
    (yank))
  (ee-kill-buffer (concat bufname ".html"))
  ;;
  ;; Now we're in a temporary buffer called "bufname (htmlized)";
  ;; mode is fundamental-mode
  ;;
  (goto-char (point-min))
  (my-replace-glyphs glyphsstr)		; transform glyphs into normal chars
  (eeh-encode-sexps)			; hack for htmlizing elisp hyperlinks
  (switch-to-buffer (htmlize-buffer))	; ask htmlize.el to do its magic
  ;;
  ;; Now we're in a temporary buffer called "bufname.html"
  ;;
  (goto-char (point-min))
  (some-extra-replacements)		; make some anchors
  (eeh-unencode-sexps)		        ; hack for elisp hyperlinks, part 2
  )

(defun my-htmlize-this-file (&optional glyphsstr)
  (interactive)
  (let ((bufname (file-name-nondirectory (buffer-file-name))))
    ;; ^ because we don't want names like README<2>
    (my-fontify-whole-buffer)
    (my-htmlize-buffer bufname glyphsstr)
    (write-file (concat bufname ".html"))))

;; (find-sh0 "cp -v ~/eev-current/README /tmp/")
;; "^\\(;;\\|#\\| \\) +\\((find-[^\n]+\\)"

;; (find-efaces)
;; (find-ecolors)
;; (find-node "(elisp)Face Attributes")

;; (my-outline-colors)
;;
(defun my-outline-colors ()
  (interactive)
  (require 'outline)
  (set-face-foreground 'outline-1 "OrangeRed")
  (set-face-foreground 'outline-2 "Goldenrod2")
  (set-face-foreground 'outline-3 "LimeGreen")
  (set-face-foreground 'outline-4 "Dodger Blue"))

(defun change-log-mode-face-hack ()
  "Because htmlize.el doesn't follow inherits in faces."
  (require 'add-log)
  (mapc (lambda (face)
	  (set-face-foreground
	   face
	   (face-attribute face :foreground nil 'default)))
	'(change-log-date-face
	  change-log-email-face
	  change-log-file-face
	  change-log-function-face
	  change-log-list-face
	  change-log-name-face)))


;;;                  _       
;;;  _ __ ___   __ _(_)_ __  
;;; | '_ ` _ \ / _` | | '_ \ 
;;; | | | | | | (_| | | | | |
;;; |_| |_| |_|\__,_|_|_| |_|
;;;                          
;;; (find-es "page" "upload-rsync")

(defun htmlize-eev-files ()
  (interactive)
  (my-outline-colors)
  (if (not window-system)
      (error "Running htmlize outside X would produce ugly colors"))
  (let ((eeh-regexp "^\\(.*\\)[ \t]\\((find-[^\n]+)\\)$"))
    (find-eev "EMACS")           (my-htmlize-this-file)
    (find-eev "INSTALL")         (my-htmlize-this-file)
    (find-eev "INTERFACE")       (my-htmlize-this-file)
    (find-eev "NEWS")            (my-htmlize-this-file)
    (find-eev "README")          (my-htmlize-this-file)
    (find-eev "doc/keys.e")      (my-htmlize-this-file))
  (let ((eeh-regexp "^\\(.*;.*\\)[ \t]\\((find-[^\n]+)\\)$"))
    (find-eev "eev.el")          (my-htmlize-this-file)
    (find-eev "eev-bounded.el")  (my-htmlize-this-file)
    (find-eev "eev-bounded-old.el") (my-htmlize-this-file)
    (find-eev "eev-compose.el")  (my-htmlize-this-file)
    (find-eev "eev-glyphs.el")   (my-htmlize-this-file)
    (find-eev "eev-insert.el")   (my-htmlize-this-file)
    (find-eev "eev-langs.el")    (my-htmlize-this-file)
    (find-eev "eev-sshot.el")    (my-htmlize-this-file)
    (find-eev "eev-steps.el")    (my-htmlize-this-file)
    (find-eev "eev-browse-url.el") (my-htmlize-this-file)
    (find-eev "eev-mini.el")     (my-htmlize-this-file)
    ;;
    (find-eev "htmlize-all.el")  (my-htmlize-this-file)
    (find-eev "htmlize-eev.el")  (my-htmlize-this-file)
    ;;
    (find-eev "eev-dev.el")      (my-htmlize-this-file) ; obsolete
    (find-eev "eev-insert-old.el") (my-htmlize-this-file) ; obsolete
    (find-eev "eev-ttp.el")      (my-htmlize-this-file) ; obsolete
    (find-eev "eev-walk.el")     (my-htmlize-this-file) ; obsolete
    (find-eev "compose.el")      (my-htmlize-this-file) ; obsolete
    (find-eev "glyphs.el")       (my-htmlize-this-file) ; obsolete
    ;;
    (require 'eev-math-glyphs)
    (eev-math-glyphs-edrx)
    (let ((g "\f›«»ÞåÆØðÛÏ§®´¶ýÎñ¨Ý¢ÌþÅ¿‚©Ñ¥¤¸÷î­£¦¯ËÐ"))
      (find-eev "eev-math-glyphs.el") (my-htmlize-this-file g) ; new
    )
    ;;
    (change-log-mode-face-hack)
    (find-eev "ChangeLog")       (my-htmlize-this-file))
  (let ((eeh-regexp "^\\(.*\\)[ \t]\\((find-[^\n]+)\\)$"))
    (find-eev "anim/channels.anim")   (my-htmlize-this-file)
    (find-eev "anim/gdb.anim")        (my-htmlize-this-file))
  ;;
  (let ((eeh-regexp "^\\(.*#.*\\)[ \t]\\((find-[^\n]+)\\)$"))
    (find-eev "eeg4")            (my-htmlize-this-file)
    (find-eev "eegchannel")      (my-htmlize-this-file)
    (find-eev "rcfiles/.bashrc")      (my-htmlize-this-file)
    (find-eev "rcfiles/.bashrc-psne") (my-htmlize-this-file)
    (find-eev "rcfiles/.fvwmrc")      (my-htmlize-this-file)
    (find-eev "rcfiles/.pythonrc.py") (my-htmlize-this-file)
    (find-eev "rcfiles/.tclshrc")     (my-htmlize-this-file)
    (find-eev "rcfiles/.vimrc")       (my-htmlize-this-file)
    (find-eev "rcfiles/.zshrc")       (my-htmlize-this-file)
    (find-eev "rcfiles/.zshrc-psne")  (my-htmlize-this-file)
    (find-eev "rcfiles/change")       (my-htmlize-this-file)
    (find-eev "rcfiles/change.awk")   (my-htmlize-this-file)
    (find-eev "rcfiles/channel.py")   (my-htmlize-this-file)
    (find-eev "rcfiles/channel.tcl")  (my-htmlize-this-file)
    ;;
    (find-eev "eev-rctool")           (my-htmlize-this-file) ; new
    ))

(defun htmlize-eev-files-then-quit ()
  (interactive)
  (htmlize-eev-files-then-quit)
  (save-buffers-kill-emacs))


;; (htmlize-eev-files)



;;;        _ _        _ _    
;;;  _ __ (_) | _____(_) | __
;;; | '_ \| | |/ / __| | |/ /
;;; | | | | |   <\__ \ |   < 
;;; |_| |_|_|_|\_\___/_|_|\_\
;;;                          

;; Notes, 2006aug22:
;; I select each file manually and I do my own tricks to fontify and
;; htmlize each one; but my tricks don't work so well -- some faces are
;; not applied. htmlize-file and htmlize-many-files are much better --
;;
;; (find-eevfile "htmlize.el" "defun htmlize-file")
;; (find-eevfile "htmlize.el" "defun htmlize-many-files")
;; (find-eevfile "htmlize-all.el")
;; (find-eevfile "htmlize-eev.el")
;;
;; TODO: change htmlize-file a bit to implement hooks for htmlizing
;; glyphs, elisp hyperlinks, and defun/defvar anchors.
;;
;; A demo ("yes, htmlize knows how to fontify everything"):
;;
' (find-sh  "cd ~/eev-current/; ls *.el")
' (find-sh0 "rm -Rv /tmp/ehtml/; mkdir /tmp/ehtml/")
' (require 'htmlize)
' (let ((default-directory (ee-expand "~/eev-current/")))
    (htmlize-many-files
     '("compose.el"
       "eev-browse-url.el"
       "eev-compose.el"
       "eev-glyphs.el"
       "eev-insert.el"
       "eev-langs.el"
       "eev-math-glyphs.el"
       "eev-steps.el"
       "eev.el"
       "glyphs.el"
       "htmlize-all.el"
       "htmlize-eev.el")
     "/tmp/ehtml/"))
' (find-fline "/tmp/ehtml/")
' (find-firefox (eeurl-u-to-f "/tmp/ehtml/eev.el.html"))
' (find-firefox (eeurl-u-to-f "~/eev-current/eev.el.html"))



;; (find-eevfile "htmlize.el" "defun htmlize-file")
'
(defun htmlize-file (file &optional target)
  "Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name."
  (interactive (list (read-file-name
		      "HTML-ize file: "
		      nil nil nil (and (buffer-file-name)
				       (file-name-nondirectory
					(buffer-file-name))))))
  (let ((output-file (if (and target (not (file-directory-p target)))
			 target
		       (expand-file-name
			(htmlize-make-file-name (file-name-nondirectory file))
			(or target (file-name-directory file)))))
	;; Try to prevent `find-file-noselect' from triggering
	;; font-lock because we'll fontify explicitly below.
	(font-lock-mode nil)
	(font-lock-auto-fontify nil)
	(global-font-lock-mode nil)
	;; Ignore the size limit for the purposes of htmlization.
	(font-lock-maximum-size nil)
	;; Disable font-lock support modes.  This will only work in
	;; more recent Emacs versions, so htmlize-buffer-1 still needs
	;; to call htmlize-ensure-fontified.
	(font-lock-support-mode nil))
    (with-temp-buffer
      ;; Insert FILE into the temporary buffer.
      (insert-file-contents file)
      ;; Set the file name so normal-mode and htmlize-buffer-1 pick it
      ;; up.  Restore it afterwards so with-temp-buffer's kill-buffer
      ;; doesn't complain about killing a modified buffer.
      (let ((buffer-file-name file))
	;; Set the major mode for the sake of font-lock.
	(normal-mode)
	(font-lock-mode 1)
	(unless font-lock-mode
	  ;; In GNU Emacs (font-lock-mode 1) doesn't force font-lock,
	  ;; contrary to the documentation.  This seems to work.
	  (font-lock-fontify-buffer))
	;; htmlize the buffer and save the HTML.
	(with-current-buffer (htmlize-buffer-1)
	  (unwind-protect
	      (progn
		(run-hooks 'htmlize-file-hook)
		(write-region (point-min) (point-max) output-file))
	    (kill-buffer (current-buffer)))))))
  ;; I haven't decided on a useful return value yet, so just return
  ;; nil.
  nil)

;; (find-eevfile "htmlize.el" "defun htmlize-many-files")
'
(defun htmlize-many-files (files &optional target-directory)
  "Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file."
  (interactive
   (list
    (let (list file)
      ;; Use empty string as DEFAULT because setting DEFAULT to nil
      ;; defaults to the directory name, which is not what we want.
      (while (not (equal (setq file (read-file-name
				     "HTML-ize file (RET to finish): "
				     (and list (file-name-directory
						(car list)))
				     "" t))
			 ""))
	(push file list))
      (nreverse list))))
  ;; Verify that TARGET-DIRECTORY is indeed a directory.  If it's a
  ;; file, htmlize-file will use it as target, and that doesn't make
  ;; sense.
  (and target-directory
       (not (file-directory-p target-directory))
       (error "target-directory must name a directory: %s" target-directory))
  (dolist (file files)
    (htmlize-file file target-directory)))










(provide 'htmlize-eev)


;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; End:
