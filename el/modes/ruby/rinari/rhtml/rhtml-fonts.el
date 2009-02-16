;;;
;;; rhtml-fonts.el - font-lock-based fontification support for `rhtml-mode'
;;;

;; ***** BEGIN LICENSE BLOCK *****
;; Version: MPL 1.1/GPL 2.0/LGPL 2.1

;; The contents of this file are subject to the Mozilla Public License Version 
;; 1.1 (the "License"); you may not use this file except in compliance with 
;; the License. You may obtain a copy of the License at 
;; http://www.mozilla.org/MPL/

;; Software distributed under the License is distributed on an "AS IS" basis,
;; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;; for the specific language governing rights and limitations under the
;; License.

;; The Original Code is Fontification Support for RHTML-MODE.

;; The Initial Developer of the Original Code is
;; Paul Nathan Stickney <pstickne@gmail.com>.
;; Portions created by the Initial Developer are Copyright (C) 2006
;; the Initial Developer. All Rights Reserved.

;; Contributor(s):

;; Alternatively, the contents of this file may be used under the terms of
;; either the GNU General Public License Version 2 or later (the "GPL"), or
;; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
;; in which case the provisions of the GPL or the LGPL are applicable instead
;; of those above. If you wish to allow use of your version of this file only
;; under the terms of either the GPL or the LGPL, and not to allow others to
;; use your version of this file under the terms of the MPL, indicate your
;; decision by deleting the provisions above and replace them with the notice
;; and other provisions required by the GPL or the LGPL. If you do not delete
;; the provisions above, a recipient may use your version of this file under
;; the terms of any one of the MPL, the GPL or the LGPL.

;; ***** END LICENSE BLOCK *****


(defvar rhtml-font-lock-keywords
  '(("\\(<%\\#[^%]*%>\\)" . (1 font-lock-comment-face t nil))

    ("<%[=]?\\([^%]*\\)%>" . (1 'erb-face keep t))

    ("<\\(/?[[:alnum:]][-_.:[:alnum:]]*\\)" 1 font-lock-function-name-face) ; tags
    ("\\([a-zA-Z0-9]*[ ]?\\)=" 1 font-lock-variable-name-face) ; attributes

    ("\\(\"[^\"\n]*\"\\)" .(1 font-lock-string-face append nil))
    ("\\('[^'\n]*'\\)" .(1 font-lock-string-face prepend nil))
    ("\\(<!--.*?-->\\)" . (1 font-lock-comment-face prepend nil))))

(defvar rhtml-in-erb-keywords
  '(("\\([A-Z][0-9a-zA-Z_]*\\)" . (1 font-lock-type-face prepend))
    ("[^_]\\<\\(alias\\|and\\|begin\\|break\\|case\\|catch\\|class\\|def\\|do\\|elsif\\|else\\|fail\\|ensure\\|for\\|end\\|if\\|in\\|module\\|next\\|not\\|or\\|raise\\|redo\\|rescue\\|retry\\|return\\|then\\|throw\\|super\\|unless\\|undef\\|until\\|when\\|while\\|yield\\|render\\)\\>[^_]" .
     (1 font-lock-keyword-face prepend))
    ("\\(@[0-9a-zA-Z_]*\\)" . (1 font-lock-variable-name-face prepend))
    ("\\(:[0-9a-zA-Z_]*\\)" . (1 font-lock-constant-face prepend))))


(defun rhtml-activate-fontification ()
  "Activate font-lock fontification support for the current buffer."
  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function 'rhtml-fontify-region) 
  (make-local-variable 'font-lock-fontify-buffer-function)
  (setq font-lock-fontify-buffer-function 'rhtml-fontify-buffer)
  (setq font-lock-defaults '(rhtml-font-lock-keywords t)))


(defun rhtml-fontify-buffer ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (rhtml-fontify-region (point-min) (point-max)))))

(defun rhtml-fontify-region (begin end &optional loudly)
  "Applies fontification to the region bound by R-START and R-END.
The region actually fontified will be expanded to cover any
partial ERB tags found."
  (interactive "r")
  (remove-text-properties begin end '(face fontified))
  (let* ((region (rhtml-union-region-containing-erb-tags begin end))
	 (begin (car region))
	 (end (cdr region)))
    (font-lock-fontify-keywords-region begin end loudly)
    (let ((font-lock-keywords rhtml-in-erb-keywords)
	  (case-fold-search nil))
      (each-search rhtml-erb-tag-re
		   (setq erb-region (cons start end))
		   (font-lock-fontify-keywords-region start end loudly)
		   (rhtml-fontify-tag-base start end)))))


(defun rhtml-fontify-tag-base (start end)
  "Applies face to delimeters and body but in a less-agressive manner with
`font-lock-append-text-property'.  OPEN-PAIR, BODY-PAIR and CLOSE-PAIR are
of (START . END)."
  (flet ((fontify-regions (delim-face &optional body-face close)
			  (font-lock-append-text-property start start-end 'face delim-face)
                          (when body-face
                            (font-lock-append-text-property start-end end-start 'face body-face))
                          (font-lock-append-text-property end-start end 'face delim-face)))

    (let* ((start-end (+ start (string-match "[^%=#<]" (buffer-substring start end))))
	   (end-start (- end (length rhtml-erb-close-delim)))
	   (start-type (buffer-substring start start-end)))
      (case (rhtml-erb-delim-type start-type)
        (exec (fontify-regions 'erb-no-out-delim-face 'erb-no-out-face))
        (out (fontify-regions 'erb-out-delim-face 'erb-out-face))
        (comment (fontify-regions 'erb-comment-delim-face 'erb-comment-face))))))



(defmacro each-search (re &rest body)
  `(save-excursion
     (beginning-of-buffer)
     (while (re-search-forward ,re nil t)
       (let ((start (match-beginning 0))
	     (end (match-end 0)))
	 (save-excursion
	   ,@body)))))


;; ERB faces - each type of ERB tag has it's own face properties

(defface erb-face
  `((t (:background "snow2")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "snow3")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)

(defface erb-no-out-face
  `((t (:inherit erb-face)))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-no-out-delim-face
  `((t (:inherit erb-delim-face :weight bold)))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-out-face
  `((t (:inherit erb-face)))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-out-delim-face
  `((t (:inherit erb-delim-face :weight bold :foreground "darkred")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-comment-face
  `((t (:inherit erb-face :weight bold :foreground "darkgreen")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-comment-delim-face
  `((t (:inherit erb-delim-face :weight bold :foreground "darkgreen")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)


;;
(provide 'rhtml-fonts)