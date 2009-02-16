;;;; localized-source.el -- show localization for source files, e.g. translations of comments and perhaps names
;;; Time-stamp: <2006-05-05 17:16:50 john>

(provide 'localized-source)

;; todo: make this a minor mode?
;; todo: integrate with mulvoc for getting translations

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(defvar localized-comment-overlays nil
  "Localized comment overlays.
This variable has a separate value in each buffer.")

(make-variable-buffer-local 'localized-comment-overlays)

(defvar localized-comment-hide-originals t
  "*Whether to hide the original comments.")

(defvar localize-unlocalizable-strings-regexps '("Time-stamp: ")
  "List of patterns for recognizing strings that we will not try to localize.")

(defun localize-unlocalizable-string (string)
  "If STRING is one that we should not localize, return it; otherwise return nil.
localize-unlocalizable-strings-regexps shows which strings not to localize."
  (catch 'found
  (let ((patterns localize-unlocalizable-strings-regexps))
    (while patterns
      (if (string-match (car patterns) string)
	  (throw 'found string))
      (setq patterns (cdr patterns)))
    nil)))

(defvar localize-translations-alist 
  '(("get rid of any previous comment overlays" . "get rid of any previous comment overlays")
    ("end of localized-source.el" . "crioch na localized-source.el")
    ("this one is too horrible:" . "this one is too horrible:"))
  "Alist of translations.

You should load this with your translations if you are using the
default translation functions.  Otherwise, you can provide your own
translation functions, and put them onto localize-string-functions.

This is used in a linear search. If using this system on a large scale, it would be a good idea to write a replacement, using a hashing system such as an obarray.

Examples are given to localize the comments of the localized-source program!")

(defun localize-string-whole (string)
  "If we have a whole translation of STRING, return it, otherwise return nil.
Translations are stored in localize-translations-alist, which is used
in a linear search. If using this system on a large scale, it would be
a good idea to write a replacement, using a hashing system such as an
obarray.
You might also use mulvoc.el (which should be available along with this
package) to manage the translations if you are working in many
languages."
  (let ((pair (assoc string localize-translations-alist)))
    (if pair
	(cdr pair)
      nil)))

(defun localize-string-whole-by-mulvoc (string) ; will need to pass language in as well, and have something for remembering the current language!
  "Use mulvoc to translate STRING."
  (mulvoc-translate-word-to-language string 'phrase 'ENG to-language)
)

(defun localize-string-word (word)
  "Try to translate WORD, for any words given in localize-translations-alist,
which is used in a linear search. If using this system on a large
scale, it would be a good idea to write a replacement, using a hashing
system such as an obarray.

A word-by-word translation is pretty horrible anyway!"
  (let ((pair (assoc word localize-translations-alist)))
    (if pair
	(cdr pair)
      word)))

(defun localize-string-as-words (string)
  "Try to translate STRING, word by word, for any words given in localize-translations-alist."
  (mapconcat 'localize-string-word (split-string string) " "))

(defvar localize-string-functions '(localize-unlocalizable-string
				    localize-string-whole
				    ;; this one is too horrible:
				    ;; localize-string-as-words
				    )
  "Functions to localize a string.

Each function should take the original string, and return either a
string that is a translation of the original, or nil.

You could, for example, first try something that looks up a
translation of the whole of its argument, and then something that
tries to translate it word-by-word. Such functions are provided here
as an example. The word-by-word one is pretty horrible in its
effects! ")

(defvar localization-needed-format-string "Please translate: ``%s''"
  "A format string to pass strings through if no translation is found.")

(defvar localized-comments-needed-file "~/localization-needed"
  "A file into which strings needing localization are put.")

(defvar localized-comments-waiting-to-be-localized nil
  "Comments we have found that have not yet been localized.
These get written into the file named by localized-comments-needed-file.")

(defun localized-comments-save-unlocalized-list ()
  "Save the unlocalized comments."
  (save-window-excursion
    (save-excursion
      (find-file localized-comments-needed-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" (point-max) t)
	(let ((new (match-string-no-properties 1)))
	  (if (not (member new localized-comments-waiting-to-be-localized))
	      (setq localized-comments-waiting-to-be-localized
		    (cons new localized-comments-waiting-to-be-localized)))))
      (erase-buffer)
      (goto-char (point-max))
      (let ((these (reverse localized-comments-waiting-to-be-localized)))
	(while these
	  (insert (car these) "\n")
	  (setq these (cdr these))))
      (basic-save-buffer)
      (bury-buffer))))

(defun localize-comment (original)
  "Return a translation of ORIGINAL into your preferred language.
This uses localize-string-functions to try to make a translation.  If
no translation is found, it returns the original string, but marked
\(by passing it to format, with localization-needed-format-string) to
make its need for translation stand out."
  (let ((translation (run-hook-with-args-until-success 'localize-string-functions original)))
    (if translation
	translation
      (let ((fallback (format localization-needed-format-string original)))
	;; I wanted to make untranslated things show up garishly... this doesn't work...
	;; (put-text-property 0 (1- (length fallback)) 'face '(background-color . red) fallback)
	(if (not (member original localized-comments-waiting-to-be-localized))
	    (setq localized-comments-waiting-to-be-localized
		  (cons original localized-comments-waiting-to-be-localized)))
	fallback))))

(defun show-localized-comments ()
  "Show localized comments.
This function is intended for use on find-file-hooks or the mode setup
hooks for various programming language modes, or for interactive use."
  (interactive)
  (hide-localized-comments) ; get rid of any previous comment overlays
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s<+ *\\(\\S>*\\) *\\s>" (point-max) t)
      (if t		   ; todo: check we are not in a quoted string
	  (let* ((comment-string (match-string-no-properties 1))
		 (comment-overlay (make-overlay (match-beginning 1) (match-end 1) nil t t)))
	    (setq localized-comment-overlays (cons comment-overlay localized-comment-overlays))
	    (overlay-put comment-overlay 'after-string (localize-comment comment-string))
	    (overlay-put comment-overlay 'invisible 'localized-comment-hide-originals))))
    (localized-comments-save-unlocalized-list)))

(defun hide-localized-comments ()
  "Hide localized comments."
  (interactive)
  (mapcar 'delete-overlay localized-comment-overlays)
  (setq localized-comment-overlays nil))

;;; end of localized-source.el
