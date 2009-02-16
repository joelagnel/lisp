;;; elunit-compat.el --- Compatability helper for XEmacs, etc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Older versions require this to use `time-subtract'?
(ignore-errors
  (require 'time-date)) ;; bombs on a minial Emacs 21

;; XEmacs and Emacs 21?
(unless (fboundp 'line-number-at-pos)
  (defalias 'line-number-at-pos 'line-number))

;; XEmacs
(unless (fboundp 'propertize)
  (defun elunit-propertize (string &rest props)
    ;; from erc's erc-propertize
    (let ((string (copy-sequence string)))
      (while props
	(put-text-property 0 (length string)
			   (nth 0 props) (nth 1 props) string)
	(setq props (cddr props)))
      string))
  (defalias 'propertize 'elunit-propertize))

(unless (fboundp 'with-temp-message)
  (defmacro elunit-with-temp-message (&rest body)
    `(let ((old-message (with-current-buffer " *Echo Area*" (buffer-substring))))
       ,@body
       (with-current-buffer " *Echo Area*"
	 (delete-region (point-min) (point-max))
	 (insert old-message))))
  (defalias 'with-temp-message 'elunit-with-temp-message))

(defun elunit-format* (format &rest params)
  "A wrapper for `format' which ensures that `concat' is used internally
for strings using the \"%s\" format so that extents are copied in XEmacs."
  (let (segments)
    (save-match-data
      (let ((start 0)
            (original-params params))
	(while (string-match "%\\(?:%\\|.*?[a-z]\\)" format start)
	  (push (substring format start (match-beginning 0)) segments)
	  (setq start (match-end 0))
	  (let ((token (match-string 0 format)))
	    (push (if (string= token "%%")
                      "%"
                    (unless params ;show not enough arguments error in context
                      (apply 'format format original-params))
                    (let ((param (pop params)))
                      (if (and (string= token "%s") (stringp param))
                          param
                        (condition-case nil ;show errors in context
                            (format token param)
                          (error (apply 'format format original-params))))))
		  segments)))
	(push (substring format start) segments)))
    (apply 'concat (reverse segments))))
(defalias 'format* 'elunit-format*)

(provide 'elunit-compat)