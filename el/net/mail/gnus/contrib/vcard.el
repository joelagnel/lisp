;;; vcard.el --- vcard parsing and display routines

;; Copyright (C) 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1997-09-27

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; The display routines here are just an example.  The primitives in the
;; first section can be used to construct other vcard formatters.

;;; Code:

(defvar vcard-standard-filters '(vcard-filter-html)
  "*Standard list of filters to apply to parsed vcard data.
These filters are applied sequentially to vcard data records when
the function `vcard-standard-filter' is supplied as the second argument to
`vcard-parse-string'.")

(defun vcard-parse-string (raw &optional filter)
  "Parse RAW vcard data as a string, and return an alist representing data.

If the optional function FILTER is specified, apply that filter to the
data record of each key before splitting fields.  Filters should accept
two arguments: the key and the data.  They are expected to operate on
\(and return\) a modified data value.

Vcard data is normally in the form

    begin:        vcard
    key1:         field
    key2;subkey1: field
    key2;subkey2: field1;field2;field3
    end:          vcard

\(Whitespace after the colon separating the key and field is optional.\)
If supplied to this function an alist of the form

    ((\"key1\" \"field\")
     (\"key2\"
      (\"subkey2\" \"field1\" \"field2\" \"field3\")
      (\"subkey1\" \"field\")))

would be returned."
  (save-match-data
    (let ((raw-pos 0)
          (vcard-data nil)
          key data)
      (string-match "^[ \t]*begin:[ \t]*vcard[ \t]*[\r\n]+" raw raw-pos)
      (setq raw-pos (match-end 0))
      (while (and (< raw-pos (length raw))
                  (string-match
                   "^[ \t]*\\([^:]+\\):[ \t]*\\(.*\\)[ \t]*[\n\r]+"
                   raw raw-pos))
        (setq key (vcard-matching-substring 1 raw))
        (setq data (vcard-matching-substring 2 raw))
        (setq raw-pos (match-end 0))
        (cond
         ((string= key "end")
          (setq raw-pos (length raw)))
         (t
          (and filter
               (setq data (funcall filter key data)))
          (setq vcard-data
                (vcard-set-alist-slot vcard-data
                                      (vcard-split-string key ";")
                                      (vcard-split-string data ";"))))))
      (nreverse vcard-data))))

(defun vcard-ref (key vcard-data)
  "Return the vcard data associated with KEY in VCARD-DATA.
Key may be a list of nested keys or a single string of colon-separated
keys."
  (cond ((listp key)
         (vcard-alist-assoc key vcard-data))
        ((and (stringp key)
              (save-match-data
                (string-match ";" key)))
         (vcard-alist-assoc (vcard-split-string key ";") vcard-data))
        ((stringp key)
         (cdr (assoc key vcard-data)))))


;;; Vcard data filters.

;; These receive both the key and data, but are expected to operate on (and
;; return) just the data.
;;
;; There is probably no overwhelming need for this, except that some lusers
;; put HTML in their vcards under the misguided notion that it's a standard
;; feature of vcards just because Netscape supports this feature.  (Or
;; perhaps those lusers just don't care that their vcards look like shit in
;; every other MUA).
;;
;; On the other hand, perhaps someone will devise some other use for these
;; filters, such as noticing common phone number formats and re-formatting
;; them to fit personal preferences.

(defun vcard-filter-apply-filter-list (filter-list key data)
  (while filter-list
    (setq data (funcall (car filter-list) key data))
    (setq filter-list (cdr filter-list)))
  data)

(defun vcard-standard-filter (key data)
  (vcard-filter-apply-filter-list vcard-standard-filters key data))

(defun vcard-filter-html (key data)
  (save-match-data
    (while (string-match "<[^<>\n]+>" data)
      (setq data (concat (substring data 0 (match-beginning 0))
                         (substring data (match-end 0)))))
    data))


;;; Utility routines.

;; This does most of the dirty work of key lookup for vcard-ref.
(defun vcard-alist-assoc (keys alist)
  (while (and keys alist)
    (setq alist (cdr (assoc (car keys) alist)))
    (setq keys (cdr keys)))
  alist)

;; In ALIST, set KEY-LIST's value to VALUE, and return new value of ALIST.
;; KEY-LIST should be a list of nested keys, if ALIST is an alist of alists.
;; If any key is not present in an alist, the key and value pair will be
;; inserted into the parent alist.
(defun vcard-set-alist-slot (alist key-list value)
  (let* ((key (car key-list))
         (elt (assoc key alist)))
    (setq key-list (cdr key-list))
    (cond ((and (cdr elt) key-list)
           (vcard-set-alist-slot (cdr elt) key-list value))
          ((and elt key-list)
           (setcdr elt (vcard-set-alist-slot nil key-list value)))
          (elt (setcdr elt value))
          (t
           (let ((new))
             (setq key-list (nreverse (cons key key-list)))
             (while key-list
               (if new
                   (setq new (cons (car key-list) (cons new nil)))
                 (setq new (cons (car key-list) value)))
               (setq key-list (cdr key-list)))

             (cond ((null alist)
                    (setq alist (cons new nil)))
                   (t
                    (setcdr alist (cons (car alist) (cdr alist)))
                    (setcar alist new))))))
    alist))

;; Return substring matched by last search.
;; N specifies which match data pair to use
;; Value is nil if there is no Nth match.
;; If STRING is not specified, the current buffer is used.
(defun vcard-matching-substring (n &optional string)
  (if (match-beginning n)
      (if string
	  (substring string (match-beginning n) (match-end n))
	(buffer-substring (match-beginning n) (match-end n)))))

;; Split STRING at occurences of SEPARATOR.  Return a list of substrings.
;; SEPARATOR can be any regexp, but anything matching the separator will
;; never appear in any of the returned substrings.
(defun vcard-split-string (string separator)
  (let* ((list nil)
         (pos 0))
    (save-match-data
      (while (string-match separator string pos)
        (setq list (cons (substring string pos (match-beginning 0)) list))
        (setq pos (match-end 0)))
      (nreverse (cons (substring string pos) list)))))

(defun vcard-flatten (l)
  (if (consp l)
      (apply 'nconc (mapcar 'vcard-flatten l))
    (list l)))


;;; Sample formatting routines.

(defun vcard-format-box (vcard-data)
  "Like `vcard-format-string', but put an ascii box around text."
  (let* ((lines (vcard-format-lines vcard-data))
         (len (vcard-format-max-length lines))
         (edge (concat "\n+" (make-string (+ len 2) ?-) "+\n"))
         (line-fmt (format "| %%-%ds |" len))
         (formatted-lines
          (mapconcat (function (lambda (s) (format line-fmt s))) lines "\n")))
    (if (string= formatted-lines "")
        formatted-lines
      (concat edge formatted-lines edge))))

(defun vcard-format-string (vcard-data)
  "Format VCARD-DATA into a string suitable for presentation.
VCARD-DATA should be a parsed vcard alist.  The result is a string
with formatted vcard information which can be inserted into a mime
presentation buffer."
  (mapconcat 'identity (vcard-format-lines vcard-data) "\n"))

(defun vcard-format-lines (vcard-data)
  (let* ((name  (vcard-format-get-name      vcard-data))
         (title (vcard-format-ref "title"   vcard-data))
         (org   (vcard-format-ref "org"     vcard-data))
         (addr  (vcard-format-get-address   vcard-data))
         (tel   (vcard-format-get-telephone vcard-data))
         (lines (delete nil (vcard-flatten (list name title org addr))))
         (col-template (format "%%-%ds%%s"
                               (vcard-format-offset lines tel)))
         (l lines))
    (while tel
      (setcar l (format col-template (car l) (car tel)))
      ;; If we stripped away too many nil slots from l, add empty strings
      ;; back in so setcar above will work on next iteration.
      (and (cdr tel)
           (null (cdr l))
           (setcdr l (cons "" nil)))
      (setq l (cdr l))
      (setq tel (cdr tel)))
    lines))


(defun vcard-format-get-name (vcard-data)
  (let ((name (vcard-format-ref "fn" vcard-data))
        (email (or (vcard-format-ref '("email" "internet") vcard-data)
                   (vcard-format-ref "email" vcard-data))))
    (if email
        (format "%s <%s>" name email)
      name)))

(defun vcard-format-get-address (vcard-data)
  (let* ((addr-raw (or (vcard-format-ref '("adr" "dom") vcard-data)
                       (vcard-format-ref "adr" vcard-data)))
         (addr (if (consp addr-raw)
                   addr-raw
                 (list addr-raw)))
         (street (delete "" (list (nth 0 addr) (nth 1 addr) (nth 2 addr))))
         (city-list (delete "" (nthcdr 3 addr)))
         (city (cond ((null (car city-list)) nil)
                     ((cdr city-list)
                      (format "%s, %s"
                              (car city-list)
                              (mapconcat 'identity (cdr city-list) " ")))
                     (t (car city-list)))))
    (delete nil
            (if city
                (append street (list city))
              street))))

(defun vcard-format-get-telephone (vcard-data)
  (delete nil
          (mapcar (function (lambda (x)
                              (let ((result (vcard-format-ref (car x)
                                                              vcard-data)))
                                (and result
                                     (concat (cdr x) result)))))
                  '((("tel" "work") . "Work: ")
                    (("tel" "home") . "Home: ")
                    (("tel" "fax")  . "Fax:  ")))))

(defun vcard-format-ref (key vcard-data)
  (setq key (vcard-ref key vcard-data))
  (or (cdr key)
      (setq key (car key)))
  (and (stringp key)
       (string= key "")
       (setq key nil))
  key)

(defun vcard-format-offset (row1 row2 &optional maxwidth)
  (or maxwidth (setq maxwidth (frame-width)))
  (let ((max1 (vcard-format-max-length row1))
        (max2 (vcard-format-max-length row2)))
    (+ max1 (min 5 (max 1 (- maxwidth (+ max1 max2)))))))

(defun vcard-format-max-length (strings)
  (let ((maxlen 0)
        (len 0))
    (while strings
      (setq len (length (car strings)))
      (setq strings (cdr strings))
      (and (> len maxlen)
           (setq maxlen len)))
    maxlen))

(provide 'vcard)

;;; arch-tag: 64df032f-e54c-4cfb-9e8c-8bead284f61b
;;; vcard.el ends here
