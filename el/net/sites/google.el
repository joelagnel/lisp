;;; google.el --- Emacs interface to the Google API

;; Copyright (C) 2002  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: comm, processes, tools

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

;; You should always be able to find the latest version here:

;;           <URL:http://oconnor.cx/elisp/google.el>

;; As of 2006-12-05, the SOAP API which google.el uses has been
;; deprecated. Unfortunately, the AJAX Search API, touted as a
;; replacement, won't work for google.el.

;; A really bare-bones first hack at Google API support for Emacs.
;; Note that you need a Google license key to use this; you can
;; get one by following the instructions here:

;;                <URL:http://www.google.com/apis/>

;; Usage:

;; (require 'google)
;; (setq google-license-key "my license key")
;; Then M-x google-search RET
;; or M-x google-search-region RET

;; To use this in a program, see the functions `google-search' and
;; `google-display-response' for example usage.

;;; Code:

(require 'soap)
(require 'xml)

(defgroup google nil
  ""
  :group 'tools)

(defcustom google-license-key nil
  "*Your Google license key."
  :type '(string)
  :group 'google)

(defcustom google-search-result-callback nil
  "*The function to be called with the search result."
  :type '(function)
  :group 'google)

(defcustom google-start 0
  "*Which result to start with."
  :type 'integer
  :group 'google)

(defcustom google-max-results 10
  "*Maximum number of results to return."
  :type 'integer
  :group 'google)

(defcustom google-filter-p t
  "*Whether or not to filter results."
  :type 'boolean
  :group 'google)

(defcustom google-safe-p nil
  "*Safe or not?"
  :type 'boolean
  :group 'google)

(defcustom google-linkify-links-p t
  "*Whether or not we should linkify links in the response buffer."
  :type 'boolean
  :group 'google)

(defun google-xml-sexp-attr-to-xml (attr-cons)
  (let ((attr-name (car attr-cons))
        (attr-val (cdr attr-cons)))
    (unless (stringp attr-val)
      (setq attr-val (format "%s" attr-val)))
    (concat (format " %s=" attr-name)
            (if (string-match "[\"]" attr-val)
                (format "'%s'" attr-val)
              (format "\"%s\"" attr-val)))))

(defun google-xml-sexp-to-xml (xml-sexp)
  "Return a string containing an XML representation of XML-SEXP."
  (cond ((null xml-sexp)
         "")
        ((stringp xml-sexp)
         xml-sexp)
        ((listp xml-sexp)
         (let ((tag (xml-node-name xml-sexp))
               (attrs (xml-node-attributes xml-sexp))
               (children (xml-node-children xml-sexp)))
           (concat (format "<%s" tag)
                   (if attrs
                       (mapconcat 'google-xml-sexp-attr-to-xml
                                  attrs
                                  "")
                     "")
                   (if children
                       (concat ">"
                               (mapconcat 'google-xml-sexp-to-xml
                                          children
                                          "")
                               (format "</%s>" tag))
                     "/>"))))

        (t (google-xml-sexp-to-xml (format "%s" xml-sexp)))))

(defun google-request (xml-sexp)
  "Send XML-SEXP to Google as a request."
  (soap-request "http://api.google.com/search/beta2"
                (google-xml-sexp-to-xml xml-sexp)))

(defun google-search-internal (terms start max-results filter-p safe-p)
  "Search for TERMS."
  (google-request
   `(SOAP-ENV:Envelope ((xmlns:SOAP-ENV
                         . "http://schemas.xmlsoap.org/soap/envelope/")
                        (xmlns:xsi
                         . "http://www.w3.org/1999/XMLSchema-instance")
                        (xmlns:xsd . "http://www.w3.org/1999/XMLSchema"))
      (SOAP-ENV:Body ()
        (ns1:doGoogleSearch ((xmlns:ns1 . "urn:GoogleSearch")
                             (SOAP-ENV:encodingStyle .
                              "http://schemas.xmlsoap.org/soap/encoding/"))
          (key ((xsi:type . "xsd:string"))
            ,google-license-key)
          (q ((xsi:type . "xsd:string"))
            ,terms)
          (start ((xsi:type . "xsd:int"))
            ,(format "%d" start))
          (maxResults ((xsi:type . "xsd:int"))
            ,(format "%d" max-results))
          (filter ((xsi:type . "xsd:boolean"))
            ,(if filter-p "true" "false"))
          (restrict ((xsi:type . "xsd:string")))
          (safeSearch ((xsi:type . "xsd:boolean"))
            ,(if safe-p "true" "false"))
          (lr ((xsi:type . "xsd:string")))
          (ie ((xsi:type . "xsd:string"))
            "latin1")
          (oe ((xsi:type . "xsd:string"))
            "latin1"))))))

(defvar google-result-mode-map (make-sparse-keymap)
  "Map to be used in `google-result-mode'.")

(define-key google-result-mode-map "q" 'google-result-quit)

(defun google-result-quit ()
  (interactive)
  (kill-buffer (get-buffer-create "*google-response*")))

(defun google-result-mode ()
  (kill-all-local-variables)
  (setq major-mode 'google-result-mode
        mode-name "Google Result")
  (set (make-local-variable 'font-lock-defaults)
       '(message-font-lock-keywords t))
  (use-local-map google-result-mode-map))

(defun google-display-response (processed-response)
  (with-current-buffer (get-buffer-create "*google-response*")
    (delete-region (point-min)
                   (point-max))
    (google-result-mode)
    (setq header-line-format
          (format "Google search results for %S\n" (car processed-response)))
    (setq processed-response (cdr processed-response))
    (while processed-response
      (let* ((item (car processed-response))
             (url (nth 0 item))
             (title (nth 1 item))
             (hostname (nth 2 item))
             (cached-size (nth 3 item))
             (snippet (nth 4 item)))

        (when title
          (insert "Title: ")
            (insert title))

        (when url
          (insert (format "URL: %s\n" url)))

        (when hostname
          (insert (format "Hostname: %s\n" hostname)))

        (when cached-size
          (insert (format "Size: %s\n" cached-size)))

        (when snippet
          (insert (format "\n%s" snippet))
          (fill-paragraph -1))

        (insert "\n\n"))

      (setq processed-response (cdr processed-response)))
    (when google-linkify-links-p
      (goto-address))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(defun google-process-response (response)
  (let* ((body (car (xml-get-children (car response) 'SOAP-ENV:Body)))
         (g-s-r (car (xml-get-children body 'ns1:doGoogleSearchResponse)))
         (return (car (xml-get-children g-s-r 'return)))
         (search-query (nth 2 (car (xml-get-children return 'searchQuery))))
         (r-e (car (xml-get-children return 'resultElements)))
         (items (xml-get-children r-e 'item))
         (retval '()))

    (while items
      (let* ((item (car items))
             (hostname (nth 2 (car (xml-get-children item 'hostName))))
             (url (nth 2 (car (xml-get-children item 'URL))))
             (title (nth 2 (car (xml-get-children item 'title))))
             (snippet (nth 2 (car (xml-get-children item 'snippet))))
             (cached-size (nth 2 (car (xml-get-children item 'cachedSize))))
             (retval-item '()))

        (add-to-list 'retval-item url t)
        (add-to-list 'retval-item title t)
        (add-to-list 'retval-item hostname t)
        (add-to-list 'retval-item cached-size t)
        (add-to-list 'retval-item snippet t)

        (add-to-list 'retval retval-item)

        (setq items (cdr items))))

    (cons search-query retval)))

(defun google-search (terms)
  "Search for TERMS."
  (interactive "sGoogle search: ")
  (google-display-response
   (google-process-response
    (google-search-internal terms google-start google-max-results
                            google-filter-p google-safe-p))))

(defun google-search-region (beg end)
  "Perform a Google search on the words from BEG to END."
  (interactive "r")
  (google-search (buffer-substring-no-properties beg end)))

(provide 'google)
;;; google.el ends here
