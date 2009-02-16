;;; technorati.el --- interface to the Technorati API for Emacs

;; Copyright (C) 2005  Edward O'Connor

;; Author: Edward O'Connor <ted@evdb.com>
;; Keywords: convenience

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; History:
;; 

;;; Code:

(require 'rest-api) ;; http://edward.oconnor.cx/elisp/rest-api.el
(require 'url)      ;; Tested with the URL package in CVS Emacs
(require 'xml)      ;; xml.el in CVS Emacs

;; Placate the byte-compiler.
(defvar url-http-end-of-headers)

;; * User-serviceable parts.

(defgroup technorati nil
  "Emacs interface to Technorati's REST API."
  :group 'processes
  :prefix "technorati-")

(defcustom technorati-api-key nil
  "Your Technorati API key."
  :group 'technorati
  :type '(string))

;; * TAPI XML parsing

(defun tapi-parse-text (node)
  "Extract the text out of NODE."
  (rest-api-join (xml-node-children node)))

(defalias 'tapi-parse-date 'tapi-parse-text) ;; FIXME
(defalias 'tapi-parse-url 'tapi-parse-text) ;; FIXME?

(defun tapi-parse-number (node)
  "Extract the number contained in NODE."
  (string-to-number (tapi-parse-text node)))

(defun tapi-parse-author (author)
  "Parse the TAPI AUTHOR element."
  (let ((username (car (xml-get-children author 'username)))
        (firstname (car (xml-get-children author 'firstname)))
        (lastname (car (xml-get-children author 'lastname)))
        (thumbnailpicture (car (xml-get-children author 'thumbnailpicture)))
        retval)
    (when username
      (add-to-list 'retval (list :username (tapi-parse-text username))))
    (when firstname
      (add-to-list 'retval (list :firstname (tapi-parse-text firstname))))
    (when lastname
      (add-to-list 'retval (list :lastname (tapi-parse-text lastname))))
    (when thumbnailpicture
      (add-to-list 'retval (list :thumbnailpicture
                                 (tapi-parse-url thumbnailpicture))))
    retval))

(defun tapi-parse-weblog (weblog)
  "Parse the TAPI WEBLOG element."
  (let ((name (car (xml-get-children weblog 'name)))
        (url (car (xml-get-children weblog 'url)))
        (rssurl (car (xml-get-children weblog 'rssurl)))
        (atomurl (car (xml-get-children weblog 'atomurl)))
        (inboundblogs (car (xml-get-children weblog 'inboundblogs)))
        (inboundlinks (car (xml-get-children weblog 'inboundlinks)))
        (lastupdate (car (xml-get-children weblog 'lastupdate)))
        (author (car (xml-get-children weblog 'author)))
        (rank (car (xml-get-children weblog 'rank)))
        (lang (car (xml-get-children weblog 'lang)))
        (lat (car (xml-get-children weblog 'lat)))
        (lon (car (xml-get-children weblog 'lon)))
        (foafurl (car (xml-get-children weblog 'foafurl)))
        retval)
    (when name
      (add-to-list 'retval (list :name (tapi-parse-text name))))
    (when url
      (add-to-list 'retval (list :url (tapi-parse-url url))))
    (when rssurl
      (add-to-list 'retval (list :rssurl (tapi-parse-url rssurl))))
    (when atomurl
      (add-to-list 'retval (list :atomurl (tapi-parse-url atomurl))))
    (when inboundblogs
      (add-to-list 'retval (list :inboundblogs
                                (tapi-parse-number inboundblogs))))
    (when inboundlinks
      (add-to-list 'retval (list :inboundlinks
                                (tapi-parse-number inboundlinks))))
    (when lastupdate
      (add-to-list 'retval (list :lastupdate (tapi-parse-date lastupdate))))
    (when author
      (add-to-list 'retval (list :author (tapi-parse-author author))))
    (when rank
      (add-to-list 'retval (list :rank (tapi-parse-number rank))))
    (when lang
      (add-to-list 'retval (list :lang (tapi-parse-text lang))))
    (when lat
      (add-to-list 'retval (list :lat (tapi-parse-number lat))))
    (when lon
      (add-to-list 'retval (list :lon (tapi-parse-number lon))))
    (when foafurl
      (add-to-list 'retval (list :foafurl (tapi-parse-url foafurl))))
    retval))

(defun tapi-parse-item (item)
  "Parse the TAPI ITEM element."
  (let ((tag (car (xml-get-children item 'tag)))
        (posts (car (xml-get-children item 'posts)))
        (weblog (car (xml-get-children item 'weblog)))
        (title (car (xml-get-children item 'title)))
        (excerpt (car (xml-get-children item 'excerpt)))
        (created (car (xml-get-children item 'created)))
        (postupdate (car (xml-get-children item 'postupdat)))
        (permalink (car (xml-get-children item 'permalink)))
        (nearestpermalink (car (xml-get-children item 'nearestpermalink)))
        (linkcreated (car (xml-get-children item 'linkcreated)))
        (linkurl (car (xml-get-children item 'linkurl)))
        retval)
    (when tag
      (add-to-list 'retval (list :tag (tapi-parse-text tag))))
    (when posts
      (add-to-list 'retval (list :posts (tapi-parse-number posts))))
    (when weblog
      (add-to-list 'retval (list :weblog (tapi-parse-weblog weblog))))
    (when title
      (add-to-list 'retval (list :title (tapi-parse-text title))))
    (when excerpt
      (add-to-list 'retval (list :excerpt (tapi-parse-text excerpt))))
    (when created
      (add-to-list 'retval (list :created (tapi-parse-date created))))
    (when postupdate
      (add-to-list 'retval (list :postupdate
                                (tapi-parse-postupdate postupdate))))
    (when permalink
      (add-to-list 'retval (list :permalink (tapi-parse-url permalink))))
    (when nearestpermalink
      (add-to-list 'retval (list :nearestpermalink
                                (tapi-parse-url nearestpermalink))))
    (when linkcreated
      (add-to-list 'retval (list :linkcreated (tapi-parse-date linkcreated))))
    (when linkurl
      (add-to-list 'retval (list :linkurl (tapi-parse-url linkurl))))
    retval))

(defun tapi-parse-result (result)
  "Parse the TAPI RESULT element."
  (let ((errors (car (xml-get-children result 'error))))
    (if errors
        (tapi-parse-text errors)
      (let ((urls (car (xml-get-children result 'url)))
            (weblogs (xml-get-children result 'weblog))
            (inboundblogs (car (xml-get-children result 'inboundblogs)))
            (inboundlinks (car (xml-get-children result 'inboundlinks)))
            (rankingstart (car (xml-get-children result 'rankingstart)))
            (items (xml-get-children result 'item))
            (apiqueries (car (xml-get-children result 'apiqueries)))
            (maxqueries (car (xml-get-children result 'maxqueries)))
            (querycount (car (xml-get-children result 'querycount)))
            (postsmatched (car (xml-get-children result 'postsmatched)))
            (blogsmatched (car (xml-get-children result 'blogsmatched)))
            (start (car (xml-get-children result 'start)))
            (limit (car (xml-get-children result 'limit)))
            (querytime (car (xml-get-children result 'querytime)))
            (query (car (xml-get-children result 'query)))
            (username (car (xml-get-children result 'username)))
            (firstname (car (xml-get-children result 'firstname)))
            (lastname (car (xml-get-children result 'lastname)))
            (thumbnailpicture
             (car (xml-get-children result 'thumbnailpicture)))
            retval)
        (when urls
          (add-to-list 'retval
                 (list :url (tapi-parse-url urls))))
        (when weblogs
          (add-to-list 'retval
                 (list :weblogs (mapcar 'tapi-parse-weblog weblogs))))
        (when inboundblogs
          (add-to-list 'retval
                 (list :inboundblogs (tapi-parse-number inboundblogs))))
        (when inboundlinks
          (add-to-list 'retval
                 (list :inboundlinks (tapi-parse-number inboundlinks))))
        (when rankingstart
          (add-to-list 'retval
                 (list :rankingstart
                              (tapi-parse-number rankingstart))))
        (when items
          (add-to-list 'retval
                 (list :items (mapcar 'tapi-parse-item items))))
        (when apiqueries
          (add-to-list 'retval
                 (list :apiqueries (tapi-parse-number apiqueries))))
        (when maxqueries
          (add-to-list 'retval
                 (list :maxqueries (tapi-parse-number maxqueries))))
        (when querycount
          (add-to-list 'retval
                 (list :querycount (tapi-parse-number querycount))))
        (when postsmatched
          (add-to-list 'retval
                 (list :postsmatched (tapi-parse-number postsmatched))))
        (when blogsmatched
          (add-to-list 'retval
                 (list :blogsmatched (tapi-parse-number blogsmatched))))
        (when start
          (add-to-list 'retval
                 (list :start (tapi-parse-number start))))
        (when limit
          (add-to-list 'retval
                 (list :limit (tapi-parse-number limit))))
        (when query
          (add-to-list 'retval
                 (list :query (tapi-parse-text query))))
        (when querytime
          (add-to-list 'retval
                 (list :querytime (tapi-parse-number querytime))))
        (when username
          (add-to-list 'retval
                 (list :username (tapi-parse-text username))))
        (when firstname
          (add-to-list 'retval
                 (list :firstname (tapi-parse-text firstname))))
        (when lastname
          (add-to-list 'retval
                 (list :lastname (tapi-parse-text lastname))))
        (when thumbnailpicture
          (add-to-list 'retval
                 (list :thumbnailpicture (tapi-parse-url thumbnailpicture))))
        retval))))

(defun tapi-parse-document (document)
  "Parse DOCUMENT, a Technorati API TAPI document."
  (list :result (tapi-parse-result
                 (car (xml-get-children document 'result)))
        :items (mapcar 'tapi-parse-item
                       (xml-get-children document 'item))))

(defun tapi-parse-tapi (tapi)
  "Parse the TAPI TAPI element."
  (tapi-parse-document (car (xml-get-children tapi 'document))))

;; * HTTP request/response handling

(put 'technorati-error 'error-message "Technorati API error")
(put 'technorati-error 'error-conditions '(technorati-error error))

(defvar technorati-debug nil)

(defun technorati-check-error (response)
  "Check for an error in RESPONSE.
If an error is found, signal the error."
  (let ((error-node (car (xml-get-children
                          (car (xml-get-children
                                (car (xml-get-children response 'document))
                                'result))
                          'error))))
    (when error-node
      (signal 'technorati-error
              (list (tapi-parse-text error-node))))))

(defun technorati-response (buffer)
  "Process the XML response from Technorati which resides in BUFFER."
  (unwind-protect
      (with-current-buffer buffer
        (save-excursion
          (goto-char url-http-end-of-headers)
          (let ((response (xml-parse-region (point) (point-max) nil nil nil)))
            (setq response (car response))
            (technorati-check-error response)
            (tapi-parse-tapi response))))
    (unless technorati-debug
      (kill-buffer buffer))))

(defun technorati-request (call args &optional http-method)
  "Perform a Technorati API request to CALL with ARGS using HTTP-METHOD."
  (let ((url-package-name "technorati.el")
        (url-request-method (or http-method "POST"))
        (extra-args (list (cons 'key technorati-api-key))))
    (setq args (append args extra-args))
    (technorati-response
     (url-retrieve-synchronously
      (concat "http://api.technorati.com/" call "?"
              (rest-api-format-url-parameters args))))))

;; * Defining elisp functions corresponding to each API method

(defmacro technorati-api-define (method mandatory-args optional-args docstring)
  "Define an elisp function for Technorati API METHOD.
MANDATORY-ARGS are required by METHOD; OPTIONAL-ARGS are not.
DOCSTRING describes METHOD."
  (let ((func-name (intern (concat "technorati-api/" method)))
        (func-args mandatory-args)
        (args-sym (make-symbol "args")))
    (when optional-args
      (setq func-args (append func-args (list '&optional) optional-args)))
    `(defun ,func-name ,func-args
       ,docstring
       (let ((,args-sym (list ,@(mapcar (lambda (arg)
                                          (list 'cons (list 'quote arg) arg))
                                        mandatory-args))))
         (mapc (lambda (other-arg)
                 (when (symbol-value other-arg)
                   (setq ,args-sym
                         (cons (cons other-arg (symbol-value other-arg))
                               ,args-sym))))
               ',optional-args)
         (technorati-request ,method ,args-sym)))))

(put 'technorati-api-define 'lisp-indent-function 3)
(put 'technorati-api-define 'doc-string-elt 4)

(technorati-api-define "bloginfo" (url) (format)
  "http://developers.technorati.com/wiki/BlogInfoQuery")

(technorati-api-define "blogposttags" (url) (limit)
  "http://developers.technorati.com/wiki/BlogPostTags")

(technorati-api-define "cosmos" (url)
                       (type limit start current format claim highlight)
  "http://developers.technorati.com/wiki/CosmosQuery")

(technorati-api-define "getinfo" (username) (format)
  "http://developers.technorati.com/wiki/GetInfoQuery")

(technorati-api-define "keyinfo" () ()
  "http://developers.technorati.com/wiki/KeyInfoQuery")

(technorati-api-define "outbound" (url) (format start)
  "http://developers.technorati.com/wiki/OutboundQuery")

(technorati-api-define "search" (query) (language start limit format claim)
  "http://developers.technorati.com/wiki/SearchQuery")

(technorati-api-define "tag" (tag)
                       (limit start format excerptsize topexcerptsize)
  "http://developers.technorati.com/wiki/TagQuery")

(technorati-api-define "toptags" () (limit start format)
  "http://developers.technorati.com/wiki/TopTagsQuery")

(provide 'technorati)
;;; technorati.el ends here
