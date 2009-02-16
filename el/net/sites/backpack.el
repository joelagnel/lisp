;;; backpack.el --- Emacs interface to 37signals' Backpack

;; Copyright (C) 2005  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

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

;; backpack.el is intended to be an Emacs interface to Backpack. At the
;; moment, it's an undocumented, fairly bare-bones, but complete
;; implementation of the Backpack API. I'll be adding a UI soon.

;; Please set `backpack-username' and `backpack-api-key' appropriately.

;; I've been developing this under a fairly recent build of Emacs' CVS
;; HEAD. There are known incompatibilities between this code and XEmacs'
;; versions of url.el and xml.el.

;; You may have noticed the lack of adequate documentation. It's on the
;; way, but for now, you can use the Backpack API docs:

;;                <URL:http://www.backpackit.com/api/>

;; Also, here are unofficial docs for the new API methods pertaining to
;; multiple-lists-per-page:

;;  <URL:http://danontopic.com/public/pages/backpackApiLists.html>

;; Share and Enjoy!

;;; History:
;; 2006-02-27 - Add support for multiple lists per page.
;; 2005-11-01 - Use plists for formatting.
;; 2005-10-15 - Initial version.

;;; Code:

(require 'rest-api) ;; http://edward.oconnor.cx/elisp/rest-api.el
(require 'url)      ;; Tested with the URL package in CVS Emacs
(require 'xml)      ;; xml.el in CVS Emacs

;; Placate the byte-compiler.
(defvar url-http-end-of-headers)

;; * User-serviceable parts.

(defgroup backpack nil
  "Emacs interface to 37signals' Backpack."
  :group 'processes
  :prefix "backpack-"
  :link '(url-link :tag "Backpack"
                   "http://backpackit.com/?referrer=BPWJ9")
  :link '(url-link :tag "Latest version of backpack.el"
                   "http://edward.oconnor.cx/elisp/backpack.el")
  :link '(url-link :tag "Backpack API documentation"
                   "http://www.backpackit.com/api/"))

(defcustom backpack-username nil
  "Your Backpack username."
  :group 'backpack
  :type '(string))

(defcustom backpack-api-key nil
  "Your Backpack API key."
  :group 'backpack
  :type '(string))

;; * HTTP request/response handling

(put 'backpack-error 'error-message "Backpack error")
(put 'backpack-error 'error-conditions '(backpack-error error))

(defun backpack-check-error (response)
  "Check to see if RESPONSE is actually an API error.
If it is, signal the error."
  (unless (string-equal "true" (xml-get-attribute response 'success))
    (let* ((error-node (car (xml-get-children response 'error)))
           (code (xml-get-attribute error-node 'code))
           (message (rest-api-join (xml-node-children error-node))))
      (signal 'backpack-error (list code message)))))

(defvar backpack-debug nil)

(defun backpack-response (buffer)
  "Process the XML response from Backpack which resides in BUFFER."
  (unwind-protect
      (with-current-buffer buffer
        (save-excursion
          (goto-char url-http-end-of-headers)
          (let ((response (car (xml-parse-region (point) (point-max)))))
            (backpack-check-error response)
            response)))
    (unless backpack-debug
      (kill-buffer buffer))))

(defun backpack-request (path &optional payload)
  "Perform a Backpack API request to PATH.
PAYLOAD may contain extra arguments to certain API calls."
  (when (and payload (not (stringp payload)))
    (setq payload (rest-api-xmlify payload)))
  (let ((url-package-name "backpack.el")
        (url-request-method "POST")
        (url-request-extra-headers '(("X-POST_DATA_FORMAT" . "xml")))
        (url-request-data
         (format "<request><token>%s</token>%s</request>"
                 backpack-api-key (or payload "")))
        (backpack-api-host (format "%s.backpackit.com" backpack-username)))
    (backpack-response
     (condition-case nil
         (url-retrieve-synchronously
          (concat "http://" backpack-api-host "/ws" path))
       ;; Thrown when URL is unable to create a network process.
       (wrong-type-argument
        (signal 'backpack-error
                (list (format "Unable to connect to %s."
                              backpack-api-host))))))))

(put 'backpack-request 'lisp-indent-function 1)

;; * Massaging Backpack's XML into more idiomatic elisp structures

(defun backpack-parse-lists-list (lists)
  "Extract a nice list of lists from LISTS."
  (let (retval)
    (mapc (lambda (list)
            (let ((thing '())
                  (name (xml-get-attribute-or-nil list 'name))
                  (id (string-to-number (xml-get-attribute list 'id))))
              (when name
                (setq thing (plist-put thing :name name)))
              (when id
                (setq thing (plist-put thing :id id)))
              (push thing retval)))
          lists)
    (nreverse retval)))

(defun backpack-parse-items-list (items-element)
  "Extract a nice list of items from ITEMS-ELEMENT."
  (let ((items (xml-get-children items-element 'item))
        retval)
    (mapc (lambda (item)
            (let ((thing '())
                  (completed
                   (xml-get-attribute-or-nil item 'completed))
                  (id
                   (string-to-number (xml-get-attribute item 'id)))
                  (list-id
                   (string-to-number (xml-get-attribute item 'list_id)))
                  (content (rest-api-join (xml-node-children item))))
              (when completed
                (setq thing (plist-put thing :completed completed)))
              (when id
                (setq thing (plist-put thing :id id)))
              (when list-id
                (setq thing (plist-put thing :list-id list-id)))
              (when content
                (setq thing (plist-put thing :content content)))
              (push thing retval)))
          items)
    (nreverse retval)))

(defun backpack-parse-notes-list (notes-element)
  "Extract a nice list of notes from NOTES-ELEMENT."
  (let ((notes (xml-get-children notes-element 'note))
        retval)
    (mapc (lambda (note)
            (let ((thing '())
                  (title (xml-get-attribute-or-nil note 'title))
                  (id
                   (string-to-number (xml-get-attribute note 'id)))
                  (created-at
                   (xml-get-attribute-or-nil note 'created_at))
                  (content (rest-api-join (xml-node-children note))))
              (when title
                (setq thing (plist-put thing :title title)))
              (when id
                (setq thing (plist-put thing :id id)))
              (when created-at
                (setq thing (plist-put thing :created-at created-at)))
              (when content
                (setq thing (plist-put thing :content content)))
              (push thing retval)))
          notes)
    (nreverse retval)))

(defun backpack-parse-pages-list (pages-element)
  "Extract a nice list of pages from PAGES-ELEMENT."
  (let ((pages (xml-get-children pages-element 'page))
        retval)
    (mapc (lambda (page)
            (let ((thing '())
                  (title (xml-get-attribute-or-nil page 'title))
                  (id (string-to-number (xml-get-attribute page 'id)))
                  (email (xml-get-attribute-or-nil page 'email_address))
                  (scope (xml-get-attribute-or-nil page 'scope))
                  (body
                   (rest-api-join
                    (xml-node-children
                     (car (xml-get-children page 'description)))))
                  (items (backpack-parse-items-list
                          (car (xml-get-children page 'items))))
                  (notes (backpack-parse-notes-list
                          (car (xml-get-children page 'notes))))
                  (linked-pages
                   (backpack-parse-pages-list
                    (car (xml-get-children page 'linked_pages))))
                  (tags (backpack-parse-tags-list
                         (car (xml-get-children page 'tags)))))
              (when title
                (setq thing (plist-put thing :title title)))
              (when id
                (setq thing (plist-put thing :id id)))
              (when email
                (setq thing (plist-put thing :email email)))
              (when scope
                (setq thing (plist-put thing :scope scope)))
              (when (and body (stringp body) (> (length body) 0))
                (setq thing (plist-put thing :body body)))
              (when items
                (setq thing (plist-put thing :items items)))
              (when notes
                (setq thing (plist-put thing :notes noes)))
              (when linked-pages
                (setq thing (plist-put thing :linked-pages linked-pages)))
              (when tags
                (setq thing (plist-put thing :tags tags)))
              (push thing retval)))
          pages)
    (nreverse retval)))

(defun backpack-parse-tags-list (tags-element)
  "Extract a nice list of tags from TAGS-ELEMENT."
  (let ((tags (xml-get-children tags-element 'tag))
        retval)
    (mapc (lambda (tag)
            (let ((thing '())
                  (name (xml-get-attribute-or-nil tag 'name))
                  (id
                   (string-to-number (xml-get-attribute tag 'id))))
              (when name
                (setq thing (plist-put thing :name name)))
              (when id
                (setq thing (plist-put thing :id id)))
              (push thing retval)))
          tags)
    (nreverse retval)))

(defun backpack-parse-reminders-list (reminders-element)
  "Extract a nice list of reminders from REMINDERS-ELEMENT."
  (let ((reminders (xml-get-children reminders-element 'reminder))
        retval)
    (mapc (lambda (reminder)
            (let ((thing '())
                  (remind-at
                   (xml-get-attribute-or-nil reminder 'remind_at))
                  (id (string-to-number
                       (xml-get-attribute reminder 'id)))
                  (content
                   (rest-api-join (xml-node-children reminder))))
              (when remind-at
                (setq thing (plist-put thing :remind-at remind-at)))
              (when id
                (setq thing (plist-put thing :id id)))
              (when content
                (setq thing (plist-put thing :content content)))
              (push thing retval)))
          reminders)
    (nreverse retval)))

(defun backpack-parse-emails-list (emails-element)
  "Extract a nice list of emails from EMAILS-ELEMENT."
  (let ((emails (xml-get-children emails-element 'email))
        retval)
    (mapc (lambda (email)
            (let ((thing '())
                  (id
                   (string-to-number (xml-get-attribute email 'id)))
                  (created-at
                   (xml-get-attribute-or-nil email 'created_at))
                  (subject (xml-get-attribute-or-nil email 'subject))
                  (content
                   (rest-api-join (xml-node-children email))))
              (when id
                (setq thing (plist-put thing :id id)))
              (when created-at
                (setq thing (plist-put thing :created-at created-at)))
              (when subject
                (setq thing (plist-put thing :subject subject)))
              (when content
                (setq thing (plist-put thing :content content)))
              (push thing retval)))
          emails)
    (nreverse retval)))

;; * Defining elisp functions corresponding to each API method

(defmacro backpack-api-define (call args payload-args docstring
                               &optional payload)
  "Define an elisp function for CALL, a Backpack API method.

CALL should be in the form of a format string, to which ARGS will be
applied. PAYLOAD-ARGS are additional function arguments. CALL should be
documented with DOCSTRING. PAYLOAD, if specified, will be sent to
Backpack in the request."
  (let ((name (intern
               (concat "backpack-api"
                       (replace-regexp-in-string "/%s" "" call)))))
    `(defun ,name ,(append args payload-args)
       ,docstring
       (backpack-request (format ,call ,@args)
         ,payload)
       t)))

(put 'backpack-api-define 'lisp-indent-function 3)
(put 'backpack-api-define 'doc-string-elt 4)

;; ** Page listing

(defun backpack-api/pages/all ()
  "Return a list of all Backpack pages you have access to."
  (let ((r (backpack-request "/pages/all")))
    (backpack-parse-pages-list (car (xml-get-children r 'pages)))))

;; ** Pages

(defun backpack-api/page (page-id)
  "Fetch the Backpack page identified by PAGE-ID."
  (let ((r (backpack-request (format "/page/%s" page-id))))
    (car (backpack-parse-pages-list r))))

(backpack-api-define "/page/%s/destroy" (page-id) ()
  "Delete the page identified by PAGE-ID.")

(backpack-api-define "/page/%s/update_title" (page-id) (title)
  "Set the title of PAGE-ID's page to TITLE."
  `((page () (title () ,title))))

(backpack-api-define "/page/%s/update_body" (page-id) (body)
  "Set the body of PAGE-ID's page to BODY."
  `((page () (description () ,body))))

(defun backpack-api/page/duplicate (page-id)
  "Create a duplicate of PAGE-ID's page."
  (let ((r (backpack-request (format "/page/%s/duplicate" page-id))))
    (backpack-parse-pages-list r)))

(backpack-api-define "/page/%s/link" (page-id) (linked-page-id)
  "Create a link on PAGE-ID to LINKED-PAGE-ID."
  `((linked_page_id () ,linked-page-id)))

(backpack-api-define "/page/%s/unlink" (page-id) (linked-page-id)
  "Delete the link on PAGE-ID to LINKED-PAGE-ID."
  `((linked_page_id () ,linked-page-id)))

(backpack-api-define "/page/%s/share" (page-id)
                     (emails &optional public)
  "Share PAGE-ID with the emails in EMAILS.
Optionally, set the status of the page with PUBLIC (1 or 0)."
  `((email_addresses () ,emails)
    ,(if (numberp public) (list 'page () (list 'public () public)) "")))

(backpack-api-define "/page/%s/unshare_friend_page" (page-id) ()
  "FIXME: I don't know what this does to PAGE-ID.")

(backpack-api-define "/page/%s/email" (page-id) ()
  "Email PAGE-ID to yourself.")

;; ** Items - legacy (one page, one list) interface

(defun backpack-api/page/items/list (page-id &optional list-id)
  "Return a list of items on PAGE-ID."
  (let ((r (backpack-request
               (concat
                (format "/page/%s/items/list" page-id)
                (if list-id (format "?list_id=%s" list-id) "")))))
    (backpack-parse-items-list (car (xml-get-children r 'items)))))

(defun backpack-api/page/items/add (page-id content &optional list-id)
  "Create a new item on PAGE-ID with CONTENT.
If non-null, add the item to LIST-ID."
  (let ((r (backpack-request
               (concat
                (format "/page/%s/items/add" page-id)
                (if list-id (format "?list_id=%s" list-id) ""))
             `((item () (content () ,content))))))
    (car (backpack-parse-items-list r))))

(backpack-api-define "/page/%s/items/update/%s" (page-id item-id)
                     (content)
  "Replace the content of PAGE-ID's ITEM-ID with CONTENT."
  `((item () (content () ,content))))

(backpack-api-define "/page/%s/items/toggle/%s" (page-id item-id) ()
  "Toggle the completion status of PAGE-ID's ITEM-ID.")

(backpack-api-define "/page/%s/items/destroy/%s" (page-id item-id) ()
  "Delete PAGE-ID's ITEM-ID.")

(backpack-api-define "/page/%s/items/move/%s" (page-id item-id)
                     (direction)
  "Delete PAGE-ID's ITEM-ID."
  `((direction () ,direction)))

;; ** Lists

(defun backpack-api/page/lists/add (page-id name)
  "Add a new list to PAGE-ID, named NAME."
  (let* ((r (backpack-request (format "/page/%s/lists/add" page-id)
              `((name () ,name)))))
    (car (backpack-parse-lists-list (xml-get-children r 'list)))))

(backpack-api-define "/page/%s/lists/update/%s" (page-id list-id) (name)
  "Update the name of PAGE-ID's LIST-ID."
  `((list ()
      (name () ,name))))

(defun backpack-api/page/lists/list (page-id)
  "Fetch a list of PAGE-ID's lists."
  (let* ((r (backpack-request (format "/page/%s/lists/list" page-id))))
    (backpack-parse-lists-list
     (xml-get-children (car (xml-get-children r 'lists)) 'list))))

(backpack-api-define "/page/%s/lists/destroy/%s" (page-id list-id) ()
  "Destroy PAGE-ID's LIST-ID.")

;; ** Notes

(defun backpack-api/page/notes/list (page-id)
  "Lists the notes of the page identified by PAGE-ID."
  (let* ((r (backpack-request (format "/page/%s/notes/list" page-id))))
    (backpack-parse-notes-list (car (xml-get-children r 'notes)))))

(defun backpack-api/page/notes/create (page-id title &optional body)
  "Create a new note on the page identified by PAGE-ID.
The note will be named TITLE. Its body will be taken from BODY."
  (let ((r (backpack-request (format "/page/%s/notes/create" page-id)
              `((note ()
                (title () ,title)
                ,(if body (list 'body () body) ""))))))
    (backpack-parse-notes-list r)))

(backpack-api-define "/page/%s/notes/update/%s" (page-id note-id)
                     (title &optional body)
  "On PAGE-ID, update NOTE-ID's  TITLE and possibly BODY."
  `((note ()
      (title () ,title)
      ,(if body (list 'body () body) ""))))

(backpack-api-define "/page/%s/notes/destroy/%s" (page-id note-id) ()
  "Delete a note from a page.
Page identified by PAGE-ID, note identified by NOTE-ID.")

;; ** Tags

(defun backpack-api/tags/select (tag-id)
  "Return a list of pages tagged with the tag identified by TAG-ID."
  (let ((r (backpack-request (format "/tags/select/%s" tag-id))))
    (backpack-parse-pages-list (car (xml-get-children r 'pages)))))

;; FIXME: doesn't seem to work. hmm.
(backpack-api-define "/page/%s/tags/tag" (page-id) (tags)
  "Tag page (whose id is PAGE-ID) with TAGS."
  ;; FIXME: quote tags with spaces (and quotes) in them
  `((tags () ,(rest-api-join tags))))

;; ** Reminders

(defun backpack-api/reminders ()
  "Fetch a list of your Backpack reminders."
  (let* ((r (backpack-request "/reminders")))
    (backpack-parse-reminders-list
     (car (xml-get-children r 'reminders)))))

(defun backpack-api/reminders/create (content &optional remind-at)
  "Create a new reminder containing CONTENT (and its REMIND-AT)."
  (let ((r (backpack-request "/reminders/create"
             `((reminder ()
               (content () ,content)
               ,(if remind-at (list 'remind_at () remind-at) ""))))))
    (car (backpack-parse-reminders-list r))))

(backpack-api-define "/reminders/update/%s" (reminder-id)
                     (content &optional remind-at)
  "Update REMINDER-ID's CONTENT (and optionally its REMIND-AT)."
  `((reminder ()
      (content () ,content)
      ,(if remind-at (list 'remind_at () remind-at) ""))))

(backpack-api-define "/reminders/destroy/%s" (reminder-id) ()
  "Destroy the reminder identified by REMINDER-ID.")

;; ** Emails

(defun backpack-api/page/emails/list (page-id)
  "List PAGE-ID's emails."
  (let ((r (backpack-request (format "/page/%s/emails/list" page-id))))
    (backpack-parse-emails-list (car (xml-get-children r 'emails)))))

(defun backpack-api/page/emails/show (page-id email-id)
  "On page PAGE-ID, fetch the email identified by EMAIL-ID."
  (let ((r (backpack-request (format "/page/%s/emails/show/%s"
                                      page-id email-id))))
    (car (backpack-parse-emails-list r))))

(backpack-api-define "/page/%s/emails/destroy/%s" (page-id email-id) ()
  "Delete PAGE-ID's email identified by EMAIL-ID.")

;; ** Account

(defun backpack-api/account/export ()
  "Fetch a full export of your Backpack account."
  (let* ((r (backpack-request "/account/export"))
         (pages-element (car (xml-get-children r 'pages)))
         (reminders-element (car (xml-get-children r 'reminders))))
    `((pages ,(backpack-parse-pages-list pages-element))
      (reminders ,(backpack-parse-reminders-list reminders-element)))))

;; * UI

(defvar backpack-pages nil
  "An alist mapping Backpack page names to page IDs.")

(defun backpack-pages (&optional invalidate-cache)
  "Returns an alist mapping Backpack page names to page IDs."
  (when invalidate-cache
    (setq backpack-pages nil))
  (or backpack-pages
      (setq backpack-pages
            (mapcar (lambda (page)
                      (cons (plist-get page :title)
                            (plist-get page :id)))
                    (backpack-api/pages/all)))))

(defvar backpack-last-page nil
  "The last page you completed to.")

(defun backpack-read-page ()
  "Prompts the user for a name of one of their Backpack pages."
  (setq backpack-last-page
        (completing-read "Page: " (backpack-pages) nil t
                         backpack-last-page)))

(defun backpack-remind (what)
  "Create a reminder (of WHAT)."
  (interactive "sRemind: ")
  (let ((reminder (backpack-api/reminders/create (concat "+180 " what))))
    (message "Created reminder %s." (plist-get reminder :id))))

(defun backpack-remind-from-region (start end)
  "Create a reminder consisting of the text from START to END."
  (interactive "r")
  (backpack-remind (buffer-substring start end)))



;; Local Variables:
;; indent-tabs-mode: nil
;; mode: outline-minor
;; outline-regexp: ";;\\( [*]+\\|; [^*\n]\\)"
;; End:

(provide 'backpack)
;;; backpack.el ends here
