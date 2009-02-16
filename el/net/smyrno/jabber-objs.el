;;; jabber-objs.el --- Jabber object definitions

;; Copyright (C) 2002  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: processes, comm, tools

;; This file is part of Smyrno, a Jabber client for Emacs.

;; Smyrno is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Smyrno is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

;;; Object & method definitions.

(defmacro jabber-define-object (object &rest fieldsyms)
  "Define a new Jabber object OBJECT with FIELDSYMS."

  ;; Note that this is a big and scary macro-defining-macro. Why
  ;; are we doing this when `defstruct' already exists? To avoid
  ;; runtime need of CL. Also, we get the cool-ass
  ;; `jabber-FOO-bind' macro, which totally rox0rs.

  ;; I am not proud.

  (let* ((name (symbol-name object))
         (prefix (concat "jabber-" name))
         (prefixsym (intern prefix))
         (predicate (intern (concat prefix "-p")))
         (constructor (intern (concat prefix "-new")))
         (fields (mapcar (lambda (fieldsym)
                           (symbol-name fieldsym))
                         fieldsyms))
         (getters (mapcar (lambda (field)
                            (intern (concat prefix "-" field)))
                          fields))
         (setters (mapcar (lambda (field)
                            (intern (concat prefix "-" field "-set")))
                          fields))
         (binder (intern (concat prefix "-bind"))))

    `(progn

       (defun ,predicate (obj)
         ,(format "When non-nil, OBJ is a %s." name)
         (and (vectorp obj)
              (= (length obj) ,(+ (length getters) 1))
              (eq (aref obj 0) ',prefixsym)))

       (defun ,constructor (&optional ,@fieldsyms)
         ,(format "Return a new %s with the specified field values."
                  name)
         (vector ',prefixsym ,@fieldsyms))

       ,@(let ((count 0))
           (mapcar (lambda (getter)
                     (setq count (+ 1 count))
                     (list 'defun getter '(obj)
                           (list 'aref 'obj count)))
                   getters))

       ,@(let ((count 0))
           (mapcar (lambda (setter)
                     (setq count (+ 1 count))
                     (list 'defun setter '(obj new-value)
                           (list 'aset 'obj count 'new-value)))
                   setters))

       (defmacro ,binder (obj &rest body)
         "Bind the fields of OBJ to convenient names, then evaluate BODY."
         (nconc
          (list 'let
                (list (list 'obj obj)))
          (list
           (nconc
            (list
             'let
             (list ,@(let ((count 0))
                       (mapcar
                        (lambda (fs)
                          (let ((fieldsym (nth count fieldsyms))
                                (getter (nth count getters)))
                            (setq count (+ count 1))
                            (list 'list
                                  (list 'quote fieldsym)
                                  (list 'quote
                                        (list getter 'obj)))))
                        fieldsyms))))
            body))))
       (put ',binder 'lisp-indent-function 1)

       ,name)))
(put 'jabber-define-object 'lisp-indent-function 1)

(jabber-define-object jid
  user host resource)

(jabber-define-object message
  ;; "Official" fields
  type from id to
  body error html subject thread x
  ;; My own fields
  date   ; date message received
  unread ; Non-null if this message has not been read
)

(jabber-define-object presence
  ;; "Official" fields"
  type from id to
  priority status show x
  ;; should this be here?
  error
  )

(jabber-define-object iq
  ;; "Official" fields"
  type from id to
  error key query vCard)

(jabber-define-object error
  ;; "Official" fields"
  code text)

;; This is the object that comes over the wire.
(jabber-define-object roster-entry
  ;; "Official" fields"
  jid name subscription groups
  ;; My own fields
  show status)

(defun jabber-jid-to-string (jid)
  "Return a string representation of JID."
  (let ((user (jabber-jid-user jid))
        (host (jabber-jid-host jid))
        (resource (jabber-jid-resource jid)))
    (if (stringp host)
        (let ((retval host))
          (when (stringp user)
            (setq retval (concat user "@" retval)))
          (when (stringp resource)
            (setq retval (concat retval "/" resource)))
          retval)
      (error
       "JID must have a non-empty host to generate a JID string %S"
       jid))))

(defun jabber-jid-from-string (jid-string &optional start)
  "Return a JID object from JID-STRING."
  (if (string-match
       (concat
        ;; User name
        "^\\(\\([^@/\n\r\t :\"]+\\)[@]\\)?"
        ;; Host name
        "\\([-A-Za-z0-9.]+\\)"
        ;; Resource
        "\\([/]\\(\\([\n\t\r]\\|.\\)*\\)\\)?$"
        )
       jid-string (or start 0))
      (jabber-jid-new (match-string 2 jid-string)
                      (match-string 3 jid-string)
                      (match-string 5 jid-string))
    (error "Provided string %S does not appear to be a valid JID"
           jid-string)))

(defun jabber-get-jid (jid-designator)
  "Return the JID object associated with JID-DESIGNATOR.
A jid-designator can be a jid object (and under such a degenerate case
this function simply returns its argument) or a string. The string may
be either a string representation of a jid or a nickname in the user's
roster."
  (cond ((jabber-jid-p jid-designator) jid-designator)
        ((stringp jid-designator
                  (or (jabber-lookup-nick jid-designator)
                      (jabber-lookup-jid-string jid-designator)
                      (error "Provided string %s does not appear to be \
a valid JID"
                             jid-designator))))
         something
         (error "Provided object %S does not appear to be a valid JID"
                jid-designator)))

(defun jabber-message-to-xml-sexp (msgobj)
  "Given a Jabber message object, create a sexp representation of some XML."
  (let ((attrs '())
        (children '()))
    (jabber-message-bind msgobj
      (when (symbolp type)
        (add-to-list 'attrs (cons 'type type)))
      (when (stringp id)
        (add-to-list 'attrs (cons 'id id)))
      (when (jabber-jid-p from)
        (add-to-list 'attrs (cons 'from (jabber-jid-to-string from))))
      (when (jabber-jid-p to)
        (add-to-list 'attrs (cons 'to (jabber-jid-to-string to))))
      (when (stringp body)
        (add-to-list 'children (list 'body nil (xe-escape body))))
      (when (jabber-error-p error)
        (add-to-list 'children
                     (jabber-error-to-xml-sexp error)))
      (when html
        (add-to-list 'children (list 'html nil html)))
      (when (stringp subject)
        (add-to-list 'children (list 'subject nil subject)))
      (when (stringp thread)
        (add-to-list 'children (list 'thread nil thread))))
    `(message ,attrs ,@children)))

(defun jabber-message-from-xml-sexp (xml)
  "Given a sexp representation of some XML, create a Jabber message object."
  (let ((retval (jabber-message-new))
        (attrs (xml-node-attributes xml))
        (children (xml-node-children xml))
        (x-list '()))

    (jabber-message-type-set   retval 'normal)
    (jabber-message-date-set   retval (current-time))
    (jabber-message-unread-set retval t)

    (mapc
     (lambda (attr)
       (let ((attr-name (car attr))
             (attr-value (cdr attr)))
         (cond
          ((eq attr-name 'type)
           (jabber-message-type-set retval (intern attr-value)))
          ((eq attr-name 'id)
           (jabber-message-id-set retval attr-value))
          ((eq attr-name 'from)
           (jabber-message-from-set
            retval
            (jabber-jid-from-string attr-value)))
          ((eq attr-name 'to)
           (jabber-message-to-set
            retval
            (jabber-jid-from-string attr-value))))))
     attrs)

    (mapc
     (lambda (child)
       (when (listp child)
         (let ((node-name (xml-node-name child))
               (node-attrs (xml-node-attributes child))
               (node-children (xml-node-children child)))
           (cond
            ((eq node-name 'body)
             (jabber-message-body-set retval
                                      (xe-unescape (apply 'concat
                                                          node-children))))
            ((eq node-name 'error)
             (jabber-message-error-set
              retval
              (jabber-error-from-xml-sexp child)))
            ((eq node-name 'html)
             (jabber-message-html-set retval node-children))
            ((eq node-name 'subject)
             (jabber-message-subject-set
              retval
              (apply 'concat node-children)))
            ((eq node-name 'thread)
             (jabber-message-thread-set retval
                                        (apply 'concat node-children)))
            ((eq node-name 'x)
             (add-to-list 'x-list child))))))
     children)

    (jabber-message-x-set retval x-list)

    (when (and (eq (jabber-message-type retval) 'chat)
               (or (not (jabber-message-thread retval))
                   (string-equal (jabber-message-thread retval) "")))
      (jabber-message-thread-set retval
                                 (downcase
                                  (jabber-jid-to-string
                                   (jabber-message-from retval)))))
    retval))

(defun jabber-presence-to-xml-sexp (msgobj)
  "Given a Jabber presence object, create a sexp representation of some XML."
  (let ((attrs '())
        (children '()))
    (jabber-message-bind msgobj
      (when type
        (add-to-list 'attrs (cons 'type type)))
      (when (stringp id)
        (add-to-list 'attrs (cons 'id id)))
      (when (jabber-jid-p from)
        (add-to-list 'attrs (cons 'from (jabber-jid-to-string from))))
      (when (jabber-jid-p to)
        (add-to-list 'attrs (cons 'to (jabber-jid-to-string to))))
      (when (stringp priority)
        (add-to-list 'children (list 'priority nil priority)))
      (when (jabber-error-p error)
        (add-to-list 'children
                     (jabber-error-to-xml-sexp error)))
      (when (symbolp show)
        (add-to-list 'children (list 'show nil show)))
      (when (stringp status)
        (add-to-list 'children (list 'status nil status))))
    `(presence ,attrs ,@children)))

(defun jabber-presence-from-xml-sexp (xml)
  "Given a sexp representation of some XML, create a Jabber presence object."
  (let ((retval (jabber-presence-new))
        (attrs (xml-node-attributes xml))
        (children (xml-node-children xml)))

    (jabber-presence-type-set retval 'available)
    (jabber-presence-show-set retval 'normal)
    (jabber-presence-status-set retval "")

    (mapc
     (lambda (attr)
       (let ((attr-name (car attr))
             (attr-value (cdr attr)))
         (cond
          ((eq attr-name 'type)
           (jabber-presence-type-set retval (intern attr-value)))
          ((eq attr-name 'id)
           (jabber-presence-id-set retval attr-value))
          ((eq attr-name 'from)
           (jabber-presence-from-set retval
                                    (jabber-jid-from-string attr-value)))
          ((eq attr-name 'to)
           (jabber-presence-to-set retval
                                  (jabber-jid-from-string attr-value))))))
     attrs)

    (mapc
     (lambda (child)
       (when (listp child)
         (let ((node-name (xml-node-name child))
               (node-attrs (xml-node-attributes child))
               (node-children (xml-node-children child)))
           (cond
            ((eq node-name 'priority)
             (jabber-presence-priority-set retval
                                           (apply 'concat node-children)))
            ((eq node-name 'error)
             (xmpp-log "Error issue: name %S attrs %S children %S"
                       node-name
                       node-attrs
                       node-children)
             (jabber-presence-error-set
              retval
              (jabber-error-from-xml-sexp child)))
            ((eq node-name 'show)
             (jabber-presence-show-set retval
                                       (intern
                                        (apply 'concat node-children))))
            ((eq node-name 'status)
             (jabber-presence-status-set retval
                                         (apply 'concat node-children)))
            ;; We're ignoring <x/> thingies for the time being.
            ))))
     children)

    retval))

(defun jabber-iq-to-xml-sexp (iqobj)
  "Given a Jabber iq object, create a sexp representation of some XML."
  (let ((attrs '())
        (children '()))
    (jabber-iq-bind iqobj
      (when (symbolp type)
        (add-to-list 'attrs (cons 'type type)))
      (when (stringp id)
        (add-to-list 'attrs (cons 'id id)))
      (when (jabber-jid-p from)
        (add-to-list 'attrs (cons 'from (jabber-jid-to-string from))))
      (when (jabber-jid-p to)
        (add-to-list 'attrs (cons 'to (jabber-jid-to-string to))))
      (when (stringp key)
        (add-to-list 'children (list 'key nil key)))
      ;; Only handle vCards if the user has `require'd xmpp-vcard.el.
      (when (and (featurep 'xmpp-vcard)
                 (listp vCard)
                 (not (null vCard))))
        ;; `something' should be a function which converts a
        ;; vcard.el-style list into a vcard-temp XML sexp.
        (add-to-list 'children (jabber-vcard-xml-sexp-from-vcard-sexp vCard)))
      (when (jabber-error-p error)
        (add-to-list 'children
                     (jabber-error-to-xml-sexp error))))
    `(iq ,attrs ,@children))

(defun jabber-iq-from-xml-sexp (xml)
  "Given a sexp representation of some XML, create a Jabber iq object."

  (xmpp-log "Trying to translate IQ %S" xml)

  (let ((retval (jabber-iq-new))
        (attrs (xml-node-attributes xml))
        (children (xml-node-children xml)))

    (jabber-iq-type-set retval 'get)

    (mapc
     (lambda (attr)
       (let ((attr-name (car attr))
             (attr-value (cdr attr)))
         (cond
          ((eq attr-name 'type)
           (jabber-iq-type-set retval (intern attr-value)))
          ((eq attr-name 'id)
           (jabber-iq-id-set retval attr-value))
          ((eq attr-name 'from)
           (jabber-iq-from-set retval
                               (jabber-jid-from-string attr-value)))
          ((eq attr-name 'to)
           (jabber-iq-to-set retval
                             (jabber-jid-from-string attr-value))))))
     attrs)

    (mapc
     (lambda (child)
       (when (listp child)
         (let ((node-name (xml-node-name child))
               (node-attrs (xml-node-attributes child))
               (node-children (xml-node-children child)))
           (cond
            ((eq node-name 'key)
             (jabber-iq-key-set retval
                                (apply 'concat node-children)))
            ((eq node-name 'error)
             (jabber-iq-error-set
              retval
              (jabber-error-from-xml-sexp child)))
            ;; We're ignoring <query/> thingies for the time being.
            ((eq node-name 'query)
             (jabber-iq-query-set retval child))
            ))))
     children)

    retval))

(defun jabber-error-to-xml-sexp (errobj)
  "Return an XML sexp representation of ERROBJ."
  (let ((attrs '())
        (children '()))
    (jabber-error-bind errobj
      (when (stringp code)
        (add-to-list 'attrs (cons 'code code)))
      (when (stringp text)
        (add-to-list 'children text)))
    `(error ,attrs ,@children)))

(defun jabber-error-from-xml-sexp (xml)
  "Return a Jabber error object representation of xml-sexp XML."
  (let ((retval (jabber-error-new))
        (attrs (xml-node-attributes xml))
        (children (xml-node-children xml)))
    (xmpp-log "Making error from %S" xml)
    (let ((code (cdr (assq 'code attrs))))
      (when code
        (jabber-error-code-set retval code)))
    (jabber-error-text-set retval (apply 'concat children))
    retval))

(provide 'jabber-objs)
;;; jabber-objs.el ends here
