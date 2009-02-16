;;; xml-event.el --- An event-based XML parsing engine for Emacs

;; Copyright (C) 2002 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: comm, tools, processes

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

;; Typical usage:

;; (require 'xml-event)

;; (defun my-open-tag-callback (tag attrs my-arg-1 my-arg-2 ...)
;;   ...)

;; (defun my-close-tag-callback (tag  my-arg-1 my-arg-2 ...)
;;   ...)

;; (defun my-empty-tag-callback (tag attrs my-arg-1 my-arg-2 ...)
;;   (my-open-tag-callback tag attrs)
;;   (my-close-tag-callback tag))

;; (defun my-content-callback (content my-arg-1 my-arg-2 ...)
;;   ...)

;; my-thingy is my-process or my-buffer
;; (xml-event-register-callback my-thingy 'open-tag
;;                              'my-open-tag-callback
;;                              my-arg-1 my-arg-2 ...)
;; (xml-event-register-callback my-thingy 'close-tag
;;                              'my-close-tag-callback
;;                              my-arg-1 my-arg-2 ...)
;; (xml-event-register-callback my-thingy 'empty-tag
;;                              'my-empty-tag-callback
;;                              my-arg-1 my-arg-2 ...)
;; (xml-event-register-callback my-thingy 'content
;;                              'my-content-callback
;;                              my-arg-1 my-arg-2 ...)
;; (xml-event-register-callback my-thingy 'stream-closed
;;                              'my-stream-closed-callback
;;                              my-arg-1 my-arg-2 ...)

;; (xml-event-install my-process)
;; - or -
;; (xml-event-parse my-buffer)

;;; Code:

;; Internal names are xe-FOO, external names are xml-event-FOO.

(require 'xml)

(condition-case nil
    (require 'smyrno-hack)
  (error
   (defun xe-log (&rest args)
     nil)))

(defun xe-escape (string)
  "Escape STRING to be included in some XML."
  ;; Important to do this one first!
  (setq string (replace-regexp-in-string "&" "&amp;" string))
  (setq string (replace-regexp-in-string "'" "&apos;" string))
  (setq string (replace-regexp-in-string "\"" "&quot;" string))
  (setq string (replace-regexp-in-string "<" "&lt;" string))
  (setq string (replace-regexp-in-string ">" "&gt;" string))
  string)

(defun xe-unescape (string)
  "XML unescape STRING."
  ;; Important to do this one first!
  (setq string (replace-regexp-in-string "&amp;" "&" string))
  (setq string (replace-regexp-in-string "&apos;" "'" string))
  (setq string (replace-regexp-in-string "&quot;" "\"" string))
  (setq string (replace-regexp-in-string "&lt;" "<" string))
  (setq string (replace-regexp-in-string "&gt;" ">" string))
  string)

(defvar xe-state 'default
  "Current XML parsing state-machine state for this buffer.")
(make-variable-buffer-local 'xe-state)

(defvar xe-unprocessed-text ""
  "Content not yet sent on to one of the callback functions.")
(make-variable-buffer-local 'xe-unprocessed-text)

(defvar xe-current-tag-name nil
  "The name of the current tag being processed.")
(make-variable-buffer-local 'xe-current-tag-name)

(defvar xe-current-tag-attrs '()
  "The attributes of the current tag being processed.")
(make-variable-buffer-local 'xe-current-tag-attrs)

(defvar xe-open-tag-cb nil "*Callback to be used for open-tag events.")
(make-variable-buffer-local 'xe-open-tag-cb)
(defvar xe-open-tag-args nil "*Args to be passed to `xe-open-tag-cb'.")
(make-variable-buffer-local 'xe-open-tag-args)

(defvar xe-close-tag-cb nil "*Callback to be used for close-tag events.")
(make-variable-buffer-local 'xe-close-tag-cb)
(defvar xe-close-tag-args nil "*Args to be passed to `xe-close-tag-cb'.")
(make-variable-buffer-local 'xe-close-tag-args)

(defvar xe-empty-tag-cb nil "*Callback to be used for empty-tag events.")
(make-variable-buffer-local 'xe-empty-tag-cb)
(defvar xe-empty-tag-args nil "*Args to be passed to `xe-empty-tag-cb'.")
(make-variable-buffer-local 'xe-empty-tag-args)

(defvar xe-content-cb nil "*Callback to be used for content events.")
(make-variable-buffer-local 'xe-content-cb)
(defvar xe-content-args nil "*Args to be passed to `xe-content-cb'.")
(make-variable-buffer-local 'xe-content-args)

(defun xe-process-filter (process-or-buffer string)
  "Accept from PROCESS new XML content in STRING.

This is a fairly hairy function that could be made significanly
less hairy by the removal of the sanity checks and debugging code,
but I'm not comfortable with doing that yet. So sorry it's so big
and scary."
  (xe-log "--------------------------")
  (xe-log "Calling xe-process filter with %S %S" process-or-buffer string)
  (let ((buffer (cond ((processp process-or-buffer)
                       (process-buffer process-or-buffer))
                      ((bufferp process-or-buffer)
                       process-or-buffer)
                      (t
                       (error "Expecting a process or buffer, got %S"
                              process-or-buffer))))
        text
        state
        done
        open-cb open-args
        close-cb close-args
        empty-cb empty-args
        content-cb content-args
        current-tag-name
        current-tag-attrs
        unprocessed-text
        current-tag-name)

    ;; Get the various buffer-local values
    (with-current-buffer buffer
      (setq text (concat xe-unprocessed-text string)
            state xe-state
            open-cb xe-open-tag-cb
            close-cb xe-close-tag-cb
            empty-cb xe-empty-tag-cb
            content-cb xe-content-cb
            open-args xe-open-tag-args
            close-args xe-close-tag-args
            empty-args xe-empty-tag-args
            content-args xe-content-args
            current-tag-name xe-current-tag-name
            current-tag-attrs xe-current-tag-attrs))

    ;; Process the text
    (with-temp-buffer
      (delete-region (point-min) (point-max))
      (insert text)
      (let ((case-fold-search nil)
            (unprocessed-point (point-min)))
      (goto-char (point-min))

      (while (not done)

        (cond

         ;; We're somewhere in the text (non-tag) area of the XML.
          ((eq state 'default)
            (let ((next-tag-loc (search-forward "<" nil t 1)))
              (if next-tag-loc
                  (progn
                    (goto-char (- next-tag-loc 1))
                    (xe-log "u-p is %S, n-t-l is %S, p-m is %S"
                            unprocessed-point
                            next-tag-loc
                            (point-max))
                    (let ((content-string (buffer-substring-no-properties
                                           unprocessed-point
                                           (point))))
                      (when (not (string-equal content-string ""))
                        (xe-log "%s %S %S" content-cb
                                 content-string content-args)
                        (apply content-cb content-string content-args)))
                    (setq unprocessed-point (point))
                    (goto-char next-tag-loc)
                    (setq state 'tag-start))
                (let ((u-t (buffer-substring-no-properties
                            unprocessed-point
                            (point-max))))
                  (xe-log "Giving up; can't find a < in %s" u-t)
                  (setq unprocessed-text u-t)
                  (setq state 'default)
                  (goto-char (point-max))
                  (setq done t)))))

          ;; We hit a <, and so we now need to process the tag if we can.
          ((eq state 'tag-start)
           (cond

            ((looking-at "[?]xml")
             (let ((prolog-end-loc (search-forward "?>" nil t 1)))
               (if prolog-end-loc
                   (progn
                     (goto-char prolog-end-loc)
                     (setq unprocessed-point (point))
                     (setq state 'default))
                 (let ((u-t (buffer-substring-no-properties
                             unprocessed-point
                             (point-max))))
                   (xe-log "Giving up in tag-start; can't find a ?> in %s" u-t)
                   (setq unprocessed-text u-t)
                   (setq state 'default)
                   (goto-char (point-max))
                   (setq done t)))))

            ((looking-at "/\\([-A-Za-z0-9._:]+\\)>")
             (let ((close-tag (intern (match-string-no-properties 1)))
                   (target (match-end 0)))
               (xe-log "%s %S %S" close-cb close-tag close-args)
               (apply close-cb close-tag close-args)
               (xe-log "Left after close %S"
                       (buffer-substring-no-properties (point) (point-max)))
               (xe-log "u-p1 is %S" unprocessed-point)
               (goto-char target)
               (setq unprocessed-point (point))
               (xe-log "u-p2 is %S" unprocessed-point)
               (setq state 'default)))

            ((looking-at "!--")
             (let ((comment-end-loc (search-forward "-->" nil t 1)))
               (if comment-end-loc
                   (progn
                     (goto-char comment-end-loc)
                     (setq unprocessed-point (point))
                     (setq state 'default))
                 (let ((u-t (buffer-substring-no-properties
                             unprocessed-point
                             (point-max))))
                   (xe-log "Giving up in tag-start; can't find a --> in %s" u-t)
                   (setq unprocessed-text u-t)
                   (setq state 'default)
                   (goto-char (point-max))))))

            ((looking-at "\\([-A-Za-z0-9._:]+\\)[ \t\r\n>/]")
             (setq unprocessed-point (- (point) 1))
             (setq current-tag-name
                   (intern (match-string-no-properties 1)))
             (goto-char (match-end 1))
             (setq state 'attr-start))

            (t

             (let ((u-t (buffer-substring-no-properties
                         unprocessed-point
                         (point-max))))
               (xe-log "Giving up in tag-start; don't understand %S" u-t)
               (setq unprocessed-text u-t)
               (setq state 'default)
               (goto-char (point-max))
               (setq done t)))))

          ;; We're looking at a tag's attribute list.
          ((eq state 'attr-start)
           (cond

            ;; We've hit the end, and it's an empty tag.
            ((looking-at "/>")
             (xe-log "%s %S %S %S" empty-cb current-tag-name
                     current-tag-attrs empty-args)
             (apply empty-cb current-tag-name current-tag-attrs
                    empty-args)

             (setq current-tag-name nil)
             (setq current-tag-attrs '())
             (forward-char 2)
             (setq unprocessed-point (point))
             (setq state 'default))

            ;; We've hit the end, and it's a normal open tag.
            ((looking-at ">")
             (xe-log "%s %S %S %S" open-cb current-tag-name
                     current-tag-attrs open-args)
             (apply open-cb current-tag-name current-tag-attrs
                    open-args)

             (setq current-tag-name nil)
             (setq current-tag-attrs '())
             (forward-char 1)
             (setq unprocessed-point (point))
             (setq state 'default))

            ((looking-at "[ \t\r\n]+")
             (goto-char (match-end 0)))

            ((looking-at (concat "\\([-A-Za-z0-9._:]+\\)"
                                 "[ \t\r\n]*=[ \t\r\n]*"
                                 "\\(['][^']*[']\\|[\"][^\"]*[\"]\\)"))
             (add-to-list 'current-tag-attrs
                          (cons (intern (match-string 1))
                                (substring (match-string 2) 1 -1)))
             (goto-char (match-end 0)))

            ;; There's something screwed up about this attribute list.
            (t
             (let ((u-t (buffer-substring-no-properties
                         unprocessed-point
                         (point-max))))
               (xe-log "Giving up in attr-start; don't understand %S" u-t)
               (setq unprocessed-text u-t)
               (setq state 'default
                     current-tag-name nil
                     current-tag-attrs nil)
               (goto-char (point-max))
               (setq done t)))))

          ;; Somehow our `state' variable has become completely screwed up.
          (t
           (let ((u-t (buffer-substring-no-properties
                       unprocessed-point
                       (point-max))))
             (xe-log "Giving up in state %S; don't understand %S" state u-t)
             (setq unprocessed-text u-t)
             (goto-char (point-max))
             (setq done t))))

        ;; If we've reached the end of the buffer, we're done.
        (when (>= (point) (point-max))
          (setq done t)))))

    ;; Update the various buffer-local values
    (with-current-buffer buffer
      (setq xe-state state
            xe-current-tag-name current-tag-name
            xe-current-tag-attrs current-tag-attrs
            xe-unprocessed-text unprocessed-text)
      (xe-log "xe-state is %s" xe-state)
      (xe-log "xe-current-tag-name is %s" xe-current-tag-name)
      (xe-log "xe-current-tag-attrs is %S" xe-current-tag-attrs)
      (xe-log "xe-unprocessed-text is %S" xe-unprocessed-text)
      (xe-log "--------------------------"))))

(defun xe-process-sentinel (process description)
  "Internal process sentinel."
  (let ((status (process-status process)))
    (with-current-buffer (process-buffer process)
      (cond ((eq status 'run) nil)
            ((eq status 'stop) nil)
            ((eq status 'exit) nil)
            ((eq status 'signal) nil)
            ((eq status 'open) nil)
            ((eq status 'closed)
             (apply xe-closed-cb process xe-closed-args))
            ((null status) nil)
            (t nil)))))

;;;###autoload
(defun xml-event-register-callback (process-or-buffer type callback &rest args)
  "For PROCESS-OR-BUFFER's TYPE events, register CALLBACK.
CALLBACK should be a function.

If TYPE is open-tag or empty-tag, CALLBACK should take two (or more)
arguments, the tag (a string) and an alist of (attribute . value)
pairs.

If TYPE is close-tag, CALLBACK should take one (or more) arguments,
the tag.

If TYPE is content, CALLBACK should take one (or more) arguments, the
content.

If TYPE is stream-closed, CALLBACK should take one (or more)
arguments, the process.

In addition to the listed arguments, ARGS will also be passed to the
callback."
  (let ((buffer (cond ((processp process-or-buffer)
                       (process-buffer process-or-buffer))
                      ((bufferp process-or-buffer)
                       process-or-buffer)
                      (t
                       (error "%s is not a process or buffer"
                              process-or-buffer)))))
    (with-current-buffer buffer
      (cond ((eq type 'open-tag)
             (setq xe-open-tag-cb callback
                   xe-open-tag-args args))
            ((eq type 'close-tag)
             (setq xe-close-tag-cb callback
                   xe-close-tag-args args))
            ((eq type 'empty-tag)
             (setq xe-empty-tag-cb callback
                   xe-empty-tag-args args))
            ((eq type 'content)
             (setq xe-content-cb callback
                   xe-content-args args))
            ((eq type 'stream-closed)
             (setq xe-closed-cb callback
                   xe-closed-args args))
            (t
             (error "Unknown callback type %s" type))))))

;;;###autoload
(defun xml-event-install (process)
  "Install the event-based XML parser as PROCESS' filter."
  (set-process-sentinel process 'xe-process-sentinel)
  (set-process-filter process 'xe-process-filter))

;;;###autoload
(defun xml-event-parse (buffer)
  "Parse BUFFER using the event-based XML parser."
  (with-current-buffer buffer
    (xe-process-filter buffer (buffer-substring (point-min) (point-max)))))

(defun xe-xml-sexp-attr-to-xml (attr-cons)
  (let ((attr-name (car attr-cons))
        (attr-val (cdr attr-cons)))
    (unless (stringp attr-val)
      (setq attr-val (format "%s" attr-val)))
    (concat (format " %s=" attr-name)
            (if (string-match "[\"]" attr-val)
                (format "'%s'" attr-val)
              (format "\"%s\"" attr-val)))))

;;;###autoload
(defun xe-xml-sexp-to-xml (xml-sexp)
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
                       (mapconcat 'xe-xml-sexp-attr-to-xml
                                  attrs
                                  "")
                     "")
                   (if children
                       (concat ">"
                               (mapconcat 'xe-xml-sexp-to-xml
                                          children
                                          "")
                               (format "</%s>" tag))
                     "/>"))))

        (t (xe-xml-sexp-to-xml (format "%s" xml-sexp)))))

(provide 'xml-event)
;;; xml-event.el ends here
