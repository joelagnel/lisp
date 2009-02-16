;; xml-stream.el --- Simple XML stream support for Emacs

;; Copyright (C) 2002 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: comm, tools, processes

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See <URL:http://etherx.jabber.org/streams/> for more about simple
;; XML streams.

;; Generally speaking, elements directly under your <stream:stream/>
;; should be reasonably small.

;; Sample usage:

;; (require 'xml-stream)
;;
;; (defun my-xs-callback (thingy &rest args) ...)
;;
;; (setq my-xs-stream
;;       (xml-stream-open-network-stream "Hello" "*hello*"
;;                                       "floobin.cx" 4732
;;                                       '((xmlns . "hello"))
;;                                       'my-xs-callback
;;                                       arg1 arg2 ... argN)
;; ...
;; (xml-stream-send my-xs-stream '(hello nil "world"))
;; ...
;; (xml-stream-close my-xs-stream)

;;; Code:

;; Internal names are xs-FOO, external names are xml-stream-FOO.

(require 'xml)

(require 'xml-event)

;; Quick and dirty tree data structure for keeping track of the XML.

(defun xs-tree (&optional parent name attributes children)
  (let ((retval (vector parent name attributes children)))
    (when parent
      (xs-tree-add-child parent retval))
    retval))

(defsubst xs-tree-parent (node) (aref node 0))
(defsubst xs-tree-set-parent (node parent) (aset node 0 parent))
(defsubst xs-tree-name (node) (aref node 1))
(defsubst xs-tree-set-name (node name) (aset node 1 name))
(defsubst xs-tree-attributes (node) (aref node 2))
(defsubst xs-tree-set-attributes (node attrs) (aset node 2 attrs))
(defsubst xs-tree-children (node) (aref node 3))
(defsubst xs-tree-set-children (node children) (aset node 3 children))

(defun xs-tree-add-child (node child)
  (let ((children (xs-tree-children node)))
    (setq children (append children (list child)))
    (xs-tree-set-children node children)))

(defun xs-tree-add-attribute (node attr)
  (let ((attrs (xs-tree-attributes node)))
    (setq attrs (append attrs (list attr)))
    (xs-tree-set-attributes node attrs)))

(defun xs-tree-root (node)
  (let ((parent (xs-tree-parent node)))
    (if (null parent)
        node
      (xs-tree-root parent))))

(defun xs-tree-to-list (node)
  (if (vectorp node)
      `(,(xs-tree-name node) ,(xs-tree-attributes node)
        ,@(mapcar 'xs-tree-to-list (xs-tree-children node)))
    node))

;;; Formatting sexps into XML.



(defvar xs-callback nil
  "Callback to be used for incoming XML objects.")
(make-variable-buffer-local 'xs-callback)
(defvar xs-callback-args nil
  "Arguments to be passed to `xs-callback'.")
(make-variable-buffer-local 'xs-callback-args)

(defvar xs-current-toplevel-node nil
  "Current toplevel XML node coming gently down the stream.
Merrily merrily merrily merrily, life is but a dream.")
(make-variable-buffer-local 'xs-current-toplevel-node)

(defun xs-open-tag-callback (tag attrs process)
  ""
  (with-current-buffer (cond ((processp process)
                              (process-buffer process))
                             ((bufferp process)
                              process)
                             (t
                              (error "Not a process or buffer, %S"
                                     process)))
    (setq xs-current-toplevel-node (xs-tree xs-current-toplevel-node
                                            tag attrs))))

(defun xs-close-tag-callback (tag process)
  ""
  (with-current-buffer (cond ((processp process)
                              (process-buffer process))
                             ((bufferp process)
                              process)
                             ;; Bad default.
                             (t
                              (error "Not a process or buffer, %S"
                                     process)))
    (when (vectorp xs-current-toplevel-node)
      (let ((parent (xs-tree-parent xs-current-toplevel-node)))
        (when (and (vectorp parent)
                   (eq (xs-tree-name parent)
                       'stream:stream))
          ;; Let's forget about this child.
          (xs-tree-set-children parent '())
          (let ((list (xs-tree-to-list xs-current-toplevel-node)))
            (apply xs-callback list xs-callback-args)))))
    (setq xs-current-toplevel-node (xs-tree-parent xs-current-toplevel-node))))

(defun xs-empty-tag-callback (tag attrs process)
  "An empty tag might as well be treated as an open and then a close.
Yay code reuse."
  (xs-open-tag-callback tag attrs process)
  (xs-close-tag-callback tag process))

(defun xs-content-callback (content process)
  ""
  ;; We don't want to save spurious toplevel content. At least, I
  ;; don't think we want to.
  (with-current-buffer (cond ((processp process)
                              (process-buffer process))
                             ((bufferp process)
                              process)
                             ;; Bad default.
                             (t (error "%S is not a process or buffer"
                                       process)))
    (let ((parent (condition-case nil
                      (xs-tree-parent xs-current-toplevel-node)
                    (error nil))))
      (when parent ;; not directly under <stream:stream/>
        (xs-tree-add-child xs-current-toplevel-node content)))))

(defun xs-stream-closed-callback (process)
  ""
  )

;;;###autoload
(defun xml-stream-close (process)
  "Close the simple XML stream associated with PROCESS."
  (process-send-string process "</stream:stream>\n"))

;;;###autoload
(defun xml-stream-open-network-stream (name buffer host service attrs callback &rest args)
  "Open a (read-write) simple XML stream connection for a service to a host.
Returns the subprocess-object."

  (let ((process (open-network-stream name buffer host service)))

    (set-process-coding-system process 'utf-8 'utf-8)

    (xml-event-register-callback process 'open-tag
                                 'xs-open-tag-callback process)
    (xml-event-register-callback process 'close-tag
                                 'xs-close-tag-callback process)
    (xml-event-register-callback process 'empty-tag
                                 'xs-empty-tag-callback process)
    (xml-event-register-callback process 'content
                                 'xs-content-callback process)
    (xml-event-register-callback process 'stream-closed
                                 'xs-stream-closed-callback)

    (xml-event-install process)

    (with-current-buffer (process-buffer process)
      (setq xs-current-toplevel-node nil)
      (setq xs-callback callback
            xs-callback-args args))

    (process-send-string
     process
     (format (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
                     "<stream:stream\n"
                     "  to=\"%s\"\n"
                     " %s\n"
                     "  xmlns:stream=\"http://etherx.jabber.org/streams\">\n")
             host
             (mapconcat 'xe-xml-sexp-attr-to-xml attrs "\n ")))

    process))

;;;###autoload
(defun xml-stream-open-buffer-stream (buffer callback &rest args)
  "Open a (read-only) simple XML stream with BUFFER as the data source."
  (apply 'xml-event-register-callback buffer 'open-tag
         'xs-open-tag-callback args)
  (apply 'xml-event-register-callback buffer 'close-tag
         'xs-close-tag-callback args)
  (apply 'xml-event-register-callback buffer 'empty-tag
         'xs-empty-tag-callback args)
  (apply 'xml-event-register-callback buffer 'content
         'xs-content-callback args)

  (with-current-buffer buffer
    (goto-char (point-min))
    (setq xs-current-toplevel-node nil)
    (setq xs-callback callback
          xs-callback-args args))

  (xml-event-parse buffer))

;;;###autoload
(defun xml-stream-open-file-stream (file callback)
  "Open a (read-only) simple XML stream with FILE as the data source."
  (let ((buf (find-buffer-visiting file))
        (killp nil))
    (if buf
        (setq killp t)
      (setq buf (find-file file)))
    (xml-stream-open-buffer-stream buf callback)
    (when killp
      (kill-buffer buf))))

;;;###autoload
(defun xml-stream-send (process xml-sexp)
  "Send an XML representation of XML-SEXP down the simple XML stream PROCESS."
  (process-send-string process
                       (format "%s\n" (xe-xml-sexp-to-xml xml-sexp))))

(provide 'xml-stream)
;;; xml-stream.el ends here
