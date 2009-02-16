;;; trampcache.el --- file information caching for tramp

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: Daniel Pittman <daniel@inanna.danann.net>
;; Keywords: comm, processes
;; Version: $Id: trampcache.el,v 2.0 2001/02/28 10:39:32 grossjoh Exp $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; An implementation of file information caching for remote files.

;; Each connection (host, user) has a unique cache.
;; Each file has a unique set of properties associated with it.

;;; Code:


;; The internal interface to the cache system.
(defun tramp-cache-set (file key value)
  "Within the current tramp cache, set the property KEY to VALUE,
for FILE.

This must be run with the current cache context set."
  ;; do we have an entry for the file?
  (let* ((info (or (assoc file tramp-cache-data)
		   (assoc file (setq tramp-cache-data
				     (cons (list file (cons nil nil)) tramp-cache-data)))))
	 (prop (or (assoc key (cdr info))
		   (assoc key (setcdr info
				      (cons (cons key nil) (cdr info)))))))
	 (setcdr prop value)))

(defun tramp-cache-get (file key)
  "Within the current tramp cache, return the property KEY of FILE."
  (cdr-safe (assoc key (cdr-safe (assoc file tramp-cache-data)))))






;; The public interface to the cache system.
(defun tramp-cache-setup (multi method user host)
  "Initialise the cache system for a new tramp connection."
  (with-current-buffer (tramp-get-buffer multi method user host)
    (set (make-local-variable 'tramp-cached-data) nil)))


;; REVISIT: These should only key on (user . host), not the method.
;;  --daniel@danann.net, 2000-06-01
(defun tramp-cache-get-file-property (file key multi method user host)
  "Get the property KEY of FILE from the cache context of the
user USER on the remote machine HOST."
  (with-current-buffer (tramp-get-buffer multi method user host)
    (tramp-cache-get file key)))


(defun tramp-cache-set-file-property (file key value multi method user host)
  "Set the property KEY of FILE to VALUE, in the cache context of the
user USER on the remote machine HOST."
  (with-current-buffer (tramp-get-buffer multi method user host)
    (tramp-cache-set file key value)))


(defun tramp-cache-flush-file (file multi method user host)
  "Remove all properties of FILE in the cache context of USER on HOST."
  (with-current-buffer (tramp-get-buffer multi method user host)
    ;; REVISIT: Implement me. --daniel@danann.net, 2000-06-01
    ))


(defun tramp-cache-flush (multi method user host)
  "Remove all information from the cache context of USER on HOST."
  (with-current-buffer (tramp-get-buffer multi method user host)
    ;; REVISIT: Implement me. --daniel@danann.net, 2000-06-01
    ))
  
  

(provide 'trampcache)

;;; trampcache.el ends here
