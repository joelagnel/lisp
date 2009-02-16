;;; google.el --- Googling for stuff
;; $Id: google.el,v 1.9 2004/02/27 21:28:21 wence Exp $

;; This file is NOT part of Emacs.

;; Copyright (C) 2002, 2003 lawrence mitchell <wence@gmx.li>
;; Filename: google.el
;; Version: $Revision: 1.9 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-05-15
;; Keywords: convenience searching

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; This file provides functionality I'm sure most of us have
;; implemented, starting web searches from within Emacs.
;; It was inspired and modified from a few functions in Ted O'Connor's
;; .emacs <URL:http://oconnor.cx/emacs.html>

;;; History:
;;

;;; Code:


(defun google (string &optional type option no-browse)
  "Google for STRING.

TYPE is the type of search (a symbol), one of:
   search --- search www.google.com.
   groups --- search groups.google.com.
   direct --- search the Google directory.
   images --- search images.google.com.

OPTION is an option for groups.google.com searches only (a symbol), one of:
   selm --- search for a message-id.
   group --- go to a group.

If given a prefix arg NO-BROWSE will be non-nil, and the resultant url
will be returned, rather than browsed to."
  (interactive "sGoogle: ")
  (let* ((dir (eq type 'direct))
         (type (symbol-name (if (or (null type) (eq type 'direct))
                                'search
                                type)))
         (option (and option (symbol-name option)))
         (no-browse (or no-browse current-prefix-arg))
         (url (concat "http://www.google.com/"
                      type
                      "?"
                      (or option "q")
                      "="
                      (google-make-sendable-string string)
                      (and dir "&cat=gwd/Top"))))
    (unless no-browse
      (browse-url url))
    url))


(defconst google-not-encoded-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u
    ?v ?w ?x ?y ?z ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P
    ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?-
    ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "List of characters that do not need to be encoded in a valid URL.

See RFC 2396 for details.")

(defun google-make-sendable-string (string)
  "Make STRING sendable as part of a Google URL.

This converts each character in STRING to its hex representation
preceded by a \"%\".

e.g.
\(google-make-sendable-string \"foo \")
    => \"foo%20\"."
  (mapconcat #'(lambda (c)
                 (if (memq c google-not-encoded-chars)
                     (format "%c" c)
                     (format "%%%02X" c)))
             (string-to-list string) ""))

(defun google-groups (string)
  "Search for STRING on groups.google.com."
  (interactive "sGoogle Groups: ")
  (google string 'groups))


(defun google-groups-group ()
  "Prompt for a newsgroup to go to on groups.google.com.

Defaults to the newsgroup at point."
  (interactive)
  (let* ((group (thing-at-point 'url))
         (group (and group
                    (string-match "^http://" group)
                    (replace-match "" nil t group))))
    (google
     (read-string (if group
                      (format "Which newsgroup (default %s): " group)
                    "Which newsgroup: ")
                  nil nil group)
     'groups 'group)))


(defun google-groups-message-id (start end)
  "Google Groups for the message-id between START and END."
  (interactive "r")
  (google (buffer-substring-no-properties start end) 'groups 'selm))


(defun google-region (start end)
  "Google for text from START to END."
  (interactive "r")
  (google (buffer-substring-no-properties start end) 'search))


(defun google-sentence ()
  "Google for sentence at point."
  (interactive)
  (google (thing-at-point 'sentence) 'search))


(defun google-word ()
  "Google for word at point."
  (interactive)
  (google (thing-at-point 'word) 'search))


(provide 'google)

;;; google.el ends here
