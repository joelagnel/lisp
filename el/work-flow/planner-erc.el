;;; planner-erc.el --- ERC support for Planner, an organizer for Emacs

;;; Commentary:
;;
;;;_* Commentary

;;;_ + Package description

;; Copyright (C) 2004 Sandra Jean Chua <sacha@free.net.ph>

;; Emacs Lisp Archive Entry
;; Filename: planner.el
;; Version: $Version$
;; Keywords: hypermedia erc chat
;; Author: Sacha Chua <sacha@free.net.ph>
;; Description: Create tasks and notes based on IRC
;; URL: http://sacha.free.net.ph/notebook/emacs/emacs-wiki/planner.el
;; ChangeLog: http://sacha.free.net.ph/notebook/emacs/emacs-wiki/ChangeLog
;; Compatibility: Emacs20, Emacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;;_ + Usage
;;
;; Place planner-erc.el in your load path and add this to your .emacs:
;;
;;    (require 'planner-erc)
;;
;; ERC URLs are of the form
;;
;; erc://server/nick/channel or
;; erc://server/nick or
;; erc://server/nick
;;
;; Annotations will be of the form
;; [[erc://server/nick/#channel][Chat with nick on server#channel]]
;; [[erc://server/nick][Chat with nick on server]]
;; [[erc://server][Chat on server]]

(require 'planner)
(require 'erc)

;;; Code:
(defun planner-erc-annotation-from-erc ()
  "Return an annotation for the current line.
This function can be added to `planner-annotation-functions'."
  (when (eq major-mode 'erc-mode)
    (if (erc-default-target)
        (if (erc-channel-p (erc-default-target))
            (if (and (get-text-property (point) 'erc-parsed)
                     (elt (get-text-property (point) 'erc-parsed) 1))
                (let ((nick
                       (car
                        (erc-parse-user
                         (elt (get-text-property (point) 'erc-parsed) 1)))))
                  (emacs-wiki-make-link
                   (concat "irc://"
                           erc-announced-server-name "/"
                           nick ",isnick")
                   (concat "Chat with " nick " on "
                           erc-announced-server-name (erc-default-target))))
              (emacs-wiki-make-link
               (concat "irc://"
                       erc-announced-server-name "/"
                       (erc-default-target))
               (concat "Chat on " erc-announced-server-name
                       (erc-default-target))))
          (emacs-wiki-make-link
           (concat "irc://" erc-announced-server-name "/"
                   (erc-default-target))
           (concat "Chat with " (erc-default-target) " on "
                   erc-announced-server-name)))
      (emacs-wiki-make-link
       (concat "irc://" erc-announced-server-name)
       (concat "Chat on " erc-announced-server-name)))))

(defun planner-erc-browse-url (url)
  "If this is an ERC URL, jump to it.
This just connects to the server--you have to join the channel or privmsg
people yourself."
  ;; If anyone can figure out how to get it to automatically open the
  ;; channel window, that would be ultra cool.
  ;; Also, we need a way to canonicalize going to a particular server.
  ;; But this will do for now.
  (when (or (string-match "^erc://\\([^/]+\\)" url)
            (string-match "^irc://\\([^/]+\\)" url))
    (erc-select (match-string 1 url))))

(defun planner-erc-resolve-url (id)
  "Replace ID with nothing. ERC entries should not be linked."
  nil)

(add-to-list 'planner-resolve-url-table
             '("erc:" . planner-erc-resolve-url))

(add-hook 'planner-annotation-functions 'planner-erc-annotation-from-erc)
(custom-add-option 'planner-annotation-functions
                   'planner-erc-annotation-from-erc)
(planner-option-customized 'planner-url-list
                           (append (list "erc://") planner-url-list))
(add-hook 'planner-browse-url-functions 'planner-erc-browse-url)
(planner-update-wiki-project)
(provide 'planner-erc)

;;;_* Local emacs vars.

;; Local variables:
;; allout-layout: (* 0 : )
;; End:

;;; planner-erc.el ends here
