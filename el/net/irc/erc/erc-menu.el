;; erc-menu.el -- Menu-bar definitions for the Emacs IRC Client

;; Copyright (C) 2001,2002,2004,2005 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm, processes, menu
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcMenu

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

;; Loading this file defines a menu for ERC.

;;; Code:

(require 'easymenu)

(defconst erc-menu-version "$Revision: 1.18.2.2 $"
  "ERC menu revision")

(defvar erc-menu-definition
  (list "IRC"
	["Connect to server..." erc-select t]
	["Disconnect from server..." erc-quit-server erc-connected]
	"-"
	["List channels..." erc-cmd-LIST
	 (and erc-connected (fboundp 'erc-cmd-LIST))]
	["Join channel..." erc-join-channel erc-connected]
	["Start a query..." erc-cmd-QUERY erc-connected]
	"-"
        ["List users in channel" erc-channel-names erc-channel-users]
        ["List channel operators" erc-cmd-OPS erc-channel-users]
	["Input action..." erc-input-action (erc-default-target)]
	["Set topic..." erc-set-topic
	 (and (and (erc-default-target) (not (erc-query-buffer-p)))
	      (or (not (member "t" channel-modes))
		  (erc-channel-user-op-p (erc-current-nick))))]
	(list "Channel modes"
	      ["Change mode..." erc-insert-mode-command
               (erc-channel-user-op-p (erc-current-nick))]
	      ["No external send" (erc-toggle-channel-mode "n")
               :active (erc-channel-user-op-p (erc-current-nick))
	       :style toggle :selected (member "n" channel-modes)]
	      ["Topic set by channel operator" (erc-toggle-channel-mode "t")
	       :style toggle :selected (member "t" channel-modes)
               :active (erc-channel-user-op-p (erc-current-nick))]
	      ["Invite only" (erc-toggle-channel-mode "i")
	       :style toggle :selected (member "i" channel-modes)
               :active (erc-channel-user-op-p (erc-current-nick))]
	      ["Private" (erc-toggle-channel-mode "p")
	       :style toggle :selected (member "p" channel-modes)
               :active (erc-channel-user-op-p (erc-current-nick))]
	      ["Secret" (erc-toggle-channel-mode "s")
	       :style toggle :selected (member "s" channel-modes)
               :active (erc-channel-user-op-p (erc-current-nick))]
	      ["Moderated" (erc-toggle-channel-mode "m")
	       :style toggle :selected (member "m" channel-modes)
               :active (erc-channel-user-op-p (erc-current-nick))]
	      ["Set a limit..." erc-set-channel-limit
               (erc-channel-user-op-p (erc-current-nick))]
	      ["Set a key..." erc-set-channel-key
               (erc-channel-user-op-p (erc-current-nick))])
        ["Leave this channel..." erc-part-from-channel erc-channel-users]
	"-"
	(list "Pals, fools and other keywords"
	      ["Add pal..." erc-add-pal]
	      ["Delete pal..." erc-delete-pal]
	      ["Add fool..." erc-add-fool]
	      ["Delete fool..." erc-delete-fool]
	      ["Add keyword..." erc-add-keyword]
	      ["Delete keyword..." erc-delete-keyword]
	      ["Add dangerous host..." erc-add-dangerous-host]
	      ["Delete dangerous host..." erc-delete-dangerous-host])
	"-"
	(list "IRC services"
	      ["Identify to NickServ..." erc-nickserv-identify
	       (and erc-connected (functionp 'erc-nickserv-identify))])
	"-"
	["Save buffer in log" erc-save-buffer-in-logs (fboundp 'erc-save-buffer-in-logs)]
	["Truncate buffer" erc-truncate-buffer (fboundp 'erc-truncate-buffer)]
	"-"
	["Customize ERC" (customize-group 'erc) t]
	["Enable/Disable ERC Modules" (customize-variable 'erc-modules) t]
	["Show ERC version" erc-version t])
  "ERC menu definition.")

;; `erc-mode-map' must be defined before doing this
(eval-after-load "erc"
  '(progn
     (easy-menu-define erc-menu erc-mode-map "ERC menu" erc-menu-definition)
     (easy-menu-add erc-menu erc-mode-map)

     ;; for some reason the menu isn't automatically added to the menu bar
     (when (featurep 'xemacs)
       (add-hook 'erc-mode-hook (lambda () (easy-menu-add erc-menu erc-mode-map))))))

(provide 'erc-menu)

;;; erc-menu.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; standard-indent: 4
;; End:
