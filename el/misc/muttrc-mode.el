;;; muttrc-mode.el --- Generic mode for .muttrc files.

;; Copyright (C) 2005  Kumar Appaiah

;; Author: Kumar Appaiah <akumar_NOSPAM@ee.iitm.ac.in>
;; Keywords: 

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

;; This is a mode I wrote for editing my muttrc files. It is nothing
;; more than smart and colourful text highlighting.
;; However, since there are a few problems here and there waiting to
;; be fixed, I'd love to hear from you.
;; 
;; Initially, I tried using generic-mode, and builiding on that.
;; However, I was facing problems in building the regexps
;; for highlight. Moreover, I got the feeling that muttrc's are
;; complex enough to warrant their own major mode.
;;
;; To load this mode into Emacs automatically, add the following line
;; to your .emacs (change path!):
;;  (autoload 'muttrc-mode "/path/muttrc-mode.el" "muttrc-mode" t 'nil)
;; 
;; To spawn muttrc-mode automatically while editing certain file types,
;; you could add the regexp of those filnames to auto-mode-alist:
;;  (add-to-list 'auto-mode-alist '("\\.muttrc\\'"   . muttrc-mode))
;;
;; Developed on: GNU Emacs "21.3.1"
;;
;; PROBLEMS:
;; * This is the first version, and some keywords may get highlighted
;;   incorrectly. For example, `source' exists as a variable as well
;;   as a function. So, function always seems to take precendence.
;; * I have not tried to handle "line continuation" using `\'. I don't
;;   think you'll mind that too much; if you do, please write the change
;;   and inform me. I'd love to hear about it.
;; * Complete the keyword lists. A few things may be missing.
;; * Maybe more...
;;
;; TODO: First, get the code inspected by an elisp guru, then add any
;; features which people may want. Some of these are
;;   * an abbrev table
;;   * a keymap
;;   * ...
;;
;; Kumar Appaiah akumar_NOSPAM@ee.iitm.ac.in
;; Date: 27th June, 2005


;;; Code:

(defcustom muttrc-mode-hook nil
  "Normal hook to be run when entering muttrc-mode."
  :type 'hook
  :group 'data)

(defvar muttrc-mode-syntax-table nil
  "Syntax table used while editing muttrc files.")

(defvar muttrc-mode-abbrev-table nil
  "Abbrev table used while editing muttrc files.")
(define-abbrev-table 'muttrc-mode-abbrev-table ())

(defvar muttrc-mode-map nil
  "Syntax map used while editing muttrc files.")

(if muttrc-mode-map
    ()
  (setq muttrc-mode-map (make-sparse-keymap)))

(if muttrc-mode-syntax-table
    ()
  (setq muttrc-mode-syntax-table (make-syntax-table))
;; This syntax table seems to be all right.
  (modify-syntax-entry ?\\ "\\  " muttrc-mode-syntax-table)
  (modify-syntax-entry ?\n ">  " muttrc-mode-syntax-table)
  (modify-syntax-entry ?#  "<  " muttrc-mode-syntax-table)
  (modify-syntax-entry ?\f ">  " muttrc-mode-syntax-table)
  (modify-syntax-entry ?- "_" muttrc-mode-syntax-table)
  (modify-syntax-entry ?_ "_" muttrc-mode-syntax-table)
  (modify-syntax-entry ?. "_" muttrc-mode-syntax-table)
  (modify-syntax-entry ?@ "_" muttrc-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" muttrc-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" muttrc-mode-syntax-table)
  (modify-syntax-entry ?` "\"" muttrc-mode-syntax-table)
  (modify-syntax-entry ?= ".  " muttrc-mode-syntax-table)
  (modify-syntax-entry ?+ ".  " muttrc-mode-syntax-table))

(defconst muttrc-mode-font-lock-keywords
;; This part was tough. It took me a very long time
;; and many monster regexps before I got what I wanted.
  (eval-when-compile
    (list
     '("alias[ \t]+\\([^ \t]+\\)"
       ;; Colour whatever comes just after "alias"
       (1 font-lock-keyword-face))
     '("\\(?:bind\\|macro\\|[a-z]+-hook\\)[ \t]+\\([^ \t]*\\)"
       ;; Colour whatever comes after "bind", "macro" and "-hook"
       (1 font-lock-constant-face))
     '("color[ \t]+\\([a-z]+\\)[ \t]+\\([a-z]*\\)[ \n\t]+\\([a-z]+\\)\\([ \t]*[^ \t\n]*\\)"
       ;; Make color commands look smart.
       ;; TODO: The trouble with the above regexp is that it wants
       ;; the user to complete the fourth keyword before
       ;; highlighting anything on that line, i.e. consider
       ;;   color underline brightgreen default
       ;; Highlight of "underline" "brightgreen" will begin only
       ;; when you start typing in "default".
       (1 font-lock-builtin-face)
       (2 font-lock-variable-name-face)
       (3 font-lock-variable-name-face)
       (4 font-lock-constant-face))
     '("mono[ \t]+\\([a-z]+\\)[ \t]+\\([a-z]*\\)\\([ \t]*[^ \t\n]*\\)"
       ;; Color mono attributes,
       ;; TODO: Similar to above TODO.
       (1 font-lock-builtin-face)
       (2 font-lock-variable-name-face)
       (3 font-lock-constant-face))

     ;; Some keywords fit to be branded as functions.
     (cons (regexp-opt '("auto_view" "my_hdr" "unmy_hdr" "set" "unset"
			 "ignore" "unignore"
			 "alias" "unalias" "bind" "charset-hook" "iconv-hook"
			 "folder-hook" "macro" "color" "uncolor" "mono" "unmono"
			 "lists" "unlists" "subscribe" "unsubscribe" "mbox-hook"
			 "mailboxes" "hdr_order" "unhdr_order" "save-hook"
			 "fcc-hook" "fcc-save-hook" "send-hook" "message-hook"
			 "pgp-hook" "push" "exec" "score" "unscore" "toggle" "reset"
			 "source" "unhook") 'words) 'font-lock-function-name-face)

     ;; Variables. This is a monster regexp. It took me ages to get this
     ;; working, especially since I had to get the \\< and \\> right.
     ;; These word separators are introduced neatly by regexp-opt if
     ;; one gives "'words" as the second argument. However, I am
     ;; performing a "multiplication" of two regexps, one for getting the
     ;; possible prefixes "inv" and "no" and the other containing the
     ;; variables themselves.
     (cons (concat "\\<\\(?:no\\|inv\\)?\\(?:"
		   (regexp-opt '("abort_nosubject" "abort_unmodified"
"alias" "alias_file" "alias_format" "allow_8bit" "allow_ansi" "alternates"
"arrow_cursor" "ascii_chars" "askbcc" "askcc" "attach" "attach_format"
"attach_sep" "attach_split" "attribution" "autoedit" "auto_tag" "beep"
"beep_new" "bounce_delivered" "browser" "charset" "check_new" "collapse_unread"
"uncollapse_jump" "compose" "compose_format" "confirmappend" "confirmcreate"
"connect_timeout" "copy" "date_format" "default_hook" "delete"
"delete_untag" "digest_collapse" "display_filter" "dotlock_program"
"dsn_notify" "dsn_return" "duplicate_threads" "edit_headers" "editor"
"encode_from" "envelope_from" "escape" "fast_reply" "fcc_attach"
"fcc_clear" "folder" "folder_format" "followup_to" "force_name"
"forward_decode" "forward_format" "forward_quote" "from" "gecos_mask"
"generic" "hdrs" "hdr_format" "header" "help" "hidden_host" "hide_limited"
"hide_missing" "hide_top_limited" "hide_top_missing" "history"
"honor_followup_to" "hostname" "ignore_list_reply_to"
"imap_authenticators" "imap_delim_chars" "imap_force_ssl"
"imap_home_namespace" "imap_keepalive" "imap_list_subscribed"
"imap_pass" "imap_passive" "imap_peek" "imap_servernoise" "imap_user"
"implicit_autoview" "include" "indent_string" "index" "index_format" "ispell"
"keep_flagged" "locale" "mail_check" "mailcap_path" "mailcap_sanitize"
"maildir_trash" "mark_old" "markers" "mask" "mbox" "mbox_type" "metoo"
"menu_scroll" "meta_key" "mh_purge" "mh_seq_flagged" "mh_seq_replied"
"mh_seq_unseen" "mime_forward" "mime_forward_decode"
"mime_forward_rest" "mix_entry_format" "mixmaster" "move"
"message_format" "pager" "pager_context" "pager_format"
"pager_index_lines" "pager_stop" "pgp" "pgp_autosign" "pgp_autoencrypt"
"pgp_ignore_subkeys" "pgp_entry_format" "pgp_good_sign" "pgp_long_ids"
"pgp_replyencrypt" "pgp_replysign" "pgp_replysignencrypted"
"pgp_retainable_sigs" "pgp_show_unusable" "pgp_sign_as"
"pgp_strict_enc" "pgp_timeout" "pgp_verify_sig" "pgp_sort_keys"
"pgp_create_traditional" "pgp_decode_command" "pgp_getkeys_command"
"pgp_verify_command" "pgp_decrypt_command" "pgp_clearsign_command"
"pgp_sign_command" "pgp_encrypt_sign_command"
"pgp_encrypt_only_command" "pgp_import_command" "pgp_export_command"
"pgp_verify_key_command" "pgp_list_secring_command"
"pgp_list_pubring_command" "forward_decrypt" "ssl_starttls"
"certificate_file" "ssl_usesystemcerts" "entropy_file" "ssl_use_sslv2"
"ssl_use_sslv3" "ssl_use_tlsv1" "pipe_split" "pipe_decode" "pipe_sep"
"pop_authenticators" "pop_auth_try_all" "pop_checkinterval"
"pop_delete" "pop_host" "pop_last" "pop_reconnect" "pop_user"
"pop_pass" "post_indent_string" "postpone" "postponed" "preconnect"
"print" "print_command" "print_decode" "print_split" "prompt_after"
"query_command" "quit" "quote_regexp" "read_inc" "read_only"
"realname" "recall" "record" "reply_regexp" "reply_self" "reply_to"
"resolve" "reverse_alias" "reverse_name" "reverse_realname"
"rfc2047_parameters" "save_address" "save_empty" "save_name" "score"
"score_threshold_delete" "score_threshold_flag" "score_threshold_read"
"send_charset" "sendmail" "sendmail_wait" "shell" "sig_dashes"
"sig_on_top" "signature" "simple_search" "smart_wrap" "smileys"
"sleep_time" "sort" "sort_alias" "sort_aux" "sort_browser" "sort_re"
"spoolfile" "status_chars" "status_format" "status_on_top"
"strict_threads" "suspend" "text_flowed" "thread_received"
"thorough_search" "tilde" "timeout" "tmpdir" "to_chars" "tunnel"
"use_8bitmime" "use_domain" "use_from" "use_mailcap" "user_agent"
"visual" "wait_key" "weed" "wrap_search" "wrapmargin" "write_inc"
"write_bcc")) "\\)\\>")
	   'font-lock-variable-name-face)

     ;; Things which can be calles builtins?
     (cons (regexp-opt '("yes" "no" "ask-yes" "ask-no" "on" "off"
"date" "date-sent" "date-received" "from" "mailbox-order" "score"
"size" "subject" "threads" "to" "reverse-date" "reverse-date-sent"
"reverse-date-received" "reverse-from" "reverse-mailbox-order"
"reverse-score" "reverse-size" "reverse-subject" "reverse-threads"
"reverse-to" "last-date" "last-date-sent" "last-date-received"
"last-from" "last-mailbox-order" "last-score" "last-size"
"last-subject" "last-threads" "last-to" "first-entry" "last-entry"
"top-page" "bottom-page" "trust") 'words)
	   'font-lock-builtin-face)
     ))
"Keywords highlighted in muttrc-mode.")

;;;###autoload
(defun muttrc-mode ()
  "Major mode for editing the Mutt mailer's configuration files.

Uses `muttrc-mode-syntax-table' and `muttrc-mode-font-lock-keywords'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map muttrc-mode-map)
  (setq local-abbrev-table muttrc-mode-abbrev-table)
  (set-syntax-table muttrc-mode-syntax-table)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (setq mode-name "muttrc")
  (setq major-mode 'muttrc-mode)
  (setq font-lock-defaults '(muttrc-mode-font-lock-keywords nil t ((?_ . "w")
								   (?- . "w")
								   (?. . "w")
								   (?@ . "w"))))
  (run-hooks 'muttrc-mode-hook))

(provide 'muttrc-mode)

;;; muttrc-mode.el ends here
