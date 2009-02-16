;;; host-fns.el --- functions for querying host name information

;; Copyright (C) 1991, 92, 93, 94, 95, 96, 97, 98, 99, 2000 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: host-fns.el,v 1.3 2004/02/16 11:05:29 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

(require 'file-fns)
(require 'string-fns)

;;;###autoload
(defun host-name ()
  "Returns the name of the current host minus the domain."
  (let ((hostname (downcase (system-name))))
    (save-match-data
      (substring hostname (string-match "^[^.]+" hostname) (match-end 0)))))

;;;###autoload
(defun host-nickname ()
  "Return the short version of the system hostname.
Return a string which is the name of the current host up to any \".\"
or \"-\" character.  Thus if the hostname is \"apple-gunkies.ai.mit.edu\"
then the return string will just be \"apple\"."
  (let ((hostname (system-name)))
    (save-match-data
      (if (string-match "^[^.-]+" hostname)
          (substring hostname 0 (match-end 0))
        hostname))))

;;;###autoload
(defun dns-domain-name ()
  "Get the domain name of the current host and return it as a string.

First, try to get the domain name from the function `system-name', which
will succeed if `system-name' returns the fully-qualified host name.  If
that fails, several external programs are tried.  If all these attempts
fail to determine the domain name the string \"unknown\" is returned.

Note that if  domain-name  has to call the external program
`domainname' the return value could be incorrect.  `domainname' is actually
supposed to return the NIS domain rather than the domain of the host, but
many system administrators configure systems incorrectly."
  (let ((domain (system-name)))
    (save-match-data
      (cond ((string-match "\\." domain)
             (substring domain (1+ (match-beginning 0))))
            (t
             (setq domain nil)
             (let ((progdata '(("hostname"      . "\\.\\(.*\\)")
                               ("dnsdomainname" . "\\(.*[\\.]+.*\\)")
                               ("domainname"    . "\\(.*[\\.]+.*\\)")))
                   prog regexp output)
               (while (and progdata (null domain))
                 (setq prog     (car (car progdata)))
                 (setq regexp   (cdr (car progdata)))
                 (setq progdata (cdr progdata))
                 (cond ((file-in-pathlist-p prog exec-path)
                        (setq output (with-output-to-string
                                       (call-process prog nil
                                                     standard-output nil)))
                        (and (string-match regexp output)
                             (setq domain (matching-substring 1 output)))))))
             (or domain "unknown"))))))

;;;###autoload
(defun nis-domain-name ()
  "Get the NIS domain name of the current host and return it as a string."
  (substring (with-command-output-to-string "domainname") 0 -1))

;;;###autoload
(defun abbreviate-hostnick (hostname)
  "Return a shorter string for hostname.
Any hyphenated host names get converted to using the first char of each
hyphenated part, e.g. \"apple-gunkies\" => \"ag\"."
  (and (string-match "\\." hostname)
       (setq hostname (substring hostname 0 (match-beginning 0))))
  (let* ((hostnick hostname))
    (save-match-data
      (cond ((string-match "-" hostname)
             (let ((pos (if (string-match "^-+" hostname) (match-end 0) 0))
                   (len (length hostname))
                   (abbrev ""))
               (while (< pos len)
                 (setq abbrev
                       (concat abbrev (char-to-string (aref hostname pos))))
                 (if (string-match "-+" hostname pos)
                     (setq pos (match-end 0))
                   (setq pos len)))
               (setq hostnick abbrev)))))
    hostnick))

(provide 'host-fns)

;;; host-fns.el ends here.
