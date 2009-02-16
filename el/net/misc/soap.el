;;; soap.el --- Simple Object Access Protocol support for Emacs

;; Copyright (C) 2002  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: comm, tools, processes

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; see the file COPYING. If not,
;; write to the Free Software Foundation, Inc., 59 Temple Place -
;; Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; You should always be able to find the latest version here:

;;            <URL:http://oconnor.cx/elisp/soap.el>

;; This is the barest of beginnings of SOAP support for Emacs. It
;; really doesn't do much of anything; to see how to use it, see
;; google.el. Someone who cares about SOAP should probably make
;; this into an actual SOAP implementation.

;; This code requires a CVS version of the URL package, which you
;; may find here:

;;         <URL:http://savannah.gnu.org/projects/url/>

;; TODO: I'd like to change this to use http-{get,post}.el instead of
;;       URL, but it looks like http-post.el isn't full-featured enough.
;;       Perhaps I'll send them a patch at some point.

;;; Code:

(require 'url)
(require 'xml)

(defun soap-process-response (response-buffer)
  "Process the SOAP response in RESPONSE-BUFFER."
  (let ((retval nil))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (when (looking-at "^HTTP/1.* 200 OK$")
        (re-search-forward "^$" nil t 1)
        (setq retval (buffer-substring-no-properties (point) (point-max))))
      (kill-buffer response-buffer))
    (with-temp-buffer
      (insert "\n" retval "\n")
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
        (replace-match ""))
      (xml-parse-region (point-min) (point-max)))))

(defun soap-request (url data)
  "Send and process SOAP request to URL with DATA."
  (let* ((url-request-extra-headers
          `(("Content-type" . "text/xml; charset=\"utf-8\"")
            ("SOAPAction" . ,(format "%S" url))))
         (url-request-method "POST")
         (url-request-data
          (concat "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
                  data)))
    (soap-process-response (url-retrieve-synchronously url))))

(provide 'soap)
;;; soap.el ends here
