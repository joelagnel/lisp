;;; bash.el --- bash.org interface

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Ulrik Jensen <ulrik@qcom.dk>
;; Keywords: HTTP, bash, searching
;; Time-stamp: <2003-04-14 17:08:55 Administrator>
;; Version: 0.1 alpha :)

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

;; An interface for reading bash.org with Emacs
;; Requires http-get 1.0.8:
;;   <http://www.emacswiki.org/cgi-bin/wiki.pl?HttpGet>

;;; Code:

(require 'http-get)

;; URL's, for use later in the script
(defvar bash-get-quote-url "http://www.bash.org/?%id%"
  "The URL to fetch to get a specific quote.

  %id% will be replaced with the specific id")

(defvar bash-search-quotes-url
  "http://bash.org/?search=%criteria%&sort=%sort%&show=%number%"
  "The URL to search bash.org for quotes.

 %number% will be replaced with the max number of results
 %sort% will be replaced with 1 or 2, with 1=id, and 2=rating
%criteria% will be replaced with the words to search for")

(defvar bash-get-latest-url "http://www.bash.org/?latest"
  "The URL to fetch to get the latest quotes from bash.org.")

(defvar bash-get-top-rated-url "http://www.bash.org/?top"
  "The URL to fetch to get the top 50 quotes from bash.org.")

(defvar bash-get-next-top-rated-url "http://www.bash.org/?top2"
  "The URL to fetch to get the top 50-100 quotes from bash.org.")

(defvar bash-get-random-url "http://www.bash.org/?random"
  "The URL to fetch to get 30 random quotes from bash.org.")

(defvar bash-get-random-above-zero-url
  "http://www.bash.org/?random2"
  "The URL to fetch to get 30 random quotes, with rating > 0 from bash.org")

;; Variable for holding the title of the requested page
(defvar bash-tmp-results-title "Search"
  "A temporary variable that stores a title to insert in all *bash*-buffers")

;; Buffer names
(defvar bash-buffer "*bash*"
  "Name of the buffer used to read bash.org quotes in")

(defvar bash-temp-buffer "*bash-tmp*"
  "Name of the temporary buffer used to fetch and parse bash.org results")

;; Regexps for parsing the html-output of bash.org
(defvar bash-mysql-down-regexp "<p>.*Sorry.*MySQL.*down"
  "A regular expression used to check if bash.org's mysql deamon
is down, as often is the case.")

(defvar bash-quote-regexp
  "<p class=\"quote\">\\(.*?\\)</p><p class=\"qt\">\\(.*?\\)</p>"
  "A regular expression used to parse the html-source of bash.org outputs.

The first group is data about the quote, links, id, and votes.
The second group is the quote itself")

(defvar bash-quote-data-regexp
  (concat "<a href=\"\\?\\([0-9]*\\)\" title=\".*?"
	  "<a href=\"\\./\\?\\(.*?\\)\".*?</a>"
	  "(\\(-?[0-9]*\\))<a href=\"\\./\\?\\(.*?\\)\".*?"
	  "<a href=\"\\./\\?\\(.*?\\)\"")
  "A regular expression used to parse the data-group of `bash-quote-regexp'

The groups of this regular expressions should match the following:
1. The id of the quote on bash.org
2. The URI to vote positively
3. The number of votes the quote has received
4. The URI to vote negatively
6. The URI to flag for deletion")

;; URL-generating functions
(defun bash-get-quote-url (id)
  "Return the URL for a specific quote"
  (replace-regexp-in-string "%id%" id bash-get-quote-url))

(defun bash-make-search-url (criteria sort number)
  "Returns a URL to search bash.org for criteria"
  (let* ((url (replace-regexp-in-string "%criteria%" (http-url-encode criteria 'iso-latin-1) bash-search-quotes-url))
	 (url (replace-regexp-in-string "%sort%" sort url))
	 (url (replace-regexp-in-string "%number%" (number-to-string number) url)))
    url))

;; At some point, this should add faces as well
(defun bash-parse-single-quote (quote data)
  "Parses the HTML of a single quote, and returns the appropriate output"
  (unless (string-match bash-quote-data-regexp data)
    (error "Data-field didn't match regexp!"))
  (let* ((quoteid  (match-string 1 data))
	 (uplink   (match-string 2 data))
	 (votes    (match-string 3 data))
	 (downlink (match-string 4 data))
	 (flag     (match-string 5 data))
	 ;; I really should put these into an alist or use
	 ;; a html-rendering function for it
	 (curquote (replace-regexp-in-string "<[/]?p[^>]*>" "" quote))
	 (curquote (replace-regexp-in-string "&lt;" "<" curquote))
	 (curquote (replace-regexp-in-string "&gt;" ">" curquote))
	 (curquote (replace-regexp-in-string "<br />" "\n" curquote))
	 (curquote (replace-regexp-in-string "&quot;" "\"" curquote))
	 (curquote (replace-regexp-in-string "&nbsp;" " " curquote))
	 (curquote (replace-regexp-in-string "&amp;" "&" curquote)))
    ;; Below is the visual output
    (insert "Quote ")
    (widget-create 'push-button
		   :notify `(lambda (&rest ignore)
			      (bash-specific-quote ,quoteid))
 		   (concat "#" quoteid))
    (insert "   ")
    (widget-create 'push-button
		   :notify `(lambda (&rest ignore)
			      (bash-process-request (concat "http://www.bash.org/?" ,uplink) "add a positive vote to " ,quoteid))
 		   "+")
    (insert " (" votes ") ")
    (widget-create 'push-button
		   :notify `(lambda (&rest ignore)
			      (bash-process-request (concat "http://www.bash.org/?" ,downlink) "add a negative vote to " ,quoteid))
		   "-")
    (insert "   ")
    (widget-create 'push-button
		   :notify `(lambda (&rest ignore)
			      (bash-process-request (concat "http://www.bash.org/?" ,flag) "flag " ,quoteid))
 		   "X")
    (insert "   ")
    (widget-create 'push-button
		   :notify `(lambda (&rest ignore)
			      (bash-save-quote ,curquote ,quoteid))
 		   "Save quote")
    (insert "\n"
	    "--------------------------------------------------------------------------------"
	    "\n" curquote "\n"
	    "--------------------------------------------------------------------------------"
	    "\n\n")))

(defun bash-parse (buffer)
  "Parses the results from bash.org, in the bash-temp buffer, and adds them to BUFFER"
  (set-buffer (get-buffer-create bash-temp-buffer))
  (let* ((buftext (buffer-substring (point-min) (point-max)))
	 (buftext (replace-regexp-in-string "\n" "" buftext))
	 (buftext (replace-regexp-in-string "</pt>?" "</pt>\n" buftext))
	 (buftext (replace-regexp-in-string "" "" buftext)))
    (set-buffer buffer)
    (insert "\n\n")
    (cond
     ;; If there are actually quotes in the output
     ((string-match bash-quote-regexp buftext)
      (while (string-match bash-quote-regexp buftext)
	(let ((curdata  (match-string 1 buftext))
	      (curquote (match-string 2 buftext))
	      (quotestart (string-match bash-quote-regexp buftext)))
	  (when curquote		; just a precaution.. shouldn't be necessary
	    (bash-parse-single-quote curquote curdata)
	    ;; delete the quote from the string, actually. this is a few chars
	    ;; short every time.. hope it doesn't matter though
	    (setq buftext (substring buftext (+ (length curquote) (length curdata) quotestart) nil))
	    (setq quote-count (+ quote-count 1))))))
     ;; If the output tells us that the mysql-deamon is down
     ((string-match bash-mysql-down-regexp buftext)
      (insert "Bash.org's MySQL-deamon seems to be down at the moment."))
     ;; If none of the above, panic
     (t
      (insert "No results!")))))

(defun bash-insert-menubar ()
  "Inserts a widget-based menubar for navigating bash.org"
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (bash-random-30-quotes))
		 "Random")
  (insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (bash-random-above-zero-quotes))
		 "> 0")
  (insert "   Top ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (bash-top-50-quotes))
		 "50")
  (insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (bash-top-50-100-quotes))
		 "-100")
  (insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (bash-latest-quotes))
		 "Latest")
  (insert "     ")
  (widget-create 'push-button :notify (lambda (&rest ignore) (bury-buffer)) "Bury buffer")
  (insert "\n"
	  "--------------------------------------------------------------------------------"))

(defun bash-sentinel (process string)
  "Sentinel for processing bash-results"
  (kill-buffer (get-buffer-create bash-buffer))
  (let ((buffer (get-buffer-create bash-buffer)))
    (set-buffer buffer)
    (erase-buffer)
    (goto-char 0)
    (bash-insert-menubar)
    (insert "\nBash Results - " bash-tmp-results-title "\n")
    (let ((quote-count 0))
      (bash-parse buffer)
      (insert "\n\n   " (number-to-string quote-count) " quotes showed."))    
    (pop-to-buffer buffer)
    ;; Setup widget-minor-mode
    ;; should always be called before setting a new major mode
    ;; apparently also needs to be called before widget-minor-mode
    (kill-all-local-variables)
    (widget-minor-mode 1)
    (widget-setup)
    ;; Make the buffer read-only, no need to edit it
    (setq buffer-read-only t)
    ;; Bind some keys
    (local-set-key "q" '(lambda() (interactive) (bash-cleanup-buffers)))
    ;; (local-set-key "n" 'bash-next-quote)
    ;; scroll to the top
    (goto-char 0)
    (kill-buffer (get-buffer-create bash-temp-buffer))))

;; For these to work, I might have to use http-1.1
(defun bash-request-sentinel (process string)
  "Sentinel for processing bash-results"
  (let ((buffer (get-buffer-create bash-temp-buffer)))
    (save-excursion
      (set-buffer buffer)
      ;; Check the result of the request, and message it      
      ;;       (kill-buffer (get-buffer-create bash-temp-buffer))
      )))

(defun bash-cleanup-buffers ()
  "Removes all bash-related buffers"
  (kill-buffer (get-buffer-create bash-buffer))
  (kill-buffer (get-buffer-create bash-temp-buffer)))

(defun bash-process-url (url)
  (http-get url nil 'bash-sentinel 1.0 bash-temp-buffer)
  (message "Waiting for results from bash.org"))

(defun bash-process-request (uri action id)
  (http-get (concat "http://www.bash.org/?" uri) nil 'bash-request-sentinel 1.0 bash-temp-buffer)
  (message "Attempting to %s quote #%s with bash.org" action id))

;; Entry points
;; --------------------------------------------------------------------------
(defun bash-specific-quote (id)
  "Downloads a specific quote from bash.org"
  (interactive "sEnter quote-id: ")
  (bash-process-url (bash-get-quote-url id)))
 
(defun bash-latest-quotes ()
  "Downloads a specific quote from bash.org"
  (interactive)
  (setq bash-tmp-results-title "Latest quotes")
  (bash-process-url bash-get-latest-url))

(defun bash-top-50-quotes ()
  "Downloads a specific quote from bash.org"
  (interactive)
  (setq bash-tmp-results-title "Top 50 quotes")
  (bash-process-url bash-get-top-rated-url))

(defun bash-top-50-100-quotes ()
  "Downloads a specific quote from bash.org"
  (interactive)
  (setq bash-tmp-results-title "Top 50-100 quotes")
  (bash-process-url bash-get-next-top-rated-url))

(defun bash-random-30-quotes ()
  "Downloads a specific quote from bash.org"
  (interactive)
  (setq bash-tmp-results-title "Random quotes")
  (bash-process-url bash-get-random-url))

(defun bash-random-above-zero-quotes ()
  "Fetches random quotes from bash.org, all with ratings above zero"
  (interactive)
  (setq tmp-bash-results-title "Random quotes, rating > 0")
  (bash-process-url bash-get-random-above-zero-url))

;; Saving a quote in a fortunedb file
(defun bash-save-quote (quote id)
  "Saves a quote to a fortune-format file"
  (let ((filename (read-file-name "Append to fortune-file: " )))
    (with-temp-buffer
      (find-file filename)
      (goto-char (point-max))
      (insert "\n%%\nfrom bash.org, quote #" id "\n" quote)
      (save-buffer)
      (kill-buffer (current-buffer)))))

;; Searching bash.org
(defun bash-search-quote (criteria sort number)
  "Searches bash.org for quote"
  (interactive "sSearch for: \ncSort by number ('n') or rating ('r'): \nnNumber of results to return (25,50,75 or 100): ")
  (unless (or (= sort ?n) (= sort ?r))
    (setq sort ?r))			; sort by rating pr. default
  (if (= sort ?n)
      (setq sort "1")
    (setq sort "2"))
  (setq bash-tmp-results-title (concat "Searched for \"" criteria "\""))
  (bash-process-url (bash-make-search-url criteria sort number)))

(provide 'bash-quotes)
;;; bash-quotes.el ends here