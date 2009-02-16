;;; NEWS in this version:
;;; fixed bug in expand/collapse code (subtrees of depth > 1 should
;;; remember their state)
;;; IPv6 addresses sort correctly now

;;; dig-browser.el --- a dired-style DNS zone browser

;; Copyright (C) 2002 Ian Zimmerman

;; Author:  Ian Zimmerman <itz@speakeasy.org>
;; Created: Sat Dec 14 2002
;; Keywords: network communication domain zone

;; This file is NOT part of GNU Emacs.  It is nevertheless distributed
;; under the same conditions:

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
;;
;; Originally I had the idea of wrapping dig (actually a clone whose output
;; is easier to parse) in a simple Gtk GUI program with just a clickable
;; tree control to represent the DNS info.  But, it seemed a waste, because
;; there's really nothing graphical to this.  And dired provided an excellent
;; example to follow.

;; $Id: dig-browser.el,v 1.13 2003/01/06 05:30:26 itz Exp $

;; $Log: dig-browser.el,v $
;; Revision 1.13  2003/01/06 05:30:26  itz
;; Add sorting IPv6 addresses.
;;
;; Revision 1.12  2003/01/02 03:52:05  itz
;; Fixed fatal bug in expand-collapse logic
;;
;; Revision 1.11  2003/01/01 07:40:49  itz
;; Don't use first as it seems additional work to compile right.
;;
;; Revision 1.10  2003/01/01 07:37:55  itz
;; Proper sorting implemented!  Thanks go to Thien-Thi Nguyen
;; <ttn@glug.org> for an initial idea and encouragement, but even more
;; thanks to Uri Guttman <uri@stemsystems.com> from whose prototypical
;; Sort::Records perl module I took the idea of temporarily prefixing the
;; data with their sort keys.
;;
;; Revision 1.9  2003/01/01 06:28:20  itz
;; Prefer the master server for a zone, if known.
;;
;; Revision 1.8  2002/12/20 21:42:05  itz
;; Tie minor loose ends: kill auxiliary buffer after I'm finished with
;; it, don't require other packages because the things I use in them are
;; autoloaded, and use my own variable for dig program
;;
;; Revision 1.7  2002/12/19 22:20:14  itz
;; Add reverse domain browsing (not very useful because rarely delegated :-\ )
;;
;; Revision 1.6  2002/12/19 19:28:02  itz
;; Highlight subdomain NS records.  Expand or browse them when clicked.
;;
;; Revision 1.5  2002/12/19 17:56:36  itz
;; Bulletproof interactive input code, acting on suggestion by
;; Francesco Potorti` <pot@gnu.org>
;;
;; Revision 1.4  2002/12/19 08:02:22  itz
;; Add menu keymap and imenu support
;;
;; Revision 1.3  2002/12/19 07:21:20  itz
;; Sorting works.
;;
;; Revision 1.2  2002/12/19 04:28:20  itz
;; Rewrite subdomain stuff to use text properties instead of markers, to prepare for sorting.
;;

;;; Code:

(defconst dig-browser-version "$Id: dig-browser.el,v 1.13 2003/01/06 05:30:26 itz Exp $")

;; customizations

(defgroup dig-browser nil
  "DNS browsing through dig."
  :prefix "dig-browser-"
  :group 'comm
  :version "21.2")

(defcustom dig-browser-program (cond ((boundp 'dig-program) dig-program) (t "dig"))
  "*Name of the external dig program."
  :group 'dig-browser
  :type 'string)

(defcustom dig-browser-local-server "localhost"
  "*DNS server to submit NS queries to."
  :group 'dig-browser
  :type 'string)

(defcustom dig-browser-port 53
  "*IP Port to connect to for DNS queries."
  :group 'dig-browser
  :type 'integer)

(defcustom dig-browser-srcaddr "0.0.0.0"
  "*IP source address to use for DNS queries."
  :group 'dig-browser
  :type 'string)

(defcustom dig-browser-retry 3
  "*Number of retries to use for DNS queries."
  :group 'dig-browser
  :type 'integer)

(defcustom dig-browser-timeout 5
  "*Timeout in seconds to use for DNS queries."
  :group 'dig-browser
  :type 'integer)

(defcustom dig-browser-extra-switches '("+tcp")
  "*Extra switches to pass to the dig program."
  :group 'dig-browser
  :type '(repeat string))

(defcustom dig-browser-subdomain-indent 2
  "*Number of spaces by which to indent expanded subdomains."
  :group 'dig-browser
  :type 'integer)

;; programmer variables

(defvar dig-browser-mode-hook nil
  "Hook for functions to run in newly created Dig Browser mode buffers.")

(defvar dig-browser-before-fetch-hook nil
  "Hook for functions to run in Dig Browser mode buffers before dig program runs.")

(defvar dig-browser-after-fetch-hook nil
  "Hook for functions to run in Dig Browser mode buffers after dig program runs.")

(defvar dig-browser-before-insert-hook nil
  "Hook for functions to run in Dig Browser mode buffers before RRs are inserted.")

(defvar dig-browser-after-insert-hook nil
  "Hook for functions to run in Dig Browser mode buffers after RRs are inserted.")

(defvar dig-browser-bold-face 'bold
  "Facename to use for domains of NS records.")

(defconst dig-browser-font-lock-keywords
  (list
   (list "^[ \t]*\\([^ \t]+\\)[ \t]+[0-9]+[ \t]+IN[ \t]+NS[ \t]+\\([^ \t\n]+\\)$"
         '(1 dig-browser-bold-face)
         '(2 font-lock-function-name-face))
   (list "[ \t]CNAME[ \t]+\\([^ \t\n]+\\)$" 1 'font-lock-keyword-face)
   (list "[ \t]IN[ \t]+MX[ \t]+\\(.*\\)$" 1 'font-lock-string-face)
   (list "[ \t]IN[ \t]+SOA[ \t]+\\(.*\\)$" 1 'font-lock-type-face))
  "Highlighting data for Dig Browser major mode.")
  
(defconst dig-browser-syntax-table
  (let ((tbl (copy-syntax-table)))
    (modify-syntax-entry ?- "_" tbl)
    (modify-syntax-entry ?. "_" tbl)
    (modify-syntax-entry ?/ "_" tbl)
    tbl)
  "Character syntax table to use in Dig Browser major mode.")

(defconst dig-browser-imenu-generic-expression
  (list
   (list
    nil "^[ \t]*\\([^ \t]+\\)[ \t]+[0-9]+[ \t]+IN[ \t]+SOA[ \t]" 1))
  "Expression to prime Imenu in Dig Browser mode.")

(defconst dig-browser-mode-map
  (let ((kmap (make-sparse-keymap)))
    (suppress-keymap kmap)
    (define-key kmap "a" 'dig-browser-sort-by-data)
    (define-key kmap "b" 'describe-bindings)
    (define-key kmap "d" 'dig-browser-sort-by-domain)
    (define-key kmap "g" 'revert-buffer)
    (define-key kmap "h" 'describe-mode)
    (define-key kmap "i" 'dig-browser-expand)
    (define-key kmap "j" 'dig-browser-goto-domain-at-point)
    (define-key kmap "k" 'kill-this-buffer)
    (define-key kmap "l" 'dig-browser-sort-by-ttl)
    (define-key kmap "m" 'dig-browser-mail-hostmaster)
    (define-key kmap "n" 'dig-browser-next-subdomain)
    (define-key kmap "o" 'dig-browser-browse-other-window)
    (define-key kmap "p" 'dig-browser-prev-subdomain)
    (define-key kmap "q" 'quit-window)
    (define-key kmap "r" 'dig-browser-browse-reverse)
    (define-key kmap "t" 'dig-browser-sort-by-type)
    (define-key kmap "u" 'dig-browser-up-tree)
    (define-key kmap "^" 'dig-browser-browse-parent)
    (define-key kmap "$" 'dig-browser-collapse)
    (define-key kmap "\C-m" 'dig-browser-toggle-state)
    (let ((menu (make-sparse-keymap)))
      (define-key menu [quit] '("Quit" . quit-window))
      (define-key menu [separator-format-1] '("--"))
      (define-key menu [sort-by-data] '("Sort by Data" . dig-browser-sort-by-data))
      (define-key menu [sort-by-type] '("Sort by Type" . dig-browser-sort-by-type))
      (define-key menu [sort-by-ttl] '("Sort by TTL" . dig-browser-sort-by-ttl))
      (define-key menu [sort-by-domain] '("Sort by Domain" . dig-browser-sort-by-domain))
      (define-key menu [separator-format-2] '("--"))
      (define-key menu [mail-hostmaster] '("Mail Hostmaster" . dig-browser-mail-hostmaster))
      (define-key menu [browse-parent] '("Browse Parent" . dig-browser-browse-parent))
      (define-key menu [up-tree] '("Up Tree" . dig-browser-up-tree))
      (define-key kmap [menu-bar] (make-sparse-keymap))
      (define-key kmap [menu-bar dig-browser] (cons "Dig Browser" menu)))
    (define-key kmap [S-mouse-2] 'dig-browser-mouse-browse-other)
    (define-key kmap [mouse-2] 'dig-browser-mouse-toggle)
    kmap)
  "Keymap to use in Dig Browser major mode.")

(defconst dig-browser-column-alist
  (list '(domain . 0) '(ttl . 1) '(class . 2) '(type . 3) '(data . 4))
  "Dictionary of column names for Dig Browser major mode.")

(defvar dig-browser-history nil
  "History of user input for Dig Browser mode.")

 

;; internals

(defun dig-browser-make-rr ()
  "Create a resource record (a list with 5 members) from a line of dig(1) output."
  
  (save-excursion
    (let* ((beg (progn (beginning-of-line) (point)))
           (end (progn (end-of-line) (point)))
           (line (buffer-substring-no-properties beg end))
           (fields (split-string line)))
      (list (nth 0 fields) (nth 1 fields) (nth 2 fields) (nth 3 fields)
            (mapconcat 'identity (nthcdr 4 fields) " ")))))



(defun dig-browser-query (domain &optional server type)
  "Ask the DNS a question.

This is implemented by executing dig(1) as a synchronous subprocess,
and parsing its answer.  If SERVER is nil, it defaults to the value
of `dig-browser-local-server' ; if TYPE is nil, it defaults to \"any\"."
  
  (setq server (or server dig-browser-local-server))
  (setq type (or type "any"))
  (let ((b (get-buffer-create (concat " *dig @" server " " domain " " type "*")))
        (records nil))
    (prog1
        (with-current-buffer b
          (erase-buffer)
          (apply 'call-process dig-browser-program nil t nil
                 (append
                  (list
                   (format "@%s" server)
                   "-p" (int-to-string dig-browser-port)
                   "-b" dig-browser-srcaddr
                   (concat "+tries=" (int-to-string dig-browser-retry))
                   (concat "+time=" (int-to-string dig-browser-timeout)))
                  dig-browser-extra-switches
                  (list domain type)))
          (goto-char (point-min))
          (cond
           ((re-search-forward "^;;[ \t]*->>HEADER<<-.* status: \\([A-Z]+\\)" nil t)
            (let ((res (match-string 1)))
              (if (not (string-equal res "NOERROR"))
                  (error "Dig error: %s" res))))
           ((re-search-forward "^;[ \t]*Transfer[ \t]+failed" nil t)
            (error "Dig error: Transfer failed")))
          (goto-char (point-max))
          (while (re-search-backward "^[^; \t\n]" nil t)
            (setq records (cons (dig-browser-make-rr) records)))
          records)
      (kill-buffer b))))    

(defun dig-browser-maybe-map (l p f)
  "Apply F to each element of L that satisfies P, return the list of them."

  (if (null l) nil
    (let ((hd (car l)) (rest (dig-browser-maybe-map (cdr l) p f)))
      (if (funcall p hd)
          (cons (funcall f hd) rest)
        rest))))

(defun dig-browser-fetch-servers (domain)
  "Get the list of name servers authoritative for DOMAIN."

  (dig-browser-maybe-map
   (dig-browser-query domain nil "ns")
   (lambda (rr)
     (and
      (string-equal (downcase domain) (downcase (nth 0 rr)))
      (string-equal "ns" (downcase (nth 3 rr)))))
   (lambda (rr) (list (nth 4 rr)))))

(defun dig-browser-fetch-master (domain)
  "Get the master name server for DOMAIN (from its SOA record)."

  (let ((soa (dig-browser-query domain nil "soa")))
    (if (null soa) nil
      (let ((data (nth 4 (car soa))))
        (string-match "\\`[^ \t]+" data)
        (match-string 0 data)))))

(defsubst dig-browser-fetch-zone (domain server)
  "Get a list of resource records for the zone at DOMAIN.

This is implemented by a zone transfer (AXFR)."

  (message "Fetching %s from %s..." domain server)
  (let ((rrs (butlast (dig-browser-query domain server "axfr"))))
    (message "Fetching %s from %s...done" domain server)
    rrs))

(defun dig-browser-compute-widths (rrs)
  "Compute the maximum widths of the various fields of resource records RRS."

  (let ((widths (vector 0 0 0 0)))
    (while rrs
      (let* ((rr (car rrs))
             (l0 (length (nth 0 rr)))
             (l1 (length (nth 1 rr)))
             (l2 (length (nth 2 rr)))
             (l3 (length (nth 3 rr))))
        (if (> l0 (aref widths 0)) (aset widths 0 l0))
        (if (> l1 (aref widths 1)) (aset widths 1 l1))
        (if (> l2 (aref widths 2)) (aset widths 2 l2))
        (if (> l3 (aref widths 3)) (aset widths 3 l3)))
      (setq rrs (cdr rrs)))
    widths))

(defsubst dig-browser-gensym (level)
  (intern (concat "dig-level-" (int-to-string level))))

(defun dig-browser-insert-rrs (rrs server intervals)
  "Insert a textual representation of RRS at point in the current buffer.

If LEVEL is a positive number, indent all the records LEVEL times
`dig-browser-subdomain-indent' spaces, starting from column 0."

  (beginning-of-line)
  (let ((level (length intervals)))
    (let ((indent (* level dig-browser-subdomain-indent))
          (widths (dig-browser-compute-widths rrs))
          (p (point))
          (domain (caar rrs)))
      (run-hooks 'dig-browser-before-insert-hook)
      (while rrs
        (let* ((rr (car rrs)) (r0 (nth 0 rr)) (r1 (nth 1 rr))
               (r2 (nth 2 rr)) (r3 (nth 3 rr)) (r4 (nth 4 rr))
               (l1 (length r1))
               (l4 (length r4))
               (total-length
                (+ indent
                   (aref widths 0) 2
                   (aref widths 1) 2
                   (aref widths 2) 2
                   (aref widths 3) 2
                   l4 1))
               (line (make-string total-length ?\  ))
               (offset indent))
          (store-substring line offset r0)
          (if (and (string-equal r3 "NS")
                   (dig-browser-descendant-p r0 domain))
              (let ((l0 (length r0)))
                (put-text-property offset (+ offset l0) 'mouse-face 'highlight line)))
          (setq offset (+ offset (aref widths 0) 2 (- (aref widths 1) l1)))
          (store-substring line offset r1)
          (setq offset (+ offset l1 2))
          (store-substring line offset r2)
          (setq offset (+ offset (aref widths 2) 2))
          (store-substring line offset r3)
          (setq offset (+ offset (aref widths 3) 2))
          (store-substring line offset r4)
          (setq offset (+ offset l4))
          (store-substring line offset "\n")
          (insert line))
        (setq rrs (cdr rrs)))
      (save-restriction
        (narrow-to-region p (point))
        (run-hooks 'dig-browser-after-insert-hook))
      (setq intervals (cons (list domain level server 'visible) intervals))
      (put-text-property p (point) 'dig-intervals intervals)
      (let ((i level))
        (while (>= i 0)
          (put-text-property p (point) (dig-browser-gensym i) (nthcdr (- level i) intervals))
          (setq i (1- i))))
      (goto-char p)
      (back-to-indentation))))

;; defsubst
(defsubst dig-browser-intervals ()
  (get-text-property (point) 'dig-intervals))

;; return the top level domain for the buffer
(defsubst dig-browser-domain ()
  (caar (last (dig-browser-intervals))))

;; return the server from which listing was obtained
(defsubst dig-browser-server ()
  (nth 2 (car (last (dig-browser-intervals)))))

;; return interval list entry whose zone point is on
(defsubst dig-browser-interval ()
  (car (dig-browser-intervals)))

(defsubst dig-browser-interval-domain ()
  (nth 0 (dig-browser-interval)))

(defsubst dig-browser-interval-level ()
  (nth 1 (dig-browser-interval)))

(defsubst dig-browser-interval-indent ()
  (* (dig-browser-interval-level) dig-browser-subdomain-indent))

(defsubst dig-browser-interval-server ()
  (nth 2 (dig-browser-interval)))

(defsubst dig-browser-interval-start ()
  (previous-single-char-property-change
   (1+ (point))
   (dig-browser-gensym (dig-browser-interval-level))))

(defsubst dig-browser-interval-end ()
  (next-single-char-property-change
   (point)
   (dig-browser-gensym (dig-browser-interval-level))))

(defsubst dig-browser-interval-flag ()
  (nth 3 (dig-browser-interval)))

;;defsubst

(defun dig-browser-revert (ignore-auto noconfirm)
  "Refresh a buffer browsing DNS information."

  (let ((server (dig-browser-server)))
    (run-hooks 'dig-browser-before-fetch-hook)
    (let* ((d (dig-browser-domain))
           (rrs (dig-browser-fetch-zone d server)))
      (if (null rrs) (error "Unable to fetch information for %s from %s" d server)
        (run-hooks 'dig-browser-after-fetch-hook)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (redraw-frame (window-frame (selected-window)))
          (dig-browser-insert-rrs rrs server nil)))))
  (set-buffer-modified-p nil))

;; Dig Browser mode is suitable only for specially formatted data.
(put 'dig-browser-mode 'mode-class 'special)

(defun dig-browser-mode ()
  "Special major mode for browsing DNS zone information.
\\<dig-browser-mode-map> Commands:

\\[dig-browser-expand] - Expand the subdomain of an expandable NS RR.
\\[dig-browser-goto-domain-at-point] - Jump to a following resource record keyed by the domain at point.
\\[dig-browser-mail-hostmaster] - Start an email message to the address from the SOA record.
\\[dig-browser-next-subdomain] - Jump to a following NS resource record for a subdomain.
\\[dig-browser-browse-other-window] - Open a new window to browse the domain at point.
\\[dig-browser-prev-subdomain] - Jump to a preceding NS resource record for a subdomain.
\\[dig-browser-up-tree] - Go to the NS RR in the parent domain of the subdomain point is on.
\\[dig-browser-browse-parent] - Open a new window to browse the parent of the current domain.
\\[dig-browser-collapse] - Collapse the subdomain point is on.
\\[dig-browser-toggle-state] - Assuming the point is on an expandable NS RR, expand the subdomain.
\\[dig-browser-sort-by-data] - Sort the zone around point by RR data.
\\[dig-browser-sort-by-domain] - Sort the zone around point by RR domain name.
\\[dig-browser-sort-by-ttl] - Sort the zone around point by RR TTL.
\\[dig-browser-sort-by-type] - Sort the zone around point by RR type.
"

  (kill-all-local-variables)
  (setq major-mode 'dig-browser-mode
 mode-name "Dig Browser"
        buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map dig-browser-mode-map)
  (set-syntax-table dig-browser-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(dig-browser-font-lock-keywords t nil nil))
  (set (make-local-variable 'revert-buffer-function)
       (function dig-browser-revert))
  (set (make-local-variable 'selective-display) t)
  (set (make-local-variable 'selective-display-ellipses) t)
  (set (make-local-variable 'imenu-generic-expression)
       dig-browser-imenu-generic-expression)
  (set (make-local-variable 'dig-browser-end-marker)
       (make-marker))
  (imenu-add-to-menubar "Imenu")
  (run-hooks 'dig-browser-mode-hook))

(defun dig-browser-domain-at-point ()
  "Return the domain name point is on.  Signal error if point is not on a domain."

  (let* ((bs (bounds-of-thing-at-point 'sexp))
         (s (buffer-substring-no-properties (car bs) (cdr bs))))
    (if (string-match "\\`\\(\\.\\|\\([A-Za-z0-9]\\([-/A-Za-z0-9]*[A-Za-z0-9]\\)?\\.\\)+\\)\\'" s)
        s
      (error "No domain at point"))))

(defun dig-browser-goto-column (c)
  "Go to the specified column of the current RR."

  (back-to-indentation)
  (re-search-forward "[^ \t\n]+[ \t]+" nil nil (cdr (assq c dig-browser-column-alist))))

(defun dig-browser-next-rr-satisfying (restrict n p)
  "Go to the next Nth RR satisyfing P.

P should expect its argument to be a list created by `dig-browser-make-rr'.
RESTRICT tells if matches in subdomains and superdomains count."
  
  (let ((pt (point)) (limit nil) (i 0))
    (if restrict
        (let ((l (dig-browser-interval-level)))
          (save-restriction
            (narrow-to-region (dig-browser-interval-start) (dig-browser-interval-end))
            (while (< i n)
              (if (> i 0) (forward-line 1))
              (let ((rr (dig-browser-make-rr)))
                (while (or (not (funcall p rr))
                           (> (dig-browser-interval-level) l))
                  (forward-line 1)
                  (if (and limit (>= (point) limit))
                      (progn
                        (goto-char pt)
                        (error "No matching records")))
                  (if (eobp) (progn (goto-char (point-min)) (setq limit pt)))
                  (setq rr (dig-browser-make-rr))))
              (setq i (1+ i)))))
      (while (< i n)
        (if (> i 0) (forward-line 1))
        (let ((rr (dig-browser-make-rr)))
          (while (not (funcall p rr))
            (forward-line 1)
            (if (and limit (>= (point) limit))
                (progn
                  (goto-char pt)
                  (error "No matching records")))
            (if (eobp) (progn (goto-char (point-min)) (setq limit pt)))
            (setq rr (dig-browser-make-rr))))
        (setq i (1+ i))))) t)

(defun dig-browser-prev-rr-satisfying (restrict n p)
  "Go to the previous Nth RR satisyfing P.

P should expect its argument to be a list created by `dig-browser-make-rr'.
RESTRICT tells if matches in subdomains and superdomains count."
  
  (let ((pt (point)) (limit nil) (i 0))
    (if restrict
        (let ((l (dig-browser-interval-level)))
          (save-restriction
            (narrow-to-region (dig-browser-interval-start) (dig-browser-interval-end))
            (while (< i n)
              (if (> i 0) (progn (if (bobp) (goto-char (point-max))) (forward-line -1)))
              (let ((rr (dig-browser-make-rr)))
                (while (or (not (funcall p rr))
                           (> (dig-browser-interval-level) l))
                  (if (and limit (< (point) limit))
                      (progn
                        (goto-char pt)
                        (error "No matching records")))
                  (if (bobp) (progn (setq limit pt) (goto-char (point-max))))
                  (forward-line -1)
                  (setq rr (dig-browser-make-rr))))
              (setq i (1+ i)))))
      (while (< i n)
        (if (> i 0) (progn (if (bobp) (goto-char (point-max))) (forward-line -1)))
        (let ((rr (dig-browser-make-rr)))
          (while (not (funcall p rr))
            (if (bobp) (goto-char (point-max)))
            (if (and limit (< (point) limit))
                (progn
                  (goto-char pt)
                  (error "No matching records")))
            (if (bobp) (progn (setq limit pt) (goto-char (point-max))))
            (forward-line -1)
            (setq rr (dig-browser-make-rr))))
        (setq i (1+ i))))) t)

(defun dig-browser-hostmaster ()
  "Return the email address from the SOA record, as a string."

  (save-restriction
    (narrow-to-region (dig-browser-interval-start) (dig-browser-interval-end))
    (save-excursion
      (goto-char (point-min))
      (dig-browser-next-rr-satisfying t 1 (lambda (rr) (string-equal "SOA" (nth 3 rr))))
      (dig-browser-goto-column 'data)
      (skip-chars-forward "-a-zA-Z0-9.")
      (skip-chars-forward " \t")
      (let* ((bs (bounds-of-thing-at-point 'sexp))
             (s (buffer-substring-no-properties (car bs) (cdr bs))))
        (string-match "\\`\\([^.]+\\)\\.\\(.*[^.]\\)\\.?\\'" s)
        (replace-match "\\1@\\2" nil nil s)))))

(defun dig-browser-parent-domain (domain)
  "Return the domain name one level up from the argument in the DNS hierarchy."

  (if (string-equal domain ".") (error "The root domain has no parent")
    (string-match "\\`[^.]+\\.\\(.*\\)\\'" domain)
    (replace-match "\\1" nil nil domain)))

(defun dig-browser-descendant-p (subdom dom)
  "Tests if SUBDOM is a proper subdomain of DOM."

  (let ((lsub (length subdom)) (l (length dom)))
    (and (> lsub (1+ l))
         (char-equal ?. (aref subdom (- lsub l 1)))
         (string-equal (downcase dom) (downcase (substring subdom (- lsub l)))))))

(defun dig-browser-get-args (&optional domain)
  (setq domain
        (or domain
            (let ((d (read-string "Domain: " nil 'dig-browser-history)))
              (if (not (string-equal "." (substring d -1)))
                  (concat d ".") d))))
  (let ((servers (dig-browser-fetch-servers domain)))
    (if (null servers)
        (error "No name servers could be found for domain %s" domain)
      (let* ((master (dig-browser-fetch-master domain))
             (default
               (if (assoc master servers) master
                 (caar servers)))
             (server (completing-read (format "Server [%s]: " default)
                                      servers nil t nil 'dig-browser-history default)))
        (list domain server)))))

(defun dig-browser-rr-state ()
  "Test if the point is placed on an expandable NS record.

Return nil if not an expandable NS record, t if expandable but not yet expanded,
one of the symbols visible or invisible if expanded and in that state."

  (save-excursion
    (dig-browser-goto-column 'type)
    (if (not (looking-at "NS[ \t]")) nil
      (dig-browser-goto-column 'domain)
      (if (not (dig-browser-descendant-p
                (dig-browser-domain-at-point)
                (dig-browser-interval-domain))) nil
        (skip-chars-forward "^\r\n")
        (if (looking-at "\r") 'invisible
          (let ((l (dig-browser-interval-level)))
            (forward-char 1)
            (if (or (eobp) (<= (dig-browser-interval-level) l))
                t 'visible)))))))

(defun dig-browser-sort-sub (sortfun keyfun)
  "Sort the zone around point.  The leading SOA record stays fixed.

Expanded subdomains are temporarily hidden so they aren't affected."

  (let ((beg (progn (goto-char (dig-browser-interval-start)) (forward-line 1) (point)))
        (d (dig-browser-interval-domain))
        (inhibit-read-only t))
    (set-marker dig-browser-end-marker (dig-browser-interval-end))
    (while
        (condition-case nil
            (dig-browser-next-rr-satisfying
             t 1 (lambda (rr) (and (string-equal "NS" (nth 3 rr))
                                   (dig-browser-descendant-p (nth 0 rr) d)
                                   (eq (dig-browser-rr-state) 'visible))))
          (error nil))
      (dig-browser-toggle-state)
      (put-text-property (point) (1+ (point)) 'dig-hidden t))

    (goto-char beg)
    (while (< (point) dig-browser-end-marker)
      (insert (funcall keyfun (dig-browser-make-rr)) " ")
      (forward-line 1))
    (funcall sortfun 1 beg dig-browser-end-marker)
    (goto-char beg)
    (while (< (point) dig-browser-end-marker)
      (let ((p (point)) (l (skip-chars-forward "^ ")))
        (delete-region p (+ p l 1)))
      (forward-line 1))

    (goto-char beg)
    (while
        (condition-case nil
            (dig-browser-next-rr-satisfying
             t 1 (lambda (rr) (and (string-equal "NS" (nth 3 rr))
                                   (dig-browser-descendant-p (nth 0 rr) d)
                                   (get-text-property (point) 'dig-hidden))))
          (error nil))
      (dig-browser-expand)
      (remove-text-properties (point) (1+ (point)) '(dig-hidden nil)))
    (goto-char beg)
    (forward-line -1)
    (set-marker dig-browser-end-marker nil)))

 

;; Most of the sortkey functions are trivial and and it's OK to just
;; pass them as lambdas; this is the only exception, because the data
;; field is heterogenous.

(defun dig-browser-make-data-key (rr)
  "Given a resource record RR as a list, return a sort key made from its data."

  (let ((data (car (split-string (nth 4 rr)))) (quads nil))
    (cond
     ((string-match
       "\\`\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\'" data) ;ip address, format to fixed width
      (let ((o1 (match-string 1 data))
            (o2 (match-string 2 data))
            (o3 (match-string 3 data))
            (o4 (match-string 4 data)))
        (let ((i1 (string-to-int o1))
              (i2 (string-to-int o2))
              (i3 (string-to-int o3))
              (i4 (string-to-int o4)))
          (format ".%03d%03d%03d%03d" i1 i2 i3 i4))))
     ((string-match "\\`\\(\\.\\|\\([A-Za-z0-9]\\([-/A-Za-z0-9]*[A-Za-z0-9]\\)?\\.\\)+\\)\\'" data) ; if a domain, reverse it
      (mapconcat 'identity (nreverse (split-string data "\\.")) "."))
     ((setq quads (dig-browser-quads-of-ipv6 data)) ;ipv6 address
      (format ":%s" (apply 'concat quads)))
     (t data))))                        ;otherwise just leave it alone

(defun dig-browser-ip-to-arpa (ip)
  "Given an IPv4 address in octet-quad form (as a string), return its arpa domain."

  (if (not (string-match
            "\\`\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\'"
            ip)) nil
    (let ((o1 (match-string 1 ip))
          (o2 (match-string 2 ip))
          (o3 (match-string 3 ip))
          (o4 (match-string 4 ip)))
      (let ((i1 (string-to-int o1))
            (i2 (string-to-int o2))
            (i3 (string-to-int o3))
            (i4 (string-to-int o4))
            (arpa ".in-addr.arpa."))
        (if (or (> i1 223) (> i2 255) (> i3 255) (> i4 255)) nil
          (cond
           ((< i1 128) (concat o1 arpa))
           ((< i1 192) (concat o2 "." o1 arpa))
           (t (concat o3 "." o2 "." o1 arpa))))))))

(defsubst dig-browser-format-quad (q) (format "%04x" (string-to-number q 16)))

(defun dig-browser-quads-of-ipv6 (ipv6)
  "Given an IPv6 address in hex form (as a string), return list of its quads."

    (cond
     ((string-equal "::" ipv6)
      (make-list 16 "0000" ))
     ((string-match "\\`[0-9a-fA-F]+\\(:[0-9a-fA-F]+\\)+\\'" ipv6)
      (let ((quads (split-string ipv6 ":")))
        (mapcar 'dig-browser-format-quad quads)))
     ((string-match "\\`:\\(:[0-9a-fA-F]+\\)+\\'" ipv6)
      (let* ((quads (split-string ipv6 ":+")) (nquads (length quads))
             (tail (mapcar 'dig-browser-format-quad quads)))
        (append (make-list (- 8 nquads) "0000") tail)))
     ((string-match "\\`\\([0-9a-fA-F]+:\\)+:\\'" ipv6)
      (let* ((quads (split-string ipv6 ":+")) (nquads (length quads))
             (head (mapcar 'dig-browser-format-quad quads)))
        (append head (make-list (- 8 nquads) "0000"))))
     ((string-match "\\`\\([0-9a-fA-F]+\\(:[0-9a-fA-F]+\\)*\\)::\\(\\([0-9a-fA-F]+:\\)*[0-9a-fA-F]+\\)\\'" ipv6)
      (let* ((lip (match-string 1 ipv6)) (rip (match-string 3 ipv6))
             (lquads (split-string lip ":")) (rquads (split-string rip ":"))
             (nquads (+ (length lquads) (length rquads)))
             (head (mapcar 'dig-browser-format-quad lquads))
             (tail (mapcar 'dig-browser-format-quad rquads)))
        (append head (make-list (- 8 nquads) "0000") tail)))
     (t nil)))

(defun dig-browser-get-reverse-args ()

  (let* ((bs (bounds-of-thing-at-point 'sexp))
         (s (buffer-substring-no-properties (car bs) (cdr bs)))
         (arpa (dig-browser-ip-to-arpa s)))
    (if (not arpa) (error "No class A, B, or C IPv4 address at point")
      (dig-browser-get-args arpa))))         

(defun dig-browser-visible-path-p (intervals local-intervals)
  (cond
   ((null intervals)
    nil)
   ((eq local-intervals intervals)
    (eq (dig-browser-interval-flag) 'visible))
   (t
    (and (eq (nth 3 (car intervals)) 'visible)
         (dig-browser-visible-path-p (cdr intervals) local-intervals)))))

 

;; commands

;;;###autoload
(defun dig-browser (domain server)
  "Enter a buffer browsing the DNS information for DOMAIN."

  (interactive (dig-browser-get-args))
  (let ((b (get-buffer-create (format "*Dig @%s %s*" server domain))))
    (pop-to-buffer b)
    (if (= (buffer-size) 0)
        (progn
          (run-hooks 'dig-browser-before-fetch-hook)
          (let ((rrs (dig-browser-fetch-zone domain server)))
            (if (null rrs) (error "Unable to fetch information for %s from %s" domain server))
            (progn
              (run-hooks 'dig-browser-after-fetch-hook)
              (dig-browser-mode)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (dig-browser-insert-rrs rrs server nil))
              (set-buffer-modified-p nil)))))))

(defun dig-browser-goto-domain-at-point (&optional n)
  "Jump to a following resource record keyed by the domain at point.

There can be many such records; if the optional number N is present,
this command jumps to the Nth one; if N is negative, to the Nth preceding one.
Wrap around at the end of the buffer."

  (interactive "p")
  (let ((d (dig-browser-domain-at-point)))
    (cond
     ((> n 0)
      (if (eq last-command 'dig-browser-goto-domain-at-point) (forward-line 1))
      (dig-browser-next-rr-satisfying nil n (lambda (rr) (string-equal d (nth 0 rr))))
      (back-to-indentation))     
     ((< n 0)
      (if (eq last-command 'dig-browser-goto-domain-at-point)
          (progn (if (bobp) (goto-char (point-max))) (forward-line -1)))
      (dig-browser-prev-rr-satisfying nil (- n) (lambda (rr) (string-equal d (nth 0 rr))))
      (back-to-indentation))
     (t nil))))

(defun dig-browser-next-subdomain (&optional n)
  "Jump to a following NS resource record for a subdomain.

There can be many such records; if the optional number N is present,
this command jumps to the Nth one; if N is negative, to the Nth preceding one.
Wrap around at the end of the buffer."

  (interactive "p")
  (save-restriction
    (narrow-to-region (dig-browser-interval-start) (dig-browser-interval-end))
    (let* ((d (dig-browser-interval-domain)) (l (dig-browser-interval-level))
           (selective-display (* l dig-browser-subdomain-indent)))
      (cond
       ((> n 0)
        (if (eq last-command 'dig-browser-next-subdomain)
            (progn
              (forward-line 1)
              (if (> (dig-browser-interval-level) l)
                  (goto-char (dig-browser-interval-end)))
              (if (eobp) (goto-char (point-min)))))
        (dig-browser-next-rr-satisfying
         t n (lambda (rr) (and (string-equal "NS" (nth 3 rr))
                               (dig-browser-descendant-p (nth 0 rr) d))))
        (back-to-indentation))
       ((< n 0)
        (if (eq last-command 'dig-browser-next-subdomain)
            (progn
              (if (bobp) (goto-char (point-max)))
              (forward-line -1)
              (if (> (dig-browser-interval-level) l)
                  (progn
                    (goto-char (dig-browser-interval-start))
                    (forward-line -1)))))
        (dig-browser-prev-rr-satisfying
         t n (lambda (rr) (and (string-equal "NS" (nth 3 rr))
                               (dig-browser-descendant-p (nth 0 rr) d))))
        (back-to-indentation))
       (t nil)))))

(defun dig-browser-prev-subdomain (&optional n)
  "Jump to a preceding NS resource record for a subdomain.

There can be many such records; if the optional number N is present,
this command jumps to the Nth one; if N is negative, to the Nth following one.
Wrap around at the beginning of the buffer."

  (interactive "p")
  (save-restriction
    (narrow-to-region (dig-browser-interval-start) (dig-browser-interval-end))
    (let* ((d (dig-browser-interval-domain)) (l (dig-browser-interval-level))
           (selective-display (* l dig-browser-subdomain-indent)))
      (cond
       ((< n 0)
        (if (eq last-command 'dig-browser-next-subdomain)
            (progn
              (forward-line 1)
              (if (> (dig-browser-interval-level) l)
                  (goto-char (dig-browser-interval-end)))
              (if (eobp) (goto-char (point-min)))))
        (dig-browser-next-rr-satisfying
         t n (lambda (rr) (and (string-equal "NS" (nth 3 rr))
                               (dig-browser-descendant-p (nth 0 rr) d))))
        (back-to-indentation))
       ((> n 0)
        (if (eq last-command 'dig-browser-next-subdomain)
            (progn
              (if (bobp) (goto-char (point-max)))
              (forward-line -1)
              (if (> (dig-browser-interval-level) l)
                  (progn
                    (goto-char (dig-browser-interval-start))
                    (forward-line -1)))))
        (dig-browser-prev-rr-satisfying
         t n (lambda (rr) (and (string-equal "NS" (nth 3 rr))
                               (dig-browser-descendant-p (nth 0 rr) d))))
        (back-to-indentation))
       (t nil)))))

(defun dig-browser-mail-hostmaster ()
  "Start an email message to the address from the SOA record."

  (interactive)
  (message-mail (dig-browser-hostmaster) (dig-browser-interval-domain)))

(defun dig-browser-browse-parent (domain server)
  "Open a new window to browse the parent of the current domain."

  (interactive (dig-browser-get-args (dig-browser-parent-domain (dig-browser-domain))))
  (dig-browser domain server))

(defun dig-browser-browse-other-window (domain server)
  "Open a new window to browse the domain at point."

  (interactive (dig-browser-get-args (dig-browser-domain-at-point)))
  (dig-browser domain server))

(defun dig-browser-collapse ()
  "Collapse the subdomain point is on."

  (interactive)
  (let ((flag (nthcdr 3 (dig-browser-interval))))
    (setcar flag 'invisible))
  (save-restriction
    (narrow-to-region (1- (dig-browser-interval-start)) (1- (dig-browser-interval-end)))
    (goto-char (1- (point-min)))
    (save-excursion
      (let ((inhibit-read-only t))
        (while (search-forward "\n" nil t)
          (replace-match "\r"))))))

(defun dig-browser-expand ()
  "Expand the subdomain of an expandable NS RR."

  (interactive)
  (if (not (eq (dig-browser-rr-state) 'invisible))
      (error "No collapsed subdomain here"))
  (save-excursion
    (search-forward "\r")
    (let ((intervals (dig-browser-intervals))
          (flag (nthcdr 3 (dig-browser-interval)))
          (l (dig-browser-interval-level))
          (inhibit-read-only t))
      (replace-match "\n")
      (setcar flag 'visible)
      (save-restriction
        (narrow-to-region (point) (1- (dig-browser-interval-end)))
        (while (search-forward "\r" nil t)
          (if (dig-browser-visible-path-p intervals (dig-browser-intervals))
              (replace-match "\n")))))))

(defun dig-browser-up-tree ()
  "Go to the NS RR in the parent domain of the subdomain point is on."

  (interactive)
  (goto-char (dig-browser-interval-start))
  (forward-line -1)
  (back-to-indentation))

(defun dig-browser-toggle-state ()
  "Assuming the point is on an expandable NS RR, expand the subdomain.

If already expanded, toggle its visible state."

  (interactive)
  (let ((state (dig-browser-rr-state)))
    (cond
     ((null state)
      (error "Not on an expandable subdomain NS record"))
     ((eq state 'visible)
      (save-excursion
        (forward-line 1)
        (dig-browser-collapse)))
     ((eq state 'invisible)
      (dig-browser-expand))
     (t
      (dig-browser-goto-column 'domain)
      (let ((domain (dig-browser-domain-at-point))
            (server
             (progn
               (dig-browser-goto-column 'data)
               (dig-browser-domain-at-point))))
        (run-hooks 'dig-browser-before-fetch-hook)
        (let ((rrs (dig-browser-fetch-zone domain server)))
          (if (null rrs) (error "Unable to fetch information for %s from %s" domain server))
          (progn
            (run-hooks 'dig-browser-after-fetch-hook)
            (forward-line 1)
            (let ((inhibit-read-only t))
              (dig-browser-insert-rrs rrs server (dig-browser-intervals)))
            (set-buffer-modified-p nil))))))))

(defun dig-browser-sort-by-type ()
  "Sort the zone around point by RR type.  The leading SOA record stays fixed.

Expanded subdomains are temporarily hidden so they aren't affected."

  (interactive)
  (dig-browser-sort-sub
   'sort-fields
   (lambda (rr) (nth 3 rr))))

(defun dig-browser-sort-by-domain ()
  "Sort the zone around point by RR domain name.  

The leading SOA record stays fixed.  Expanded subdomains are temporarily hidden
so they aren't affected."

  (interactive)
  (dig-browser-sort-sub
   'sort-fields
   (lambda (rr)
     (mapconcat 'identity (nreverse (split-string (nth 0 rr) "\\.")) "."))))

(defun dig-browser-sort-by-ttl ()
  "Sort the zone around point by RR TTL.  The leading SOA record stays fixed.

Expanded subdomains are temporarily hidden so they aren't affected."
  
  (interactive)
  (dig-browser-sort-sub
   'sort-numeric-fields
   (lambda (rr) (nth 1 rr))))

(defun dig-browser-sort-by-data ()
  "Sort the zone around point by RR data.  The leading SOA record stays fixed.

Expanded subdomains are temporarily hidden so they aren't affected."
  
  (interactive)
  (dig-browser-sort-sub 'sort-fields 'dig-browser-make-data-key))

(defun dig-browser-mouse-browse-other (ev)
  "In another window, browse the highlighted domain on which mouse was clicked."

  (interactive "@e")
  (mouse-set-point ev)
  (if (eq (get-text-property (point) 'mouse-face) 'highlight)
      (let ((rr (dig-browser-make-rr)))
        (dig-browser (nth 0 rr) (nth 4 rr)))))

(defun dig-browser-mouse-toggle (ev)
  "Toggle the visibility of a highlighted subdomain on which mouse was clicked."

  (interactive "@e")
  (mouse-set-point ev)
  (if (and (eq (get-text-property (point) 'mouse-face) 'highlight)
           (dig-browser-rr-state))
      (dig-browser-toggle-state)))

(defun dig-browser-browse-reverse (domain server)
  "Browse the reverse (.arpa) domain of the IPv4 address at point."

  (interactive (dig-browser-get-reverse-args))
  (dig-browser domain server))

(provide 'dig-browser)

;;; dig-browser.el ends here
