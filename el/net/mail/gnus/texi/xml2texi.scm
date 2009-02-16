;;; xml2texi.scm --- Convert gnus-faq.xml to gnus-faq.texi
;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author:  Karl Pflästerer <sigurd@12move.de>
;; Keywords: tools

;; This file is not part of GNU Emacs.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 

;;; Code:

(require (lib "ssax.ss" "ssax")
         (lib "sxpath.ss" "ssax")
         (lib "sxml-tree-trans.ss" "ssax")
         (lib "pregexp.ss")
         (lib "list.ss")
         (lib "etc.ss")
         (rename (lib "1.ss" "srfi") list-index list-index)
         (rename (lib "13.ss" "srfi") string-join string-join))


;;; Constants
;; In and out; for convenience if we work from the REPL
(define +infile+ "gnus-faq.xml")
(define +outfile+ "gnus-faq.texi")

;; These are the names of the sections.  These variables hold the names
;; of the sections where numbering starts in the main menu.
;; Where we start numbering in menu
(define +first-numbered-section+ "Installation FAQ")
;; Where we end numbering in menu
(define +last-numbered-section+ "Tuning Gnus")

;; Which sections not to include; i.e. not to name a node.
(define +ignored-sections+ '("Frequently Asked Questions with Answers"))

;; Names of menu entries and the corresponding descriptions (used in the
;; main menu).
(define +section-comments-alist+
    '(("Introduction" . "About Gnus and this FAQ.")
      ("Installation FAQ" . "Installation of Gnus.")
      ("Startup / Group buffer" . "Start up questions and the first buffer Gnus shows you.")
      ("Getting Messages" . "Making Gnus read your mail and news.")
      ("Reading messages" . "How to efficiently read messages.")
      ("Composing messages" . "Composing mails or Usenet postings.")
      ("Old messages" . "Importing, archiving, searching and deleting messages.")
      ("Gnus in a dial-up environment" . "Reading mail and news while offline.")
      ("Getting help" . "When this FAQ isn't enough.")
      ("Tuning Gnus" .  "How to make Gnus faster.")
      ("Glossary" . "Terms used in the FAQ explained.")))

;; Where to break descriptions in menus
(define +width+ 72)

;; The boilerplate text we include before the document
(define boilerplate
    (lambda (titel)
      (format
       "\
@c \\input texinfo @c -*-texinfo-*-~%\
@c Uncomment 1st line before texing this file alone.~%\
@c %**start of header~%\
@c Copyright (C) 1995, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.~%\
@c~%\
@c Do not modify this file, it was generated from gnus-faq.xml, available from~%\
@c <URL:http://my.gnus.org/FAQ/>.~%\
@c~%\
@setfilename gnus-faq.info~%\
@settitle ~A~%\
@c %**end of header~%\
@c~%\
" titel)))

;; Inserted right before the end of the file
(define +tag-for-gnus-faq-texi+
    (format "\
~%\
@ignore~%\
arch-\
tag: 64dc5692-edb4-4848-a965-7aa0181acbb8~%\
@end ignore~%\
"))

;;; Little Helpers
;; (a b c) -> (1 2 3)
(define (number-list start inc lst)
    (let loop ((lst lst) (lvl start) (acc '()))
         (if (null? lst)
           (reverse acc)
           (loop (cdr lst) (+ inc lvl) (cons lvl acc)))))

;; Given an alist made of regexps and their replacements (key and value
;; are in a proper list) returns a function which given a string
;; replaces all occurences of the regexps (from left to right).
;; ((re1 repl1) (re2 repl2)) -> str -> str
(define make-reg-replacer
    (lambda (defalist)
      (let ((allreg (string-join (map car defalist) "|")))
        (lambda (str)
          (if (and (string? str) (pregexp-match allreg str))
            (let loop ((lst defalist) (str str))
                 (if (null? lst)
                   str
                   (loop (cdr lst) (pregexp-replace* (caar lst) str (cadar lst)))))
            str)))))

(define escape-texi
    (make-reg-replacer '(("@"  "@@") ("{"  "@{") ("}"  "@}"))))

(define normalize
    (compose escape-texi (make-reg-replacer `((,(format "~%\\s+") ,(format "~%"))))))

(define normalize-example
    (compose escape-texi (make-reg-replacer '(("^\\s+|\\s+$" "")))))

(define trim-ws (make-reg-replacer '(("^\\s+|\\s+$" ""))))

(define filter-sect
    (lambda (lst)
      (filter (lambda (e) (not (member e +ignored-sections+))) lst)))

;;;; Para
(define format-para
    (lambda (list-of-entries)
      (format "~%~A~%" (trim-ws (apply string-append list-of-entries)))))

;;;; Questions
(define format-q-level
    (lambda (level)
      (apply format "[~A.~A]" (reverse level))))

(define format-q-description
    (compose trim-ws (make-reg-replacer `((,(format "~%") " ")))))

;;;; Building nodes
;; curr-node up-node (list of nodes) (list of node names) ->
;;   ((curr-node curr-name) (next next-name) (prev prev-name) up)
(define (find-prev-next-up curr up search-list name-list)
    (do ((lst   search-list (cdr lst))
         (rlst  name-list   (cdr rlst))
         (prev  up   (car lst))
         (prevn up   (car rlst)))
        ((or (null? lst) (equal? (car lst) curr))
         (values (cons curr (if (pair? rlst) (car rlst) curr))
                 (if (and (pair? lst) (pair? (cdr lst))) ;next
                   (cons (cadr lst) (cadr rlst))
                   (cons "" ""))
                 (cons prev prevn)
                 up))))


(define (format-node section title up lst-of-nodes lst-of-names)
    (if (member title +ignored-sections+)
      ()
      (call-with-values
       (lambda () (find-prev-next-up title up lst-of-nodes lst-of-names))
       (lambda (currn prevn nextn up)
         (format "~%@node ~A~%~A ~A~%"
                 (cdr currn) ;; (cdr prevn) (cdr nextn) up
                 section ;; @subsection etc.
                 (if (pair? title)
                   (apply format "~A.~A" (reverse title))
                   title))))))

;;;; Building menus

(define format-menu
    (lambda (alist-of-entries)
      (let ((len (apply max (map (lambda (s) (string-length (car s))) alist-of-entries))))
        (format "~%@menu~%~A@end menu~%"
                (apply string-append
                       (map (lambda (e)
                              (format "* ~A::~A~A~%"
                                      (car e) ;the entry
                                      (make-string (- len (string-length (car e)) -3) #\ )
                                      (format-menu-description (cdr e) +width+ (+ len 7))))
                            alist-of-entries))))))


(define format-menu-description
    (lambda (entry width offset)
      (let loop ((lst (pregexp-split "\\s" entry)) (len 0) (acc '()))
           (if (null? lst)
             (apply string-append (reverse! acc))
             (let ((slen (+ 1 (string-length (car lst))))) ; +1 because of whitespace added later
               (if (> (+ slen len) (- width offset))
                 (loop (cdr lst) 0 (cons
                                    (format "~%~A ~A"                 ; start a new line
                                            (make-string offset #\ ) ; the whitespace
                                            (car lst))
                                    acc))
                 (loop (cdr lst) (+ slen len) (cons (format " ~A"(car lst)) acc))))))))


(define format-sub-titles
    (lambda (list-of-entries first-number-entry last-number-entry)
      (let ((offset (or (list-index (lambda (e) (equal? e first-number-entry)) list-of-entries) 0))
            (end (or (list-index (lambda (e) (equal? e last-number-entry)) list-of-entries)
                     (length list-of-entries))))
      (map (lambda (entry ind)
             (format "FAQ ~A ~A"
                     (if (<= offset ind end)
                       (format "~A -" (- ind offset -1)) ;numbered entry
                       "-")
                     entry))
           list-of-entries (number-list 0 1 list-of-entries)))))

;;;; We number some sections first

;; ntags is an alist => ((tag startcounter increment)
(define (number-nodes tree level ntags)
    (if (null? ntags)
      tree
      (let* ((vals  (car ntags))
             (ntag  (car vals))
             (start (second vals))
             (inc   (third vals))
             (ntags (cdr ntags)))

        (map
         (lambda (node sublevel)
           (pre-post-order
            node
            `((,ntag *preorder*
                     . ,(lambda (tag . entry)
                          `(,tag ,(cons sublevel level)
                                 ,@(number-nodes entry (cons sublevel level) ntags))))
              (*default* . ,(lambda x x))
              (*text* . ,(lambda (tag s) s)))))
         tree (number-list start inc tree)))))


;;(transform->numbered faqsxml '(section article qandaset ((qandadiv 1 1) (qandaentry 0 1))))
(define transform->numbered
    (lambda (sxml rules)
      (let* ((rules (reverse rules))
             (rule (car rules))
             (ntag (cadr rules))
             (styles (map (lambda (tag) (cons tag (lambda x x))) (list-tail rules 2))))
  (pre-post-order
   sxml
     `((*default* *preorder* . ,(lambda x x))
       (*TOP* . ,(lambda x x))
       ,@styles
       (,ntag *preorder*
        . ,(lambda (tag . nodes)
             (cons tag (number-nodes nodes '() rule)))))))))


;;;; The main transform function

(define (transform sxml)
    (let* ((sxml (transform->numbered
                  sxml '(section article qandaset ((qandadiv 1 1) (qandaentry 0 1)))))
           (qandadivtitles (filter-sect (map second ((sxpath '(// qandadiv title)) sxml))))
           (fqandadivtitles (format-sub-titles qandadivtitles "" ""))
           (subtitles (filter-sect (append (map second ((sxpath '(// section title)) sxml))
                                           qandadivtitles
                                           (map second ((sxpath '(// glossary title)) sxml)))))
           (fsubtitles (format-sub-titles subtitles +first-numbered-section+
                                          +last-numbered-section+))
           (questlevel (map second ((sxpath '(article section qandaset qandadiv qandaentry)) sxml)))
           (up1 (cadar ((sxpath '(article articleinfo title)) sxml)))

;;; ************************************************************
;;; The Style Sheet
;;; ************************************************************
           (style-sheet
             `(
;;; ************************************************************
;;; First the SXML special markers
;;; ************************************************************
               ;; *TOP* *PI* @ are markers from SXML
               (*TOP* . ,(lambda (tag . x) x))
               (*PI* . ,(lambda _ '()))
               (@ . ,(lambda _ ""))

               ;; Look for the example rule where we overwrite the *text* rule
               ;; so code doesn't get mangled.
               (*text*
                . ,(lambda (tag string)
                     (normalize string)))
               ;; If nothing else matches
               (*default* . ,(lambda x x))
;;; ************************************************************
;;; Now to the tags of our FAQ
;;; ************************************************************
               (article . ,(lambda (tag . sects)
                             (list (boilerplate up1) sects 
                                   +tag-for-gnus-faq-texi+)))

               (articleinfo
                ((*default* . ,(lambda _ '()))
                 (title
                  . ,(lambda (tag titel)
                       (let ((menucom (map (lambda (entry)
                                             (let ((e (assoc entry +section-comments-alist+)))
                                               (if e (cdr e) "")))
                                           subtitles)))
                         (list (format-node '@section titel "" '() '())
                               (format-menu (map cons fsubtitles menucom)))))))
                . ,(lambda (tag . info) info))

               ;; Sections
               (abstract
                . ,(lambda (tag . text)
                     (cons (format "~%@subheading Abstract~%") text)))
               (section
                ((title
                  . ,(lambda (tag titel)
                       (format-node '@subheading titel up1 subtitles fsubtitles))))
                . ,(lambda (tag . entry) entry))

               ;; Q&A well it's called FAQ isn't it?
               (qandaset . ,(lambda (tag . x) x))
               (qandadiv
                ((title
                  . ,(lambda (tag titel) titel)))
                . ,(lambda (tag level titel . entries)
                     (let ((questions (map cadr entries))
                           (nlevel (filter (lambda (lvl) (eq? (car level) (cadr lvl))) questlevel)))
                       (list*
                        (format-node '@subsection titel up1 subtitles fsubtitles)
                        (format-menu (map (lambda (lvl quest)
                                            (cons (format-q-level lvl)
                                                  (format-q-description quest)))
                                          nlevel questions))
                        entries))))
               (qandaentry
                . ,(lambda (tag level question answer)
                     (let ((nodes
                             (filter (lambda (lvl) (eq? (cadr lvl) (cadr level))) questlevel))
                           (up (list-ref fqandadivtitles (- (cadr level) 1))))
                       (list*
                        (format-node "@subsubheading Question" level up nodes (map format-q-level nodes))
                        question answer))))
               (question . ,(lambda (tag quest) quest))
               (answer
                . ,(lambda (tag  . answ) (list* (format "~%@subsubheading Answer~%") answ)))

               ;; Para
               (para . ,(lambda (tag . x) (format-para x)))
               (simpara . ,(lambda (tag . x) (cons (format "~%")  x)))

               ;; Itemized lists.
               ;; We rewrite para here because it plays here the role of an
               ;; item marker
               (itemizedlist
                . ,(lambda (tag lstitem)
                     (format "~%@itemize @bullet~%~A@end itemize~%" lstitem)))
               (listitem
                ((para
                  . ,(lambda (tag item)
                       (format "~%@item~%~A~%" (trim-ws item)))))
                . ,(lambda (tag . x) (string-join x "")))

               ;; The glossary.
               (glossary
                ((title . ,(lambda _'())))
                . ,(lambda (tag . terms)
                     (let ((titel (cadar ((sxpath '(article glossary title)) sxml))))
                       (cons (format-node '@subsection titel up1 subtitles fsubtitles)
                             (list (format "~%@table @dfn~%")
                                   terms
                                   (format "~%@end table~%"))))))
               (glossentry . ,(lambda (tag . entry) entry))
               (glossterm
                . ,(lambda (tag term)
                     (format "~%@item ~A" term)))
               (glossdef
                . ,(lambda (tag def) def))

               ;; Lisp examples
               ;; We rewrite the *text* rule so code stays the way it's writen.
               (programlisting
                ((*text*
                  . ,(lambda (tag exampl)
                       (normalize-example exampl))))
                . ,(lambda (tag . exampl)
                     (format "~%@example~%~A~%@end example~%@noindent~%" (string-join exampl ""))))

               ;; The link handling
               ;; Here we are interested in the attributes, so we rewrite the @
               ;; rule.  If we find a value we look if it's an email or http
               ;; uri.
               (ulink
                ((@
                  . ,(lambda (at val) val)))
                . ,(lambda (tag uri name)
                     (if (pregexp-match "^http:|^ftp:" uri)
			 (if (equal? uri name)
			     (format "@uref{~A}"  uri)
			     (format "@uref{~A, ~A}"  uri name))
			 (format "@email{~A, ~A}" (substring uri 7) name))))
               (url
                . ,(lambda (tag val) val))

               ;; userinput
               (userinput
                . ,(lambda (tag val)
                     (format "@samp{~A}" val)))
               )))
      (pre-post-order sxml style-sheet)))

;;;; We call main with infile and outfile as arguments
(define main
    (lambda (in out)
      (with-output-to-file out
        (lambda ()
          (call-with-input-file in
            (lambda (port)
              (SRV:send-reply (transform (ssax:xml->sxml port '()))))))
        'replace)))

;; Local Variables:
;; coding: iso-8859-1
;; End:

;; arch-tag: cdd948f7-def9-4ea1-b5ae-b57c308097d7
;;; xml2texi.scm ends here
