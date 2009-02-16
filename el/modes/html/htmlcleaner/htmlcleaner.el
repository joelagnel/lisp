;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML cleaner by Torstein Krause Johansen and Bjørn Wilhelmsen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "htmlcleaner-lib.el")
(load "inspector.el")
(setq empty_elements '("br" "img" "hr" "meta" "link" "area" "input"))
(setq element_stack)
(setq element_value_width 35)
(setq lowercase_elements t)
(setq indent-comments t)
(setq clouseau t) ;; The inspector is awake by default
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Takes into acccount empty elements written in XML style 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-end-tag ()
  "Takes into acccount empty elements written in XML style."
  (if (string-match "/>" element)
      " />"
    ">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just a concatination that is used a lot in the do-it method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-element ()
  "Just a concatination that is used a lot in the do-it method."
  (concat
   (get-indentation)
   "<"
   (fix-element-case element_name)
   (if element_attributes
       (concat " " 
               (if (string-match "!doctype" (downcase element_name))
                   (strip element_attributes)
                 (fix-attributes element_attributes))
               (if (> (count "=" element_attributes 0) 
                      1)
                   (concat
                    "\n"
                    (get-indentation)
                    (get-end-tag))
                   (get-end-tag)))

     (get-end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fixing the element attributes
;;
;; All attributes needs to be seperated with spaces, so 
;; this have to be fixed in advance, before using this method,
;; e.g. (replace str "\n" " ")
;;
;; The get-first/last-of-att-pair methods
;; returns the previous value and the current attribute name 
;; respectively, an att-pair, will usually look like this:
;; "\"valueof the last attribute\" currentattributename".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-first-of-att-pair (attpair)
  (setq attpair (strip attpair))

  (if (string-match "\"" attpair)
      (if (= (string-match "\"" attpair) 0)
          (substring attpair 1 
                     (+ 1
                     (string-match "\"" (substring attpair 1))))
        (substring attpair 0 (string-match "\"" attpair)))
    (substring attpair 0 (string-match " " attpair))))

(defun get-last-of-att-pair (attpair)
  (setq attpair (strip attpair))
  (if (string-match " " attpair)
      (get-last-of-att-pair (substring attpair
                                       (+ 1
                                       (string-match " " attpair))))
    (replace attpair "\"" "")))

(defun fix-attributes (attributes)
  "Method that fixes the elements' attributes. Indentents them on the
same column, if therer are more than one."
  (setq attlist (split-string (replace attributes "\n" " ") 
                              "="))
  (setq finishedattrs)

  (while attlist
    ;; List atoms will usually contain two things:
    ;; the previous attribute's value and the current attribute name,
    ;; thus the methods get-first/last-of-att-pair.
    (if finishedattrs 
        (setq finishedattrs (concat finishedattrs
                                    "="
                                    "\""
                                    (get-first-of-att-pair (car attlist))
                                    "\""
                                    ;; A little hack ;-)
                                    (if (and (not (equal (get-first-of-att-pair (car attlist))
                                                         (get-last-of-att-pair (car attlist))))
                                             (not (string-match (get-last-of-att-pair (car attlist))
                                                                (get-first-of-att-pair (car attlist)))))
                                        (concat
                                         "\n"
                                         (get-indentation) 
                                         (get-spaces (+ 2 (length element_name)) "")
                                         (get-last-of-att-pair (car attlist))))))
      ;; The first attribute atom, i.e. either a name, value or boolean ( <input checked> )
      (setq finishedattrs (get-last-of-att-pair (car attlist))))
    (setq attlist (cdr attlist)))
  finishedattrs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixing the element case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fix-element-case (e)
"Fixes the case of the element, given by the lowercase_elements variable"
  (if lowercase_elements
      (downcase e)
    (upcase e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage: (get-next-space-in-string "hei på deg arne" 1)
;; evw is start index in sl, the string.
;; evw - element_value_width. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-next-space-in-string (sl evw)
  (if (and (> (length sl) 
              (+ evw 1))
           (not (equal (substring sl evw (+ evw 1))
                       " ")))
      (get-next-space-in-string sl (+ evw 1))
    evw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixing the real contents of the document
;; Wrapping long texts into text blocks, that wraps at 
;; a given width, given by the element_value_width variable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fix-element-value (s)
  (setq ts)
  (setq s (strip s))
  (setq s (replace s "\n" " "))
  (setq s (replace s "  " " "))

  (while (> (length s) element_value_width)
    (progn
      (setq ts (concat ts
                       "\n"
                       (progn
                         (if (and (not (is-in-list (downcase element_name) empty_elements))
                                  (not (equal (string-match "/" (strip element))
                                              (- (length element) 2))))
                             (setq element_stack (cons  "temp" element_stack)))
                         (get-indentation))
                       (progn
                         (if (and (not (is-in-list (downcase element_name) empty_elements))
                                  (not (equal (string-match "/" (strip element))
                                              (- (length element) 2))))
                             (setq element_stack (cdr element_stack)))
                         (substring s 0
                                    (get-next-space-in-string s
                                                              element_value_width)))))
      (setq s (substring s (+ (get-next-space-in-string s
                                                        element_value_width) 1))))) 
  (if ts
      (concat ts
              "\n" 
              (progn
                (if (and (not (is-in-list (downcase element_name) empty_elements))
                         (not (equal (string-match "/" (strip element))
                                     (- (length element) 2))))
                    (setq element_stack (cons  "temp" element_stack)))
                (get-indentation))
              s
              (progn
                (setq element_stack (cdr element_stack))
                ""))
    s))

;; Error here, erases last character when value is around the element_value_width
;;(fix-element-value "hei på deg, hvordan står det til, det hei på deg")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns true if the end element needs a get-element, because the
;; previous code was a block of element-values, e.g. a lot of text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-previous-code (pc)
  "Helps the end elements to get the proper indentation, or the
lack of it, in case of previous element values being short."
  (setq temp (strip (get-code-after-last-start-tag pc)))
  (setq aftertag (substring temp (+ 1 (string-match ">" temp))))

  (if (and (< (length aftertag)
              element_value_width)
           (not (= (length aftertag) 0)))
      (if (string-match "/" temp)
          (if (or (= (string-match "/" temp) 0)
                  (< (length (strip aftertag)) 1))
              t)
        nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Does the dirty work, that is, the cleaning.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do-it (html_string)
  (setq return_string)
  (setq element_stack)
  (setq element_name)

  (while (and (> (length html_string) 0)
              (string-match "<" html_string)
              (string-match ">" html_string))
    (progn
      (setq element (substring html_string 
                               (string-match "<" html_string)
                               (+ (string-match ">" html_string) 
                                  1)))

      (setq element_name (substring element 
                                    1
                                    (if (string-match " " element)
                                        (string-match " " element)
                                      (string-match ">" element))))

      (setq element_attributes (if (string-match " "  element)
                                   (substring element
                                              (string-match " " element)
                                              (if (string-match "/>" element)
                                                  (string-match "/>" element)
                                                (string-match ">" element)
                                                ))))

      ;; A little thorough code to make it easier to read :-)
      (if (< (length (strip element_attributes)) 1)
          (setq element_attributes))


      ;; JavaScript, JScript sections.
      (if (equal "script" (downcase (strip element_name)))
          (progn
            (setq return_string (concat return_string
                                        (get-indentation)
                                        (get-spaces html-cleaner-indent-level "")
                                        (substring html_string
                                                   (string-match "<script" html_string)
                                                   (+ (string-match "</script>" html_string)
                                                      (length "</script>")))
                                        "\n"))
            (setq html_string (substring html_string
                                         (+ (string-match "</script>" html_string)
                                            (length "</script>")))))
        ;; Comments
        (if (string-match "<!--" element)
            (progn
              (setq return_string (concat return_string
                                          (get-indentation)
                                          (if indent-comments
                                              (get-spaces html-cleaner-indent-level ""))
                                          (substring html_string
                                                     (string-match "<!--" html_string)
                                                     (+ (length "-->")
                                                        (string-match "-->" html_string)))
                                          "\n"))
              (setq html_string (substring html_string
                                           (+ (string-match "-->" html_string)
                                              (length "-->")))))
          ;; Normal elements
          (progn
            (setq html_string (strip (substring html_string 
                                         (+ (string-match element html_string)
                                            (length element)))))

            (setq element_value (strip (substring html_string 0
                                                  (string-match "<" html_string))))
            (if (< (length element_value) 1)
                (setq element_value))

            (if (not (equal (string-match "/" element_name) 0))
                (progn
                  (if (not (string-match "<!doctype" (downcase element)))
                      (setq element_stack (cons element_name element_stack)))
                  
                  (setq return_string (concat return_string 
                                              (print-element)
                                              (if element_value
                                                  (fix-element-value element_value))
                                              (if (or (is-in-list (downcase element_name) 
                                                                  empty_elements)
                                                      (> (length element_value) 
                                                         element_value_width)
                                                      (not element_value))
                                                  "\n")))
                  (if (or (is-in-list (downcase element_name) empty_elements)
                          (equal (string-match "/" (strip element))
                                 (- (length element) 2)))
                      (setq element_stack (cdr element_stack))))

              ;; End elements
              (progn
                (setq return_string (concat return_string
                                            (if (check-previous-code return_string)
                                                (get-indentation))
                                            "<"
                                            (fix-element-case element_name)
                                            ">"
                                            "\n"

                                            ;; Code after an end tag
                                            (if element_value
                                                (progn
                                                  (if (> (length element_value)
                                                         element_value_width)
                                                      (setq element_stack (cdr element_stack)))
                                                  (setq tt
                                                        (concat (get-indentation)
                                                                (fix-element-value (strip element_value)) 
                                                                "\n"))
                                                  (if (> (length element_value)
                                                         element_value_width)
                                                      (setq element_stack (cons "temp" element_stack)))
                                                  tt))))
                (setq element_stack (cdr element_stack)))))))))
  return_string)

;; Changing the default Emacs lisp-eval-depth,
;; in order to have some heavy recursion, needed by complex documents.
(setq max-lisp-eval-depth 3000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main method invoked by the user to clean his/her HTML/XML file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun html-cleaner ()
  (interactive)
  (setq oldpoint (point))

  (save-excursion
    (progn
      (setq html_string (replace (buffer-string) "" ""))
      
      (if clouseau
          (progn
            (tag-check html_string)
            (tag-name-check html_string)))
      (setq element_stack)
      (setq return_string)
      (setq html_string (get-rid-of-lisp-specials html_string))
      (setq temp_string html_string) 

      (do-it temp_string)

      ;; Emacs has problems with special characters used in regexps
      ;; when searching, replacing, substringing strings.
      ;; I tried to escape these, but bumped occationally into errors, that
      ;; made these lines necessary.
      (setq return_string (replace return_string "htmlcleaner-start-bracket" "["))
      (setq return_string (replace return_string "htmlcleaner-end-bracket" "]"))
      (setq return_string (replace return_string "htmlcleaner-question-mark" "?"))
      (setq return_string (replace return_string "htmlcleaner-asterix" "*"))

      (kill-region (point-min) (point-max))
      (erase-buffer)
      (insert return_string)))
  (goto-char oldpoint))

