;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML cleaner by Torstein Krause Johansen and Bjørn Wilhelmsen
;; 
;; This is a library, mostly inspired by Python's string library, that
;; is heavy used by the htmlcleaner.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global variables
(setq taglist ())
(setq html-cleaner-indent-level 3)
(setq indentationstring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns everything after the last '<' tag"
;; It's last start tag instead of last end tag, to 
;; include </element> tags, needed to sort out the indentation for endtags,
;; see the check-previous-code method in htmlcleaner.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-code-after-last-start-tag (ss)
  "Returns everything after the last '<' tag"
  (if (string-match "<" ss)
      (get-code-after-last-start-tag (substring ss
                                              (+ (string-match "<" ss) 1)))
    ss))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns the number of spaces indicated with numberofspaces, s is to be 
;; empty at first.
;; Usage, to get three spaces: (get-spaces 3 "")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-spaces (numberofspaces s)
  (if (> numberofspaces 0)
      (get-spaces (- numberofspaces 1) 
                  (concat s " "))
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns a string representing the current indentation, based
;; upon the length of the current element stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-indentation()
  (progn
    (setq indentation_string)
    
    (if (> (length element_stack) 0)
        (setq i (- (length element_stack) 1)) ;; Don't indent the first element.
      (setq i 0))

    (while (> i 0)
      (progn
        (setq indentation_string (concat indentation_string
                                         (make-string html-cleaner-indent-level ? )))

        (setq i (- i 1))))
    indentation_string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns t if str is in list, if not nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-in-list (str list)
  "Returns t if str is in list, if not, it returns nil."
  (if list
      (if (equal str (car list))
          t
        (is-in-list str (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage: (setq mylist '("hello" "there" "how are you?"))
;; (message (print-list mylist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-list (thelist)
  "Returns a string holding all elements of 'thelist'"
  (if thelist
      (progn
        (if (> (length (car thelist)) 0)
            (setq str (concat str (car thelist))))
        (print-list (cdr thelist)))
    str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp implementation of Python's rstrip :-)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rstrip2 (str)
  "Returns the incoming string with no leading or trailing whitespace"
   (setq return_string)
  
  (let
      ((buffer (get-buffer-create " *temp*")))
    (set-buffer buffer)
    (insert str)
;    (goto-char 0)
    (fixup-whitespace)
    (delete-blank-lines)
    (setq return_string  (buffer-string))
    (setq return_string (car (split-string return_string " ")))
          
    (kill-buffer buffer))
  return_string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun strip (str)
  ;; Leading newlines, spaces and tabs

  (if (and str 
           (not (equal (length str) 0)))
      (progn
         (while 
             (and (> (length str) 0)
                  (or (equal (string-match "\n" str) 0)
                      (equal (string-match " " str) 0)
                      (equal (string-match "\t" str) 0)))
           (setq str (substring str 1)))

         ;; Trailing whitespace
         (while 
             (and (> (length str) 0)
                  (or (equal (substring str 
                                        (- (length str) 1)
                                        (- (length str) 0))
                             " ")
                      (equal (substring str 
                                        (- (length str) 1)
                                        (- (length str) 0))
                             "\n")
                      (equal (substring str 
                                        (- (length str) 1)
                                        (- (length str) 0))
                             "\t")))
           (setq str (substring str 0 
                                (- (length str) 1))))
       str)
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Works like Python's replace.
;; Replaces all occurances of 'old' with 'new' in 'str'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun replace (str old new)
  (if (string-match old str)
      (progn
        (setq str (concat 
                   (substring str 0 (string-match old str))
                   new
                   (substring str (+ (length old)
                                     (string-match old str)))))
        (replace str old new))
    str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replaces all ['s and ]'s with htmlcleaner-start-bracket 
;; and htmlcleaner-end-bracket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-rid-of-lisp-specials (str)
  "Replaces all left and right brackets with the strings
html-start-bracket and html-end-bracket respectively, this is done due to the
problems with the lisp regexp and brackets"

  (if (or (string-match "\\[" str)
          (string-match "\\]" str)
          (string-match "?" str)
          (string-match "*" str))
      (if (string-match "\\[" str)
          (get-rid-of-lisp-specials (concat (substring str  0
                                                  (string-match "\\[" str))
                                       "htmlcleaner-start-bracket"
                                       (substring str (+ 1 (string-match "\\[" str)))))
        (if (string-match "\\]" str)
            (get-rid-of-lisp-specials (concat 
                                       (substring str 0
                                                  (string-match "\\]" str))
                                       "htmlcleaner-end-bracket"
                                       (substring str (+ 1 (string-match "\\]" str)))))
        (if (string-match "*" str)
            (get-rid-of-lisp-specials (concat 
                                       (substring str 0
                                                  (string-match "*" str))
                                       "htmlcleaner-asterix"
                                       (substring str (+ 1 (string-match "*" str)))))
          ;; question marks: ?
          (get-rid-of-lisp-specials (concat 
                                     (substring str 0
                                                (string-match "?" str))
                                     "htmlcleaner-question-mark"
                                     (substring str (+ 1 (string-match "?" str))))))))
        str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A method again ispired by Python.
;; Returns the i occurences of substr in str.
;; Case insensitive 
;; Usage: (count "a" "Alabama" 0) -> 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count (substr str i)
  (if (string-match (downcase substr) (downcase str))
      (count substr (substring str 
                               (+ (string-match (downcase substr) (downcase str))
                                  (length substr)))
             (+ i 1))
    i))
