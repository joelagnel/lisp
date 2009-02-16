;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;<!-- Error: Tag </sdf> doesn't match previous element. -->;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML cleaner by Torstein Krause Johansen and Bjørn Wilhelmsen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains the tools for checking and testing tags                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


(defun remove-legals (str)
  ;; Removes tags which can be unpleasant while checking for legal tags
  ;; Called by tags-check only.
  ;; 3 and 9 are the length of "-->" and "</script> respectively.
  (setq scriptlength)
  (while (string-match "<!--" str)
    (setq str (concat (substring str
                                 0
                                 (string-match "<!--" str))
                      (substring str
                                 (+ (string-match "-->" str) 3)
                                 (length str)))))

  (while (string-match "<script" str)
    (setq scriptlength (length (substring str
                                          (string-match "<script>" str)
                                          (+ (string-match "</script>" str) (length "</script>")))))
    (setq str (concat (substring str
                                 0
                                 (string-match "<script" str))
                      (substring str
                                 (+ (string-match "</script>" str) 9)
                                 (length str)))))
  str)

(defun tag-check (str)
  ;; This is the method called first in the cleaning process.
  ;; Checks if tags are matching, i.e <>, not << or >>
  (setq str (remove-legals str))
  (setq stack ()) ;; The stack for pushing and popping tags
  (setq index 0)  ;; The counter for controlling the loop
  (if (not (or (string-match "<" str) 
               (string-match ">" str)))
      (progn 
        (print "Error: No tags found. Non-valid HTML/XML")
        nil)
    (progn
      (while (< index (length str))
        (progn
          ;; Create a shrinking substring for searching
          (setq tag (substring str index (+ index 1)))
          ;; If we didn't encounter a tag, continue
          (if (or (equal tag "<") (equal tag ">"))
              (progn
                ;; If we meet two start-tags in a row
                ;; goto place where the error occured and
                ;; insert an errormessage. Then abort.
                (if (and (equal tag "<") stack)
                    (progn
                      (goto-char (1+ index ))
                      (insert "<!-- The following tag is wrong! -->")
                      (setq quit-flag t)))
                ;; If we encounter correct nesting
                ;; push the stack and continue
                (if (and (equal tag "<") (not stack))
                    (progn
                      (setq stack (cons 'dummy stack))
                      (setq index (+ index 1))))
                ;; If we get an end-tag before a start tag
                ;; goto place where the error occured
                ;; and insert an errormessage. Then abort. 
                (if (and (equal tag ">") (not stack))
                    (progn
                      (goto-char (+ (1+ index) scriptlength))
                      (insert "<!-- The following tag is wrong!! -->")
                      (setq quit-flag t)))
                ;; Correct nesting. Pop stack and continue
                (if (and (equal tag ">") stack)
                    (progn
                      (setq stack ())
                      (setq index (+ index 1)))))
            (setq index (1+ index))))
        t) 
      (if stack 
          (progn 
            (print "Error: End-tag missing")
            nil)
        t))))
;; This value is returned if all is well

(defun tag-name-check (str)
  ;; Method for checking if tagnames appear in the right order, i.e has the user
  ;; been using correct HTML-syntax.
  ;; This method is called right after the tag-check, and is only called if 
  ;; the tag-check returned true.  
  ;; Uses methods is-empty-element and compare-tags for comparing strings.
  
  ;; Remove whitespace
  (setq str (strip str))
  (setq stack ())
  (setq index 0)
  
  (setq scriptlength)

  (setq str (remove-script str))

  (while (> (length str) 0)
    (progn
      (setq tag_start (string-match "<" str))
      (setq tag_end (string-match ">" str))
      ;; prepare string for comparison against empty_elements
      (setq element (substring str
                               tag_start
                               (1+ tag_end)))
      (setq index (+ index 1 tag_end ))
      
      ;; Empty elements and xml style tags do not always need
      ;; end elements. To avoid a mismatch, we just "skip"
      ;; these strings.x
      (if (or (is-empty-element element) (is-xml-style element)) 
          ;; Make sure we don't get out of array bounds
          (progn
            (if (< tag_end (1- (length str)))
                (progn 
                  ;;Denne funker som bare fy
                  (setq str (substring str
                                       (1+ tag_end)
                                       (length str)))
                  t)
              ;; If we are at the end of the string
              ;; set length of string to zero to escape while-loop
              (setq str "")))
        ;; else we have to check some stuff
        (progn
          (if (string-match "/" element)
              (progn
                (if stack
                    (progn
                      (if (compare-tags (car stack)
                                        element)
                          ;; We have a match. Pop the stack
                          (progn
                            (setq stack (cdr stack))
                            
                            ;; Make sure we don't exceed the stringlength
                            (if (< tag_end (1- (length str)))
                                (progn
                                  (setq str (substring str
                                                       (1+ tag_end)
                                                       (length str)))
                                  )
                              (setq str "")))                        
                        ;; No match, we have an error. Output and abort
                        (progn
                          ;; Goto the place before the error occurred
                          (goto-char (+ (- index (1- (length element))) scriptlength))
                          (insert "<!-- Error: No start tag matches " element " -->")
                          (setq quit-flag t))))
                  ;; The stack is empty. Output error and abort
                  (progn
                    ;; Goto the place before the error occurred
                    (goto-char (+ (- index (1- (length element))) scriptlength))
                    (insert "<!-- Error: Tag " element " doesn't match previous element. -->")
                    (setq quit-flag t))))
            ;; Else it is a start tag.
            ;; Push the tag on the stack
            ;; Increase index to tell where we are in "str"
            (progn
              ;; don't exceed the length of the string, remember?
              (if (< tag_end (1- (length str)))
                  (setq str (substring str
                                       (1+ tag_end)
                                       (length str)))
                (setq str ""))
              (setq stack (cons element stack))
              ))))))
    t)


(defun remove-script (str)
  
  (while (string-match "<script" str)
    (progn
      (setq scriptlength (length (substring str
                                            (string-match "<script>" str)
                                            (+ (string-match "</script>" str) (length "</script>")))))
      (setq str (concat (substring str
                                   0
                                   (string-match "<script" str))
                        (substring str
                                   (+ (string-match "</script>" str) 9)
                                   (length str))))))
  str)

(defun is-xml-style (str)
  (if (string-match "/" (substring str
                                   (- (length str) 2)
                                   (1- (length str))))
      t
    nil))

(defun is-empty-element (str)
  ;; Some elements are not having a corresponding end_tag.
  ;; If this method returns true, the element will be skipped during tag-names-check.
  ;; Called only by method "tag-name-check"
  (setq empty_elements '( "<!doctype" "<!--" "<br>" "<br " "<img " "<hr>" "<hr " "<meta " "<link " "<area " "<input "))
  (setq found nil)
  (while (and empty_elements (not found))
    (if (string-match (car empty_elements)
                      str)
        (setq found t)
      (setq empty_elements (cdr empty_elements))))
  found)

(defun compare-tags (start_tag end_tag)
  ;; Called only by method "tag-name-check"
  ;; Returns true if a start_tag and end_tag are corresponding, else nil.
  
  ;; Set the end_tag to include tagname only.
  ;; I.e the removal of the 2 first and the last symbol
  (setq end_tag (substring end_tag
                           2
                           (1- (length end_tag))))
  ;; Match length of start_tag against the "formatted" end_tag
  (if (not (= (length start_tag) (1+ (length end_tag))))
      ;; if the start_tag is shorter than end_tag, no match! 
      (if (< (length start_tag) (1+ (length end_tag)))
          nil
        (progn
          ;; Check the string for space, i.e attributes
          ;; If there is a space, we test the tags against eachother
          (setq temp_tag (substring start_tag
                                    (1+ (length end_tag))
                                    (+ (length end_tag) 2)))

          (if (or (equal temp_tag " ") (equal temp_tag ">"))
              (progn
                (setq start_tag (substring start_tag
                                           1
                                           (1+ (length end_tag))))
                (if (equal start_tag end_tag)
                    t
                   nil))
            ;; else they don't match.
            nil)))
    ;; But if they have the same length, we just have to compare them.
    (progn
      (setq start_tag (substring start_tag
                                 1
                                 (length start_tag)))
      (if (equal start_tag end_tag)
          t
        nil)))
  )
 
