;;;
;;; Turn elisp data into Ruby structures.
;;;


;;;
;;; The originals deal with passing around objects.  The replacements
;;; are much simpler in that they pass everything literally. Don't
;;; like it, add your own. The server/client infractucture is too limiting for
;;; me to see why it would useful to implement something more complex.
;;;
;;; Unless otherwise noted, a these functions take in an ELISP value
;;; and return a corresponding RUBY expression.
;;;


;;; XXX - is not very smart. Will break.
(defun el4r-to-string (string)
  "Convert STRING to Ruby string expression."
  (concat "%q{" string "}"))

(defun el4r-to-symbol (symbol)
  "Turn SYMBOL into a Ruby symbol expression."
  (concat ":" (prin1-to-string
               (symbol-name symbol))))

;;; XXX - can cause circular loop
(defun el4r-list-to-native-array (list)
  "Turn LIST to a Ruby Array expression."
  (concat
   "["
   (mapconcat (lambda (i)
                (el4r-to-ruby i))
              list
              ", ")
   "]"))

(defun el4r-cons-to-native-array (cons)
  "Turn CONS into a Ruby Array expression."
  (el4r-list-to-native-array
   (list (car cons) (cdr cons))))

;;; XXX - can cause circular loop
(defun el4r-alist-to-native-hash (alist)
  "Turn ALIST into a Ruby Hash expression. This function signal an error if
ALIST is not an alist."
  (concat
   "{"
   (mapconcat (lambda (hk) ;hk should be (H . K)
                (concat (el4r-to-ruby (car hk))
                        " => "
                        (el4r-to-ruby (cdr hk))))
              alist
              ", ")
   "}"))

(defun el4r-simple-cons-p (cons)
  "Unlike `consp' which will return t on '(1 2 3) this will only return t
only if the cdr is a not also list."
  (and (consp cons)
       (not (listp (cdr cons)))))

(defun el4r-simple-alist-p (list)
  "Try to guess is a LIST is an alist that is, if the first element is a
simple cons cell, see `el4r-simple-cons-p'."
  (and (listp list)
       (el4r-simple-cons-p (car list))))

(defun el4r-to-ruby (object &optional use-native)
  "Convert OBJECT into a Ruby expression.  A signal will be raised is the
conversion can not be performed."
  (cond ((not object)
	 "nil")
        ((eq object t)
	 "true")
        ((numberp object)
	 (number-to-string object))
        ((stringp object)
	 (el4r-to-string object))
        ((symbolp object)
         (el4r-to-symbol object))
	;; XXX looks like passing around object IDs again
;;        ((el4r-rubyexpr-p obj)
;;	 (el4r-rubyexpr-string obj))
;;        ((el4r-rubyobj-p obj)
;;         (format "el4r_rubyobj_stock.id2obj(%s)"
;;                 (el4r-rubyobj-id obj)))
;;         ((el4r-proper-list-p obj)
;;          (format "el4r_elobject_new(%d, ELListCell)"
;;                  (el4r-lisp-object-to-id obj)))
;;         ((and (consp obj) (atom (cdr obj)))
;;          (format "el4r_elobject_new(%d, ELConsCell)"
;;                  (el4r-lisp-object-to-id obj)))
;;         ((vectorp obj)
;;          (format "el4r_elobject_new(%d, ELVector)"
	;;                 (el4r-lisp-object-to-id obj)))
        ;; XXX - should likely use special objects to represent
        ((el4r-simple-alist-p object)
         (el4r-alist-to-native-hash object))
        ((el4r-simple-cons-p object)
         (el4r-cons-to-native-array object))
        ((list object)
         (el4r-list-to-native-array object))
        (t
	 (error (format "Can't handle this data-type: [[%S]]" object)))
	))



(defsubst el4r-string-to-rubystr (str)
  (let ((file-read "File.read(conf.temp_file)"))
    (if (or (not el4r-treat-ctrl-codes)
            (string= str file-read))
        (concat "%q" (prin1-to-string (el4r-no-properties obj)))
      (cond ((eq el4r-treat-ctrl-codes 'use-file) ;experimental
             ;; !FIXME! coding-system @ XEmacs
             (with-temp-buffer
               (insert str)
               ;; suppress "wrote file-name" message
               ;; (find-efunctiondescr 'write-region "VISIT is neither")
               (write-region 1 (point-max) el4r-temp-file nil 0))
             file-read)
            (t
	      (concat "%Q"
                      (with-temp-buffer
                        (insert (prin1-to-string (el4r-no-properties str)))
                        (mapcar (lambda (x)
                                  (goto-char 1)
                                  (while (search-forward (car x) nil t)
                                    (replace-match (cdr x))))
                                '(("#" . "\\\\#")
                                  ("\003" . "\\\\cc")
                                  ("\004" . "\\\\cd")
                                  ("\021" . "\\\\cq")
                                  ("\023" . "\\\\cs")
                                  ("\026" . "\\\\cv")
                                  ("\027" . "\\\\cw")
                                  ("\031" . "\\\\cy")
                                  ("\032" . "\\\\cz")
                                  ))
                        (buffer-string)))
      
      )))))


(defun el4r-proper-list-p (expression)
  ;; Tell if a list is proper, id est, that it is `nil' or ends with `nil'.
  (cond ((not expression))
	((consp expression) (not (cdr (last expression))))))

(defun el4r-lisp2ruby (obj)
  (cond ((eq obj nil) "nil")
        ((eq obj t) "true")
        ((numberp obj) (number-to-string obj))
        ((stringp obj) (el4r-string-to-rubystr obj))
        ((el4r-rubyexpr-p obj) (el4r-rubyexpr-string obj))
        ((el4r-rubyobj-p obj)
         (format "el4r_rubyobj_stock.id2obj(%s)"
                 (el4r-rubyobj-id obj)))
        ((el4r-proper-list-p obj)
         (format "el4r_elobject_new(%d, ELListCell)"
                 (el4r-lisp-object-to-id obj)))
        ((and (consp obj) (atom (cdr obj)))
         (format "el4r_elobject_new(%d, ELConsCell)"
                 (el4r-lisp-object-to-id obj)))
        ((vectorp obj)
         (format "el4r_elobject_new(%d, ELVector)"
                 (el4r-lisp-object-to-id obj)))
        (t
         (format "el4r_elobject_new(%d)"
                 (el4r-lisp-object-to-id obj)))
        ))

(defun el4r-rubyexpr-p (rubyexpr)
  (and (listp rubyexpr) (eq (car rubyexpr) 'el4r-rubyexpr)))

(defun el4r-rubyexpr-string (rubyexpr)
  (cdr rubyexpr))

(defun el4r-rubyexpr-quote (string)
  (cons 'el4r-rubyexpr string))


(provide 'el4r-to-ruby)