;;; -*- Mode: Emacs-Lisp -*-

;;;; Portable Pathname Abstraction
;;;; Unix Pathnames
;;;; Version 3 (beta)

(define-file-system-type 'unix
  '(parse-namestring . unix/parse-namestring)
  '(origin-namestring . unix/origin-namestring)
  '(directory-namestring . unix/directory-namestring)
  '(file-namestring . unix/file-namestring))

(defun unix/parse-namestring (namestring)
  (let ((len (length namestring)))
    (cond ((zerop len)
           (make-pathname nil nil nil))
          ((= len 1)
           (let ((char (aref namestring 0)))
             (cond ((eq char ?~) (make-pathname 'home nil nil))
                   ((eq char ?/) (make-pathname 'root nil nil))
                   ((eq char ?$)
                    (error "Malformed Unix namestring: %S" namestring))
                   ;++ Should this be a file or a directory pathname?
                   (t (make-pathname nil nil namestring)))))
          (t
           (unix/parse-namestring-origin namestring)))))

(defun unix/parse-namestring-origin (namestring)
  (let ((char (aref namestring 0)))
    (cond ((eq char ?~)
           (unix/parse-homedir-namestring namestring))
          ((eq char ?/)                 ;++ /. for root directory file?
           (unix/parse-namestring-directory namestring 0 'root))
          ((eq char ?$)
           (unix/parse-env-namestring namestring))
          (t
           (unix/parse-namestring-directory namestring
                                            ;; No leading slash.
                                            -1
                                            nil)))))

(defun unix/parse-homedir-namestring (namestring)
  (let ((slash-index (string-match "/" namestring)))
    (if (not slash-index)
        (make-pathname `(home ,(substring namestring 1))
                       nil
                       nil)
      (unix/parse-namestring-directory
       namestring
       slash-index
       (if (= slash-index 1)
           'home
         `(home ,(substring namestring 1 slash-index)))))))

(defun unix/parse-env-namestring (namestring)
  (let ((slash-index (string-match "/" namestring)))
    (if (not slash-index)
        (make-pathname ,(unix/env-origin (substring namestring 1))
                       nil
                       nil)
      (unix/parse-namestring-directory
       namestring
       slash-index
       (unix/env-origin
        (if (= slash-index 1)
            (error "Malformed Unix namestring: %S" namestring)
          (substring namestring 1 slash-index)))))))

(defun unix/env-origin (var)
;++ (if (string= var "HOME")
;++     'home
;++     var)
;++ var
  (intern (downcase var)))

(defun unix/parse-namestring-directory (namestring start origin)
  (let ((directory '())
        (index start))
    (while (setq start (+ index 1)
                 index (string-match "/" namestring start))
      (setq directory
            (cons (substring namestring start index)
                  directory)))
    (setq directory (nreverse directory))
    (make-pathname origin
                   (if (and (not origin)
                            (consp directory)
                            (equal (car directory) "."))
                       (cdr directory)
                     directory)
                   (if (= start (length namestring))
                       nil
                     (unix/parse-namestring-file namestring
                                                 start)))))

(defvar parse-unix-file-types t)

(defun unix/parse-namestring-file (namestring start)
  (let ((version
         (unix/parse-namestring-version namestring start)))
    (let ((end (if version (car version) (length namestring)))
          (version (if version (cdr version) nil)))
      (if (not parse-unix-file-types)
          (make-file (substring namestring start end)
                     nil
                     version)
        (unix/parse-namestring-file-types namestring start end
                                          version)))))

;++ fix dot & dot dot files

(defun unix/parse-namestring-file-types (namestring start end version)
  (let ((parts (split-string (substring namestring start end)
                             "[.]")))
    (if (null parts)
        (make-file nil nil version)
      (make-file (car parts)
                 (cdr parts)
                 version))))

(defun unix/parse-namestring-version (namestring start)
  (let ((index (string-match "\\.~\\([0-9]*\\)~\\'"
;++                          ;; No rx in older Emacsen.
;++                          (rx (: ".~" (submatch (* digit)) "~"
;++                                 string-end))
                             namestring
                             start)))
    (if index
        (cons index (string-to-int (match-string 1 namestring)
                                   10.))
      nil)))

(defun unix/origin-namestring (pathname)
  (let ((origin (pathname-origin pathname)))
    (cond ((not origin) "")
          ((eq origin 'root) "/")
          (t (concat (unix/*origin-namestring origin)
                     (if (or (pathname-file pathname)
                             ;++ nil/false pun
                             (pathname-directory pathname))
                         "/"
                       ""))))))

(defun unix/*origin-namestring (origin)
  (cond ((stringp origin) (concat "$" origin))
        ((and (consp origin)
              (eq (car origin) 'home)
              (consp (cdr origin))
              (stringp (car (cdr origin)))
              (null (cdr (cdr origin))))
         (concat "~" (car (cdr origin))))
        ((eq origin 'home) "~")
        (t (error "Invalid Unix pathname origin: %S" origin))))

(defun unix/directory-namestring (pathname)
  (let ((directory (pathname-directory pathname)))
    (if directory
        (mapconcat (lambda (component)
                     (concat (unix/file->namestring component)
                             "/"))
                   (pathname-directory pathname)
                   "")
      "")))

(defun unix/file-namestring (pathname)
  (let ((file (pathname-file pathname)))
    (if file
        (unix/file->namestring file)
      "")))

(defun unix/file->namestring (file)
  (concat (unix/canonical-string (file-name file))
          (mapconcat (lambda (type)
                       (concat "." (unix/canonical-string type)))
                     (file-types file)
                     "")
          (let ((version (file-version file)))
            (if version
                (concat ".~" (int-to-string version) "~")
              ""))))

(defun unix/canonical-string (component)
  (if (symbolp component)
      (downcase (symbol-name component))
    ;; Otherwise, it will be a string.
    component))
