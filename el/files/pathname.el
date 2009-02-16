;;; -*- Mode: Emacs-Lisp -*-

;;;; Portable Pathname Abstraction

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; /Pathnames/ are platform-independent representations of locations
;;; of objects on file systems.  A pathname consists of three
;;; components, each of which is optional:
;;;
;;;   - The /origin/ is the place from which the path begins.  It might
;;;     be the Unix root directory, a user's home directory, a DOS
;;;     device/drive, an Apollo logical name, a Unix environment
;;;     variable, a VMS host, &c.  Origins can also have expansions
;;;     defined in Emacs.  A pathname with a null origin is a relative
;;;     pathname.
;;;
;;;   - The /directory/ is a list of directory names from the origin
;;;     leading up to the file.  Each directory name is as string or a
;;;     symbol.
;;;
;;;   - The /file/ is the name of the file itself along with the type
;;;     and version.  The name is a string or a symbol.  There may be
;;;     zero or more types, each of which is also a string or a symbol.
;;;     The version is either a non-negative integer or the symbol
;;;     `newest'.

;;;; Utilities

;;; This section must come first so that ENFORCE is defined before
;;; compiling the functions in the file that use it.

(eval-when-compile
  (defmacro enforce (typep object)
    (let ((var (make-symbol "OBJECT")))
      `(let ((,var ,object))
         (if (not (,typep ,var))
             (signal 'wrong-type-argument (list ',typep ,var)))))))

(defun string-or-symbol-p (object)
  "Return t if OBJECT is a string or a symbol and nil if not."
  (or (stringp object)
      (symbolp object)))

(if (not (fboundp 'warn))
    (defalias 'warn 'message))

;;;; Pathnames

(defun make-pathname (origin directory file)
  "Return a pathname with the given components."
  (if directory (enforce pathname-directory-p directory))
  (if file (enforce filep file))
  (list origin directory file))

(defun pathnamep (object)
  "Return t if OBJECT is a valid pathname and nil if otherwise."
  (and (consp object)
       (consp (cdr object))
       (pathname-directory-p (car (cdr object)))
       (consp (cdr (cdr object)))
       (filep (car (cdr (cdr object))))
       (null (cdr (cdr (cdr object))))))

(defun pathname-directory-p (object)
  "Return t if OBJECT is a valid pathname directory and nil if not."
  (catch 'lose
    (while (consp object)
      (if (pathname-directory-component-p (car object))
          (setq object (cdr object))
          (throw 'lose nil)))
    (null object)))

(defun pathname-directory-component-p (object)
  "Return t if OBJECT is a valid directory component and nil if not."
;++ (string-or-symbol-p object)
  (filep object))

(defun pathname-origin (pathname)
  "Return the origin component of PATHNAME."
  (car (->pathname pathname)))

(defun pathname-directory (pathname)
  "Return the directory component of PATHNAME."
  (car (cdr (->pathname pathname))))

(defun pathname-file (pathname)
  "Return the file component of PATHNAME."
  (car (cdr (cdr (->pathname pathname)))))

(defun ->pathname (object &optional fs-type)
  "Coerce OBJECT to a pathname.
If OBJECT is a symbol, return a pathname with a relative origin, an
  empty directory, and a file whose name is the symbol.
If OBJECT is a string, parse it according to the optional file system
  type FS-TYPE, which defaults to the local file system type.
If OBJECT is a pathname, return it.
In any other case, signal an error."
  (cond ((symbolp   object) (make-pathname nil '() object))
        ((stringp   object) (parse-namestring object fs-type))
        ((pathnamep object) object)
        (t (error "%S cannot be coerced to a pathname."
                  object))))

;;;; Files

(defun make-file (name type &optional version)
  "Make a pathname file with the given components.
NAME is the file's name, a string or a symbol.
TYPE is the file's type or types; it may be a string, a symbol, or a
  list of strings and symbols.
VERSION is a non-negative integer representing the file's version, or
  the symbol `newest' representing the most recent version of the
  file."
  (enforce string-or-symbol-p name)
  (enforce file-version-p version)
  (let ((type (make-file/type type)))
    (if (not (or type version))
        name
        (append (list name) type (or version '())))))

(defun make-file/type (type)
  (cond ((not type) '())
        ((string-or-symbol-p type) (list type))
        ((let ((type* type))
           (while (consp type*)
             (setq type* (if (string-or-symbol-p (car type*))
                             (cdr type*)
                             'lose)))   ; Neither a cons nor nil.
           (null type*))
         type)
        (t (error "Invalid file type specifier: %S" type))))

(defun filep (object)
  "Return t if OBJECT is a valid pathname file and nil if not."
  (or (string-or-symbol-p object)
      (and (consp object)
           (catch 'lose
             (while (consp object)
               (if (string-or-symbol-p (car object))
                   (setq object (cdr object))
                   (throw 'lose nil)))
             (file-version-p object)))))

(defun file-version-p (object)
  "Return t if OBJECT is a valid file version and nil if not."
  (or (null object)                     ; No version.
      (eq object 'newest)
      (natnump object)))

(defun file-name (file)
  "Return the name component of FILE."
  (enforce filep file)
  (if (string-or-symbol-p file)
      file
      (car file)))

(defun file-type (file)
  "Return the type of FILE.
Return the last type if there is more than one.
Return nil if the file has no type."
  (enforce filep file)
  (if (or (string-or-symbol-p file)
          (not (consp (cdr file))))
      nil
    (let (tail)
      (while (progn (setq tail (cdr file))
                    (consp tail))
        (setq file tail)))
    (car file)))

(defun file-types (file)
  "Return a list of all FILE's types, in order."
  (enforce filep file)
  (if (or (string-or-symbol-p file)
          (not (consp (cdr file))))
      nil
    (let ((types '()))
      (while (progn (setq file (cdr file))
                    (consp file))
        (setq types (cons (car file) types)))
      (nreverse types))))

(defun file-version (file)
  "Return the version of FILE, or nil if it has none."
  (enforce filep file)
  (if (string-or-symbol-p file)
      nil
    (while (consp file)
      (setq file (cdr file)))
    file))

;;;; Pathname Component Substitution & Merging

(defun pathname-with-origin (pathname origin)
  "Return a pathname like PATHNAME with an origin of ORIGIN."
  (setq pathname (->pathname pathname))
  (make-pathname origin
                 (pathname-directory pathname)
                 (pathname-file pathname)))

(defun pathname-with-directory (pathname directory)
  "Return a pathname like PATHNAME with a directory of DIRECTORY."
  (setq pathname (->pathname pathname))
  (make-pathname (pathname-origin pathname)
                 directory
                 (pathname-file pathname)))

(defun pathname-with-file (pathname file)
  "Return a pathname like PATHNAME with a file of FILE."
  (setq pathname (->pathname pathname))
  (make-pathname (pathname-origin pathname)
                 (pathname-directory pathname)
                 file))

(defun pathname-default (pathname origin directory file)
  "Return a pathname like PATHNAME.
Any null components of PATHNAME are filled with the supplied
  arguments."
  (setq pathname (->pathname pathname))
  (make-pathname (or (pathname-origin pathname) origin)
                 (or (pathname-directory pathname) directory)
                 (or (pathname-file pathname) file)))

(defun merge-pathnames (pathname defaults-pathname)
  "Return a pathname by merging PATHNAME with DEFAULTS-PATHNAME."
  (setq pathname (->pathname pathname))
  (setq defaults-pathname (->pathname defaults-pathname))
  (let ((origin (pathname-origin pathname))
        (origin-default (pathname-origin defaults-pathname))
        (directory (pathname-directory pathname))
        (directory-default (pathname-directory defaults-pathname))
        (file (merge-files (pathname-file pathname)
                           (pathname-file defaults-pathname))))
    (if origin
        (make-pathname origin directory file)
        (make-pathname origin-default
                       (cond ((not directory) directory-default)
                             ((not directory-default) directory)
                             (t (append directory-default directory)))
                       file))))

(defun merge-files (file defaults-file)
  "Return a file by merging FILE with DEFAULTS-FILE.

This is currently unimplemented and will simply return whichever one is
 non-nil, preferring FILE."
  (warn "Unimplemented: %S" `(merge-files ,file ,defaults-file))
  (or file defaults-file))

(defun enough-pathname (pathname relative)
  "Return a pathname whose merging with RELATIVE produces PATHNAME.

This is currently unimplemented and will simply return PATHNAME."
  (warn "Unimplemented: %S" `(enough-pathname ',pathname ',relative))
  pathname)

;;;; Directory Pathnames

(defun directory-pathname-p (pathname)
  "Returns t if PATHNAME has directory components but no file,
  and nil if otherwise."
  (setq pathname (->pathname pathname))
  (and (pathname-directory pathname)    ;++ nil/false pun
       (not (pathname-file pathname))))

(defun pathname-as-directory (pathname)
  "Return a pathname like PATHNAME, representing a directory.
If PATHNAME has a file component, it is added to the end of the list of
  directory components, and the resultant pathname has no file.
Otherwise, return PATHNAME."
  (setq pathname (->pathname pathname))
  (let ((file (pathname-file pathname)))
    (if file
        (make-pathname (pathname-origin pathname)
                       (let ((directory (pathname-directory pathname)))
                         (if directory
                             (append directory (list file))
                             (list file)))
                       nil)
        pathname)))

(defun pathname-container (pathname)
  "Return a pathname of the directory that contains PATHNAME."
  (setq pathname (->pathname pathname))
  (catch 'exit
    (while t
      (let ((origin (pathname-origin pathname))
            (directory (pathname-directory pathname))
            (file (pathname-file pathname)))
        (cond (file
               (throw 'exit (make-pathname origin directory nil)))
              ((and directory (not (null directory)))
               (throw 'exit (make-pathname origin (butlast directory)
                                           nil)))
              (t
               (let ((expansion (expand-pathname pathname)))
                 (if (equal expansion pathname)
                     (error "Unable to find pathname's container: %S"
                            pathname)
                     (setq pathname expansion)))))))))

;;;; Pathname Expansion

(defun expand-pathname (pathname)
  "Return a pathname like PATHNAME but with the origin expanded.

This is currently unimplemented and will simply return PATHNAME."
  (warn "Unimplemented: %S" `(expand-pathname ',pathname))
  pathname)

;;;; Namestrings

(defun parse-namestring (namestring &optional fs-type)
  "Parse NAMESTRING and return a pathname representing it.
Use FS-TYPE's namestring parser to parse NAMESTRING.
If FS-TYPE is not supplied, it defaults to the local file system."
  (funcall (file-system-type-parameter
            (or fs-type (local-file-system-type))
            'parse-namestring)
           namestring))

(defun ->namestring (object &optional fs-type)
  "Coerce OBJECT into a namestring.
If OBJECT is a string, canonicalize it according to FS-TYPE.
If OBJECT is a pathname, convert it according to FS-TYPE.
Otherwise, signal an error.
If FS-TYPE is not supplied, it defaults to the local file system type."
  (setq fs-type (or fs-type (local-file-system-type)))
  ;++ What if it's a symbol?  Use (MAKE-PATHNAME NIL NIL object)?
  (cond ((stringp object)
         (let ((probe (file-system-type-option
                       fs-type
                       'canonicalize-namestring)))
           (if probe
               (funcall (cdr probe) object)
               (pathname->namestring (->pathname object) fs-type))))
        ((pathnamep object)
         (pathname->namestring object fs-type))
        (t (error "Unable to coerce to a namestring: %S" object))))

(defun pathname->namestring (pathname fs-type)
  (let ((probe (file-system-type-option fs-type '->namestring)))
    (if probe
        (funcall (cdr probe) pathname)
        (concat (origin-namestring pathname)
                (directory-namestring pathname)
                (file-namestring pathname)))))

(defun origin-namestring (pathname &optional fs-type)
  "Return a string for PATHNAME's origin according to FS-TYPE.
If FS-TYPE is not supplied, it defaults to the local file system type."
  (setq pathname (->pathname pathname))
  (funcall (file-system-type-parameter
            (or fs-type (local-file-system-type))
            'origin-namestring)
           pathname))

(defun directory-namestring (pathname &optional fs-type)
  "Return a string for PATHNAME's directory according to FS-TYPE.
If FS-TYPE is not supplied, it defaults to the local file system type."
  (setq pathname (->pathname pathname))
  (funcall (file-system-type-parameter
            (or fs-type (local-file-system-type))
            'directory-namestring)
           pathname))

(defun file-namestring (pathname &optional fs-type)
  "Return a string for PATHNAME's file according to FS-TYPE.
If FS-TYPE is not supplied, it defaults to the local file system type."
  (setq pathname (->pathname pathname))
  (funcall (file-system-type-parameter
            (or fs-type (local-file-system-type))
            'file-namestring)
           pathname))

(defun enough-namestring (pathname relative &optional fs-type)
  "Return a string naming PATHNAME relative to RELATIVE,
  according to FS-TYPE.
If FS-TYPE is not supplied, it defaults to the local file system type."
  (setq pathname (->pathname pathname))
  (setq relative (->pathname relative))
  (or fs-type (setq fs-type (local-file-system-type)))
  (let ((probe (file-system-type-option fs-type 'enough-namestring)))
    (if probe
        (funcall (cdr probe) pathname relative)
        (->namestring (enough-pathname pathname relative)
                      fs-type))))

;;;; File System Types

(defvar file-system-types '()
  "Alist mapping file system type names to file system type descriptor
  alists.")

(defun find-file-system-type (fs-type)
  (or (assq fs-type file-system-types)
      (error "Unrecognized file system type: %S" fs-type)))

(defun file-system-type-option (fs-type indicator)
  (assq indicator (cdr (find-file-system-type fs-type))))

(defun file-system-type-parameter (fs-type indicator)
  (let ((probe (file-system-type-option fs-type indicator)))
    (if probe
        (cdr probe)
        (error "File system type %S has absent parameter %S."
               fs-type
               indicator))))

(defun define-file-system-type (name &rest alist)
  "Define a file system type named NAME with the given alist of
  parameters."
  (let ((probe (assq name file-system-types)))
    (cond (probe
           (setcdr probe alist))
          (t
           (setq file-system-types
                 (cons (cons name alist)
                       file-system-types))))))

(defun define-file-system-type-option (fs-type indicator value)
  (let* ((alist (find-file-system-type fs-type))
         (probe (assq indicator (cdr alist))))
    (if probe
        (setcdr probe value)
        (setcdr alist (cons (cons indicator value) (cdr alist))))))

(defvar local-file-system-type nil
  "Cached type of the local machine's file system.")

(defun local-file-system-type ()
  "Return the type of the local machine's file system."
  (or local-file-system-type
      (let ((type (determine-local-file-system-type)))
        (setq local-file-system-type type)
        type)))

(defun determine-local-file-system-type ()
  (cond ((eq system-type 'darwin)                   'mac-unix)
        ((eq system-type 'macos)                    'mac-classic)
        ((memq system-type '(ms-dos windows-nt))    'dos)
        ((memq system-type '(vax-vms axp-vms))      'vms)
        (t                                          'unix)))
