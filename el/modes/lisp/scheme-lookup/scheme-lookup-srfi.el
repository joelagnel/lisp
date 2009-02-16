;;;;;; Scheme-lookup: lookup documentation for Scheme symbols
;;;;;; Version $Version$

;;; This code is written by Trent W. Buck <trentbuck@gmail.com>
;;; (except where explicitly noted) and placed in the Public Domain.
;;; All warranties are disclaimed.

(require 'scheme-lookup)

(defvar scheme-lookup-srfi-names '())

(defun scheme-lookup-srfi (symbol-name n &optional anchor)
  (browse-url
   (cond
     ((stringp anchor)
      (format "http://srfi.schemers.org/srfi-%d/srfi-%d.html#%s" n n anchor))
     (anchor
      (format "http://srfi.schemers.org/srfi-%d/srfi-%d.html#%s" n n symbol-name))
     (t
      (format "http://srfi.schemers.org/srfi-%d/srfi-%d.html" n n)))))

(put 'scheme-lookup-srfi
     'scheme-lookup-pretty-name
     (lambda (symbol-name n &optional anchor)
       (format "SRFI %d: %s" n (cdr (assoc n scheme-lookup-srfi-names)))))

(mapc (lambda (struct)
        (let ((n    (car struct))
              (name (cadr struct))
              (syms (caddr struct)))
          (when (stringp name)
            (add-to-list 'scheme-lookup-srfi-names (cons n name))
            (mapc (lambda (x)
                    (scheme-lookup-add-reference
                     (if (stringp x)
                         (list 'scheme-lookup-srfi x n)
                         (list 'scheme-lookup-srfi (car x) n (cadr x)))))
                  syms))))
      '((0 "Feature-based conditional expansion construct"
         ("cond-expand"))
        (1 "List Library"
         (("cons"                    t)
          ("list"                    t)
          ("xcons"                   t)
          ("cons*"                   t)
          ("make-list"               t)
          ("list-tabulate"           t)
          ("list-copy"               t)
          ("circular-list"           t)
          ("iota"                    t)
          ("pair?"                   "pair-p")
          ("null?"                   "null-p")
          ("proper-list?"            "proper-list-p")
          ("circular-list?"          "circular-list-p")
          ("dotted-list?"            "dotted-list-p")
          ("not-pair?"               "not-pair-p")
          ("null-list?"              "null-list-p")
          ("list="                   t)
          ("car"                     t)
          ("cdr"                     t)
          ("caar"                    "cdr")
          ("cadr"                    "cdr")
          ("cdar"                    "cdr")
          ("cddr"                    "cdr")
          ("caaar"                   "cdr")
          ("caadr"                   "cdr")
          ("cadar"                   "cdr")
          ("caddr"                   "cdr")
          ("cdaar"                   "cdr")
          ("cdadr"                   "cdr")
          ("cddar"                   "cdr")
          ("cdddr"                   "cdr")
          ("caaaar"                  "cdr")
          ("caaadr"                  "cdr")
          ("caadar"                  "cdr")
          ("caaddr"                  "cdr")
          ("cadaar"                  "cdr")
          ("cadadr"                  t)
          ("caddar"                  "cdr")
          ("cadddr"                  "cdr")
          ("cdaaar"                  "cdr")
          ("cdaadr"                  "cdr")
          ("cdadar"                  "cdr")
          ("cdaddr"                  "cdr")
          ("cddaar"                  "cdr")
          ("cddadr"                  t)
          ("cdddar"                  "cdr")
          ("cddddr"                  "cdr")
          ("list-ref"                t)
          ("first"                   t)
          ("second"                  t)
          ("third"                   t)
          ("fourth"                  t)
          ("fifth"                   t)
          ("sixth"                   t)
          ("seventh"                 t)
          ("eighth"                  t)
          ("ninth"                   t)
          ("tenth"                   t)
          ("car+cdr"                 t)
          ("take"                    t)
          ("drop"                    t)
          ("take-right"              t)
          ("drop-right"              t)
          ("take!"                   t)
          ("drop-right!"             t)
          ("split-at"                t)
          ("split-at!"               t)
          ("last"                    t)
          ("last-pair"               t)
          ("length"                  t)
          ("length+"                 t)
          ("append"                  t)
          ("concatenate"             t)
          ("reverse"                 t)
          ("append!"                 t)
          ("concatenate!"            t)
          ("reverse!"                t)
          ("append-reverse"          t)
          ("append-reverse!"         t)
          ("zip"                     t)
          ("unzip1"                  t)
          ("unzip2"                  t)
          ("unzip3"                  t)
          ("unzip4"                  t)
          ("unzip5"                  t)
          ("count"                   t)
          ("map"                     t)
          ("for-each"                t)
          ("fold"                    t)
          ("unfold"                  t)
          ("pair-fold"               t)
          ("reduce"                  t)
          ("fold-right"              t)
          ("unfold-right"            t)
          ("pair-fold-right"         t)
          ("reduce-right"            t)
          ("append-map"              t)
          ("append-map!"             t)
          ("map!"                    t)
          ("pair-for-each"           t)
          ("filter-map"              t)
          ("map-in-order"            t)
          ("filter"                  t)
          ("partition"               t)
          ("remove"                  t)
          ("filter!"                 t)
          ("partition!"              t)
          ("remove!"                 t)
          ("member"                  t)
          ("memq"                    t)
          ("memv"                    t)
          ("find"                    t)
          ("find-tail"               t)
          ("any"                     t)
          ("every"                   t)
          ("list-index"              t)
          ("take-while"              t)
          ("drop-while"              t)
          ("take-while!"             t)
          ("span"                    t)
          ("break"                   t)
          ("span!"                   t)
          ("break!"                  t)
          ("delete"                  t)
          ("delete-duplicates"       t)
          ("delete!"                 t)
          ("delete-duplicates!"      t)
          ("assoc"                   t)
          ("assq"                    t)
          ("assv"                    t)
          ("alist-cons"              t)
          ("alist-copy"              t)
          ("alist-delete"            t)
          ("alist-delete!"           t)
          ("lset<="                  "lset&lt;=")
          ("lset="                   t)
          ("lset-adjoin"             t)
          ("lset-union"              t)
          ("lset-union!"             t)
          ("lset-intersection"       t)
          ("lset-intersection!"      t)
          ("lset-difference"         t)
          ("lset-difference!"        t)
          ("lset-xor"                t)
          ("lset-xor!"               t)
          ("lset-diff+intersection"  t)
          ("lset-diff+intersection!" t)
          ("set-car!"                t)
          ("set-cdr!"                t)))
        (2 "AND-LET*: an AND with local bindings, a guarded LET* special form"
         ("and-let*"))
        (3 :withdrawn)
        (4 "Homogeneous numeric vector datatypes"
         ("s8vector?"
          "make-s8vector"
          "s8vector"
          "s8vector-length"
          "s8vector-ref"
          "s8vector-set!"
          "s8vector->list"
          "list->s8vector"))
        (5 "A compatible let form with signatures and rest arguments"
         ("let"))
        (6 "Basic String Ports"
         ("open-input-string"
          "open-output-string"
          "get-output-string"))
        (7 "Feature-based program configuration language"
         ("requires"
          "files"
          "code"
          "feature-cond"))
        (8 "receive: Binding to multiple values"
         ("receive"))
        (9 "Defining Record Types"
         ("define-record-type"))
        (10 "Sharp-Comma External Form")
        (11 "Syntax for receiving multiple values"
         ("let-values"
          "let-values*"))
        (12 :withdrawn)
        (13 "String Library"
         (("string?"                           "string-p")
          ("string-null?"                      "string-null-p")
          ("string-every"                      t)
          ("string-any"                        t)
          ("make-string"                       t)
          ("string"                            t)
          ("string-tabulate"                   t)
          ("string->list"                      "string2list")
          ("list->string"                      "list2string")
          ("reverse-list->string"              "reverse-list2string")
          ("string-join"                       t)
          ("string-length"                     t)
          ("string-ref"                        t)
          ("string-copy"                       t)
          ("substring/shared"                  t)
          ("string-copy!"                      t)
          ("string-take"                       t)
          ("string-take-right"                 t)
          ("string-drop"                       t)
          ("string-drop-right"                 t)
          ("string-pad"                        t)
          ("string-pad-right"                  t)
          ("string-trim"                       t)
          ("string-trim-right"                 t)
          ("string-trim-both"                  t)
          ("string-set!"                       t)
          ("string-fill!"                      t)
          ("string-compare"                    t)
          ("string-compare-ci"                 t)
          ("string<>"                          "string&lt;&gt;")
          ("string="                           t)
          ("string<"                           "string&lt;")
          ("string>"                           "string&gt;")
          ("string<="                          "string&lt;=")
          ("string>="                          "string&gt;=")
          ("string-ci<>"                       "string-ci&lt;&gt;")
          ("string-ci="                        t)
          ("string-ci<"                        "string-ci&lt;")
          ("string-ci>"                        "string-ci&gt;")
          ("string-ci<="                       "string-ci&lt;=")
          ("string-ci>="                       "string-ci&gt;=")
          ("string-hash"                       t)
          ("string-hash-ci"                    t)
          ("string-prefix-length"              t)
          ("string-suffix-length"              t)
          ("string-prefix-length-ci"           t)
          ("string-suffix-length-ci"           t)
          ("string-prefix?"                    "string-prefix-p")
          ("string-suffix?"                    "string-suffix-p")
          ("string-prefix-ci?"                 "string-prefix-ci-p")
          ("string-suffix-ci?"                 "string-suffix-ci-p")
          ("string-index"                      t)
          ("string-index-right"                t)
          ("string-skip"                       t)
          ("string-skip-right"                 t)
          ("string-count"                      t)
          ("string-contains"                   t)
          ("string-contains-ci"                t)
          ("string-titlecase"                  t)
          ("string-upcase"                     t)
          ("string-downcase"                   t)
          ("string-titlecase!"                 t)
          ("string-upcase!"                    t)
          ("string-downcase!"                  t)
          ("string-reverse"                    t)
          ("string-reverse!"                   t)
          ("string-append"                     t)
          ("string-concatenate"                t)
          ("string-concatenate/shared"         t)
          ("string-append/shared"              t)
          ("string-concatenate-reverse"        t)
          ("string-concatenate-reverse/shared" t)
          ("string-map"                        t)
          ("string-map!"                       t)
          ("string-fold"                       t)
          ("string-fold-right"                 t)
          ("string-unfold"                     t)
          ("string-unfold-right"               t)
          ("string-for-each"                   t)
          ("string-for-each-index"             t)
          ("xsubstring"                        t)
          ("string-xcopy!"                     t)
          ("string-replace"                    t)
          ("string-tokenize"                   t)
          ("string-filter"                     t)
          ("string-delete"                     t)
          ("string-parse-start+end"            t)
          ("string-parse-final-start+end"      t)
          ("let-string-start+end"              t)
          ("check-substring-spec"              t)
          ("substring-spec-ok?"                "substring-spec-ok-p")
          ("make-kmp-restart-vector"           t)
          ("kmp-step"                          t)
          ("string-kmp-partial-search"         t)))
        (14 "Character-Set Library"
         (("char-set?"              "char-set-p")
          ("char-set="              t)
          ("char-set<="             t)
          ("char-set-hash"          t)
          ("char-set-cursor"        t)
          ("char-set-ref"           t)
          ("char-set-cursor-next"   t)
          ("end-of-char-set?"       "end-of-char-set-p")
          ("char-set-fold"          t)
          ("char-set-unfold"        t)
          ("char-set-unfold!"       t)
          ("char-set-for-each"      t)
          ("char-set-map"           t)
          ("char-set-copy"          t)
          ("char-set"               t)
          ("list->char-set"         t)
          ("string->char-set"       t)
          ("list->char-set!"        t)
          ("string->char-set!"      t)
          ("char-set-filter"        t)
          ("ucs-range->char-set"    t)
          ("char-set-filter!"       t)
          ("ucs-range->char-set!"   t)
          ("->char-set"             t)
          ("char-set->list"         t)
          ("char-set->string"       t)
          ("char-set-size"          t)
          ("char-set-count"         t)
          ("char-set-contains?"     t)
          ("char-set-every"         t)
          ("char-set-any"           t)
          ("char-set-adjoin"        t)
          ("char-set-delete"        t)
          ("char-set-adjoin!"       t)
          ("char-set-delete!"       t)
          ("char-set-complement"    t)
          ("char-set-union"         t)
          ("char-set-intersection"  t)
          ("char-set-complement!"   t)
          ("char-set-union!"        t)
          ("char-set-intersection!" t)
          ("char-set-difference"    t)
          ("char-set-xor"           t)))
        (15 :withdrawn)
        (16 "Syntax for procedures of variable arity"
         ("case-lambda"))
        (17 "Generalized set!"
         ("set!"))
        (18 "Multithreading support"
         ("current-thread"
          "make-thread"
          "thread-name"
          "thread-specific"
          "thread-specific-set!"
          "thread-start!"
          "thread-yield!"
          "thread-sleep!"
          "thread-terminate!"
          "thread-join!"
          "mutex?"
          "make-mutex"
          "mutex-name"
          "mutex-specific"
          "mutex-specific-set!"
          "mutex-state"
          "mutex-lock!"
          "mutex-unlock!"
          "condition-variable?"
          "make-condition-variable"
          "condition-variable-name"
          "condition-variable-specific"
          "condition-variable-specific-set!"
          "condition-variable-signal!"
          "condition-variable-broadcast!"
          "current-time"
          "time?"
          "time->seconds"
          "seconds->time"
          "current-exception-handler"
          "with-exception-handler"
          "raise"
          "join-timeout-exception?"
          "abandoned-mutex-exception?"
          "terminated-thread-exception?"
          "uncaught-exception?"
          "uncaught-exception-reason"))
        (19 "Time Data Types and Procedures"
         ("time-duration"
          "time-monotonic"
          "time-process"
          "time-tai"
          "time-thread"
          "time-utc"
          "current-date"
          "current-julian-day"
          "current-modified-julian-day"
          "current-time"
          "time-resolution"
          "make-time"
          "time?"
          "time-type"
          "time-nanosecond"
          "time-second"
          "set-time-type!"
          "set-time-nanosecond!"
          "set-time-second!"
          "copy-time"
          "time<=?"
          "time<?"
          "time=?"
          "time>=?"
          "time>?"
          "time-difference"
          "time-difference!"
          "add-duration"
          "add-duration!"
          "subtract-duration"
          "subtract-duration!"
          "make-date"
          "date?"
          "date-nanosecond"
          "date-second"
          "date-minute"
          "date-hour"
          "date-day"
          "date-month"
          "date-year"
          "date-zone-offset"
          "date-year-day"
          "date-week-day"
          "date-week-number"
          "date->julian-day"
          "date->modified-julian-day"
          "date->time-monotonic"
          "date->time-tai"
          "date->time-utc"
          "julian-day->date"
          "julian-day->time-monotonic"
          "julian-day->time-tai"
          "julian-day->time-utc"
          "modified-julian-day->date"
          "modified-julian-day->time-monotonic"
          "modified-julian-day->time-tai"
          "modified-julian-day->time-utc"
          "time-monotonic->date"
          "time-monotonic->julian-day"
          "time-monotonic->modified-julian-day"
          "time-monotonic->time-tai"
          "time-monotonic->time-tai!"
          "time-monotonic->time-utc"
          "time-monotonic->time-utc!"
          "time-tai->date"
          "time-tai->julian-day"
          "time-tai->modified-julian-day"
          "time-tai->time-monotonic"
          "time-tai->time-monotonic!"
          "time-tai->time-utc"
          "time-tai->time-utc!"
          "time-utc->date"
          "time-utc->julian-day"
          "time-utc->modified-julian-day"
          "time-utc->time-monotonic"
          "time-utc->time-monotonic!"
          "time-utc->time-tai"
          "time-utc->time-tai!"
          "date->string"
          "string->date"))
        (20 :withdrawn)
        (21 "Real-time multithreading support"
         ("current-thread"
          "thread?"
          "make-thread"
          "thread-name"
          "thread-specific"
          "thread-specific-set!"
          "thread-base-priority"
          "thread-base-priority-set!"
          "thread-priority-boost"
          "thread-priority-boost-set!"
          "thread-quantum"
          "thread-quantum-set!"
          "thread-start!"
          "thread-yield!"
          "thread-sleep!"
          "thread-terminate!"
          "thread-join!"
          "mutex?"
          "make-mutex"
          "mutex-name"
          "mutex-specific"
          "mutex-specific-set!"
          "mutex-state"
          "mutex-lock!"
          "mutex-unlock!"
          "condition-variable?"
          "make-condition-variable"
          "condition-variable-name"
          "condition-variable-specific"
          "condition-variable-specific-set!"
          "condition-variable-signal!"
          "condition-variable-broadcast!"
          "current-time"
          "time?"
          "time->seconds"
          "seconds->time"
          "current-exception-handler"
          "with-exception-handler"
          "raise"
          "join-timeout-exception?"
          "abandoned-mutex-exception?"
          "terminated-thread-exception?"
          "uncaught-exception?"
          "uncaught-exception-reason"))
        (22 "Running Scheme Scripts on Unix")
        (23 "Error reporting mechanism"
         ("error"))
        (24 :withdrawn)
        (25 "Multi-dimensional Array Primitives"
         ("array?"
          "make-array"
          "shape"
          "array"
          "array-rank"
          "array-start"
          "array-end"
          "array-ref"
          "array-set!"
          "share-array"))
        (26 "Notation for Specializing Parameters without Currying"
         ("cut"))
        (27 "Sources of Random Bits"
         ("random-integer"
          "random-real"
          "default-random-source"
          "make-random-source"
          "random-source?"
          "random-source-state-ref"
          "random-source-state-set!"
          "random-source-randomize!"
          "random-source-pseudo-randomize!"
          "random-source-make-integers"
          "random-source-make-reals"
          "random-source-make-reals"))
        (28 "Basic Format Strings"
         ("format"))
        (29 "Localization"
         ("current-language"
          "current-language"
          "current-country"
          "current-country"
          "current-locale-details"
          "current-locale-details"
          "declare-bundle!"
          "store-bundle"
          "load-bundle!"
          "localized-template"))
        (30 "Nested Multi-line Comments")
        (31 "A special form for recursive evaluation"
         ("rec"))
        (32 :withdrawn)
        (33 :withdrawn)
        (34 "Exception Handling for Programs"
         ("with-exception-handler"
          "guard"
          "raise"))
        (35 "Conditions"
         ("make-condition-type"
          "condition-type?"
          "make-condition"
          "condition?"
          "condition-has-type?"
          "condition-ref"
          "make-compound-condition"
          "extract-condition"
          "define-condition-type"
          "condition"
          "&condition"
          "&message"
          "&serious"
          "&error"))
        (36 "I/O Conditions"
         ("&i/o-error"
          "&i/o-port-error"
          "&i/o-read-error"
          "&i/o-write-error"
          "&i/o-closed-error"
          "&i/o-filename-error"
          "&i/o-malformed-filename-error"
          "&i/o-file-protection-error"
          "&i/o-file-is-read-only-error"
          "&i/o-file-already-exists-error"
          "&i/o-no-such-file-error"
          "&read-error"))
        (37 "args-fold: a program argument processor"
         ("option-processor"
          "operand-processor"
          "option"
          "option-names"
          "option-required-arg?"
          "option-optional-arg?"
          "option-processor"
          "args-fold"))
        (38 "External Representation for Data With Shared Structure"
         ("write-with-shared-structure"
          "read-with-shared-structure"))
        (39 "Parameter objects"
         ("make-parameter"
          "parameterize"))
        (40 "A Library of Streams"
         ("stream-null"
          "stream-cons"
          "stream?"
          "stream-null?"
          "stream-pair?"
          "stream-car"
          "stream-cdr"
          "stream-delay"
          "stream"
          "stream-unfoldn"
          "stream-map"
          "stream-for-each"
          "stream-filter"))
        (41 :unallocated)
        (42 "Eager Comprehensions")
        (43 "Vector Library")
        (44 "Collections")
        (45 "Primitives for expressing iterative lazy algorithms"
         ("delay"
          "lazy"
          "force"
          "eager"))
        (46 "Basic Syntax-rules Extensions")
        (47 :superseded)
        (48 "Intermediate Format Strings"
         ("format"))
        (49 "Indentation-sensitive syntax")
        (50 :withdrawn)
        (51 :stupid)
        (52 :withdrawn)
        (53 :withdrawn)
        (54 :stupid)
        (55 "require-extension"
         ("require-extension"))
        (56 :withdrawn)
        (57 "Records"
         ("define-record-type"
          "define-record-scheme"))
        (58 "Array Notation")
        (59 "Vicinity"
         ("program-vicinity"
          "library-vicinity"
          "implementation-vicinity"
          "user-vicinity"
          "home-vicinity"
          "in-vicinity"
          "vicinity"
          "filename"
          "sub-vicinity"
          "vicinity"
          "name"
          "make-vicinity"
          "dirpath"
          "pathname->vicinity"
          "path"
          "vicinity:suffix?"
          "chr"))
        (60 "Integers as Bits"
         (("logand"              "IDX487")
          ("bitwise-and"         "IDX488")
          ("logior"              "IDX489")
          ("bitwise-ior"         "IDX490")
          ("logxor"              "IDX491")
          ("bitwise-xor"         "IDX492")
          ("lognot"              "IDX493")
          ("bitwise-not"         "IDX494")
          ("bitwise-if"          "IDX495")
          ("bitwise-merge"       "IDX496")
          ("logtest"             "IDX497")
          ("any-bits-set?"       "IDX498")
          ("logcount"            "IDX499")
          ("bit-count"           "IDX500")
          ("integer-length"      "IDX501")
          ("log2-binary-factors" "IDX502")
          ("first-set-bit"       "IDX503")
          ("logbit?"             "IDX504")
          ("bit-set?"            "IDX505")
          ("copy-bit"            "IDX506")
          ("bit-field"           "IDX507")
          ("copy-bit-field"      "IDX508")
          ("ash"                 "IDX509")
          ("arithmetic-shift"    "IDX510")
          ("rotate-bit-field"    "IDX511")
          ("reverse-bit-field"   "IDX512")
          ("integer->list"       "IDX513")
          ("integer->list"       "IDX514")
          ("list->integer"       "IDX515")
          ("booleans->integer"   "IDX516")))
        (61 "A more general cond clause"
         ("cond"))
        (62 "S-expression comments")
        (63 "Homogeneous and Heterogeneous Arrays"
         (("array?"            "IDX1108")
          ("equal?"            "IDX1109")
          ("array-rank"        "IDX1110")
          ("array-dimensions"  "IDX1111")
          ("make-array"        "IDX1112")
          ("make-shared-array" "IDX1114")
          ("list->array"       "IDX1115")
          ("array->list"       "IDX1116")
          ("vector->array"     "IDX1117")
          ("array->vector"     "IDX1118")
          ("array-in-bounds?"  "IDX1119")
          ("array-ref"         "IDX1120")
          ("array-set!"        "IDX1121")
          ("a:floc128b"        "IDX1122")
          ("a:floc64b"         "IDX1124")
          ("a:floc32b"         "IDX1126")
          ("a:floc16b"         "IDX1128")
          ("a:flor128b"        "IDX1130")
          ("a:flor64b"         "IDX1132")
          ("a:flor32b"         "IDX1134")
          ("a:flor16b"         "IDX1136")
          ("a:flor128b"        "IDX1138")
          ("a:flor64b"         "IDX1140")
          ("a:flor32b"         "IDX1142")
          ("a:fixz64b"         "IDX1144")
          ("a:fixz32b"         "IDX1146")
          ("a:fixz16b"         "IDX1148")
          ("a:fixz8b"          "IDX1150")
          ("a:fixn64b"         "IDX1152")
          ("a:fixn32b"         "IDX1154")
          ("a:fixn16b"         "IDX1156")
          ("a:fixn8b"          "IDX1158")
          ("a:bool"            "IDX1160")))
        (64 "A Scheme API for test suites") ; draft
        (65 :withdrawn)
        (66 "Octet Vectors"
         ("u8vector?"
          "make-u8vector"
          "u8vector"
          "u8vector->list"
          "list->u8vector"
          "u8vector-length"
          "u8vector-ref"
          "u8vector-set!"
          "u8vector=?"
          "u8vector-compare"
          "u8vector-copy!"
          "u8vector-copy"))
        (67 "Compare Procedures"
         (("boolean-compare"        "node_idx_2")
          ("char-compare"           "node_idx_4")
          ("char-compare-ci"        "node_idx_6")
          ("string-compare"         "node_idx_8")
          ("string-compare-ci"      "node_idx_10")
          ("symbol-compare"         "node_idx_12")
          ("integer-compare"        "node_idx_14")
          ("rational-compare"       "node_idx_16")
          ("real-compare"           "node_idx_18")
          ("complex-compare"        "node_idx_20")
          ("number-compare"         "node_idx_22")
          ("vector-compare"         "node_idx_24")
          ("vector-compare-as-list" "node_idx_26")
          ("list-compare"           "node_idx_28")
          ("list-compare-as-vector" "node_idx_30")
          ("pair-compare-car"       "node_idx_32")
          ("pair-compare-cdr"       "node_idx_34")
          ("pair-compare"           "node_idx_36")
          ("default-compare"        "node_idx_40")
          ("refine-compare"         "node_idx_42")
          ("select-compare"         "node_idx_44")
          ("cond-compare"           "node_idx_46")
          ("if3"                    "node_idx_48")
          ("if=?"                   "node_idx_50")
          ("if<?"                   "node_idx_52")
          ("if>?"                   "node_idx_54")
          ("if<=?"                  "node_idx_56")
          ("if>=?"                  "node_idx_58")
          ("if-not=?"               "node_idx_60")
          ("=?"                     "node_idx_62")
          ("<?"                     "node_idx_64")
          (">?"                     "node_idx_66")
          ("<=?"                    "node_idx_68")
          (">=?"                    "node_idx_70")
          ("not=?"                  "node_idx_72")
          ("</<?"                   "node_idx_74")
          ("</<=?"                  "node_idx_76")
          ("<=/<?"                  "node_idx_78")
          ("<=/<=?"                 "node_idx_80")
          (">/>?"                   "node_idx_82")
          (">/>=?"                  "node_idx_84")
          (">=/>?"                  "node_idx_86")
          (">=/>=?"                 "node_idx_88")
          ("chain=?"                "node_idx_90")
          ("chain<?"                "node_idx_92")
          ("chain>?"                "node_idx_94")
          ("chain<=?"               "node_idx_96")
          ("chain>=?"               "node_idx_98")
          ("pairwise-not=?"         "node_idx_100")
          ("min-compare"            "node_idx_102")
          ("max-compare"            "node_idx_104")
          ("kth-largest"            "node_idx_106")
          ("compare-by<"            "node_idx_108")
          ("compare-by>"            "node_idx_110")
          ("compare-by<="           "node_idx_112")
          ("compare-by>="           "node_idx_114")
          ("compare-by=/<"          "node_idx_116")
          ("compare-by=/>"          "node_idx_118")
          ("debug-compare"          "node_idx_120")))
        (68 :withdrawn)
        (69 "Basic hash tables"
         (("make-hash-table"                 "mkh")
          ("hash-table?"                     "hsht")
          ("alist->hash-table"               "lst")
          ("hash-table-equivalence-function" "hsht1")
          ("hash-table-hash-function"        "hsht2")
          ("hash-table-ref"                  "hsht3")
          ("hash-table-ref/default"          "hsht4")
          ("hash-table-set!"                 "hsht5")
          ("hash-table-delete!"              "hsht6")
          ("hash-table-exists?"              "hsht7")
          ("hash-table-update!"              "hsht8")
          ("hash-table-update!/default"      "hsht9")
          ("hash-table-size"                 "hsht11")
          ("hash-table-keys"                 "hsht12")
          ("hash-table-values"               "hsht13")
          ("hash-table-walk"                 "hsht14")
          ("hash-table-fold"                 "hsht15")
          ("hash-table->alist"               "hsht16")
          ("hash-table-copy"                 "hsht17")
          ("hash-table-merge!"               "hsht18")
          ("hash"                            "hsh")
          ("string-hash"                     "strng")
          ("string-ci-hash"                  "strng19")
          ("hash-by-identity"                "hshb")))
        (70 "Numbers"
         (("number?"          "6.2.5")
          ("complex?"         "6.2.5")
          ("real?"            "6.2.5")
          ("rational?"        "6.2.5")
          ("integer?"         "6.2.5")
          ("exact?"           "6.2.5")
          ("inexact?"         "6.2.5")
          ("="                "6.2.5")
          ("<"                "6.2.5")
          (">"                "6.2.5")
          ("<="               "6.2.5")
          (">="               "6.2.5")
          ("finite?"          "6.2.5")
          ("infinite?"        "6.2.5")
          ("zero?"            "6.2.5")
          ("positive?"        "6.2.5")
          ("negative?"        "6.2.5")
          ("odd?"             "6.2.5")
          ("even?"            "6.2.5")
          ("max"              "6.2.5")
          ("min"              "6.2.5")
          ("+"                "6.2.5")
          ("*"                "6.2.5")
          ("-"                "6.2.5")
          ("/"                "6.2.5")
          ("abs"              "6.2.5")
          ("quotient"         "IDX258")
          ("remainder"        "IDX259")
          ("modulo"           "IDX260")
          ("gcd"              "6.2.5")
          ("lcm"              "6.2.5")
          ("numerator"        "6.2.5")
          ("denominator"      "6.2.5")
          ("floor"            "6.2.5")
          ("ceiling"          "6.2.5")
          ("truncate"         "6.2.5")
          ("round"            "6.2.5")
          ("exact-floor"      "6.2.5")
          ("exact-ceiling"    "6.2.5")
          ("exact-truncate"   "6.2.5")
          ("exact-round"      "6.2.5")
          ("rationalize"      "6.2.5")
          ("exp"              "6.2.5")
          ("log"              "6.2.5")
          ("sin"              "6.2.5")
          ("cos"              "6.2.5")
          ("tan"              "6.2.5")
          ("asin"             "6.2.5")
          ("acos"             "6.2.5")
          ("atan"             "6.2.5")
          ("sqrt"             "6.2.5")
          ("expt"             "6.2.5")
          ("make-rectangular" "6.2.5")
          ("make-polar"       "6.2.5")
          ("real-part"        "6.2.5")
          ("imag-part"        "6.2.5")
          ("magnitude"        "6.2.5")
          ("angle"            "6.2.5")
          ("exact->inexact"   "6.2.5")
          ("inexact->exact"   "6.2.5")
          ("number->string"   "6.2.5")
          ("string->number"   "6.2.5")))
        (71 "LET-syntax for multiple values"
         ("let"))
        (72 "Simple hygienic macros"
         ("define-syntax"
          "let-syntax"
          "letrec-syntax"
          "identifier?"
          "bound-identifier=?"
          "free-identifier=?"
          "literal-identifier=?"
          "syntax"
          "quasisyntax"
          "datum->syntax-object"
          "syntax-object->datum"
          "make-capturing-identifier"
          "begin-for-syntax"
          "around-syntax"
          "syntax-error"
          "syntax-case"
          "with-syntax"
          "syntax-rules"))
        (73 :withdrawn)
        (74 "Octet-Addressed Binary Blocks"
         ("endianness"
          "blob?"
          "make-blob"
          "blob-length"
          "blob-u8-ref"
          "blob-s8-ref"
          "blob-u8-set!"
          "blob-s8-set!"
          "blob-uint-ref"
          "blob-sint-ref"
          "blob-uint-set!"
          "blob-sint-set!"
          "blob-u16-ref"
          "blob-s16-ref"
          "blob-u16-native-ref"
          "blob-s16-native-ref"
          "blob-u16-set!"
          "blob-s16-set!"
          "blob-u16-native-set!"
          "blob-s16-native-set!"
          "blob-u32-ref"
          "blob-s32-ref"
          "blob-u32-native-ref"
          "blob-s32-native-ref"
          "blob-u32-set!"
          "blob-s32-set!"
          "blob-u32-native-set!"
          "blob-s32-native-set!"
          "blob-u64-ref"
          "blob-s64-ref"
          "blob-u64-native-ref"
          "blob-s64-native-ref"
          "blob-u64-set!"
          "blob-s64-set!"
          "blob-u64-native-set!"
          "blob-s64-native-set!"
          "blob=?"
          "blob-copy!"
          "blob-copy"
          "blob->u8-list"
          "u8-list->blob"
          "blob->uint-list"
          "blob->sint-list"
          "uint-list->blob"
          "sint-list->blob"))
        (75 "R6RS Unicode data")        ; draft
        (76 :withdrawn)
        (77 "Preliminary Proposal for R6RS Arithmetic") ; draft
        (78 "Lightweight testing"
         ("check"
          "check-ec"
          "check-report"
          "check-set-mode!"
          "check-reset!"
          "check-passed?"))
        (79 "Primitive I/O")            ; draft
        (80 "Stream I/O")               ; draft
        (81 "Port I/O")                 ; draft
        (82 "Stream Ports")             ; draft
        (83 "R6RS Library Syntax")      ; draft
        (84 "Universal Identifiers")    ; draft
        (85 "Recursive Equivalence Predicates") ; draft
        (86 :stupid)
        (87 "=> in case clauses")       ; draft
        (88 "Keyword Objects")          ; draft
        (89 "Optional parameters")      ; draft
        (90 "Extensible hash table constructor") ; draft
        (91 "Extended ports")))                  ; draft

(provide 'scheme-lookup-srfi)
