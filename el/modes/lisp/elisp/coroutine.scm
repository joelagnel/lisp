;; coroutine.scm --- Recursive Coroutines in Scheme

(define call/cc call-with-current-continuation)

;; `coroutine' is intended to be like `lambda': it returns a coroutine
;; object which can be `define'd into a variable.  It has space for a
;; lambda-style argument list.  Arguments of an inital invocation are
;; passed to the BODY-FORMS, parsed according to ARGLIST, while
;; arguments of a continuing invocation are returned as the value of
;; the `yield' form.
(defmacro coroutine (arglist . body-forms)
  ;; Not all identifiers need to be hygienicized, only those whose
  ;; scope overlaps that of BODY-FORMS and for which capture is
  ;; undesired.  (We do want to capture `yield'.)
  (let ((entry-point (gentemp))
        (top-level-return (gentemp))
        (body (gentemp))
        (args (gentemp))
        (recursive? (gentemp)))
    `(letrec 
         ((,entry-point #f)
          (,top-level-return #f)
          ;; Return a value to top-level and save current execution
          ;; point for next coroutine entry.  Returns the argument
          ;; list, if any, passed to the coroutine reinvocation.
          (yield (lambda (value)
                   (call/cc (lambda (entry)
                              ;; Save current entry point
                              (set! ,entry-point entry)
                              ;; Clear TLR and return the value
                              (let ((tlr ,top-level-return))
                                (set! ,top-level-return #f)
                                (tlr value))))))
          ;; Here we execute the code and take appropriate action to
          ;; return its value.
          (,body (lambda (,recursive? ,args)
                   (let ((value (apply (lambda ,arglist
                                         ,@body-forms)
                                       ,args)))
                     (if ,recursive?
                         ;; Return the value to the caller.
                         value
                         ;; Clear TLR and return the value to
                         ;; top-level, clearing entry-point so next
                         ;; call starts a new execution.
                         (let ((tlr ,top-level-return))
                           (set! ,entry-point #f)
                           (set! ,top-level-return #f)
                           (tlr value)))))))
       ;; This is the procedure called from outside the coroutine.
       (lambda ,args
         (if ,top-level-return
             ;; Recursive call
             (,body #t ,args)
             ;; Call from outside the coroutine.  This call only
             ;; returns by invocation of top-level-return.
             (call/cc (lambda (tlr)
                        (set! ,top-level-return tlr)
                        (if ,entry-point
                            ;; Continue a previous execution
                            (,entry-point ,args)
                            ;; Start a new evaluation
                            (,body #f ,args)))))))))

;; `define-coroutine' is like (define (function args ...) body ...).
(define-syntax define-coroutine
  (syntax-rules ()
    ((_ (name . arg) body ...)
     (define name (coroutine arg body ...)))
    ((_ (name arg ...) body ...)
     (define name (coroutine (arg ...) body ...)))))

;; Warning: coroutines are not re-entrant.  By which is meant that no
;; function called by a coroutine should call that coroutine.

;; coroutine.scm ends here
