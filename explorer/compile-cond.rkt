#lang racket/base
;; Conditional compilation.

(provide compile/cond
         module-provides?)

(require (for-syntax racket))

(define-syntax-rule (compile/cond [test expr ...] ...)
  (let-syntax ((conditionally-compiled-code
                (lambda (stx)
                  (cond
                    [test (syntax (begin expr ...))]
                    ...))))
    (conditionally-compiled-code)))

(define (module-provides? module-path sym)
  (with-handlers [(exn:fail:filesystem:missing-module? (lambda (e) #f))]
    (dynamic-require module-path #f)
    (define-values (vars stxs) (module->exports module-path))
    (define (in-bindings? bs)
      (cond [(assoc 0 bs) =>
             (lambda (phase0)
               (cond [(assq sym (cdr phase0)) #t] [else #f]))]
            [else #f]))
    (or (in-bindings? vars)
        (in-bindings? stxs))))

(module+ test
  (require rackunit)

  (check-equal? (syntax->datum
                 (expand #'(compile/cond [(even? 2) (+ 3 4)] [else 'other])))
                '(let-values () (let-values () (#%app + (quote 3) (quote 4))))
                ;; Overly specific, but will do for now. It's hard to test macros.
                )

  (check-equal? (syntax->datum
                 (expand #'(compile/cond [(odd? 2) (+ 3 4)] [else 'other])))
                '(let-values () (let-values () (quote other)))
                )

  (check-false (module-provides? 'non-existent-module 'absent-sym))
  (check-false (module-provides? 'racket/base '--not-a-real-exported-identifier--explorer))
  (check-true (module-provides? 'racket/base '+))
  (check-true (module-provides? 'racket/set 'set-member?))
  (check-false (module-provides? 'racket/base 'set-member?))
  )
