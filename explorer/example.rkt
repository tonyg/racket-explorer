#lang racket/gui

(require racket/runtime-path)
(define-runtime-path here ".")

(require "main.rkt")

(module+ main
  (define e (explore (explorer-item "Hello, world" '(1 2 3) 'foo)))

  (send e add-item!
        (letrec ((counter 0)
                 (ei (explorer-item (lambda ()
                                      (set! counter (+ counter 1))
                                      (format "magic mutable thing ~a" counter))
                                    (lambda () (list 'a ei 'z))
                                    (void))))
          ei))

  (send e add-item! (delay (begin (log-info "promise forced")
                                  (+ 2 3))))

  (send e add-item!
	(set "hello" 1 2 3.45 (lambda (x) x)))

  (send e add-item!
	(lambda (x #:y y #:z [z 2]) y))

  (send e add-item!
	(new (class object%
	       (super-new)
	       (field [x 5] [y 10]))))

  (send e add-item! e)

  (send e add-item!
	(hash "hello" 1
	      "world" 2))

  (send e add-item!
        (let ((h (make-hash)))
          (hash-set! h "mutable" "hash")
          h))

  (send e add-item! (box 'this-is-a-box))

  ;; (send e add-item!
  ;; 	(with-input-from-file "main.rkt"
  ;; 	  (lambda ()
  ;; 	    (read-line)
  ;; 	    (port->list))))

  (send e add-item!
	(with-input-from-file (build-path here "example.rkt")
	  (lambda ()
	    (parameterize ((read-accept-reader #t)
			   (read-accept-lang #t))
	      (read-syntax)))))

  (send e add-item! here)

  (send e add-item! (explorer-item "testing exceptions"
                                   (lambda () (+ 1 "hello"))
                                   (void)))

  (struct test-struct-opaque (field1 field2))
  (send e add-item! (test-struct-opaque 'a 'b))

  (struct test-struct-transparent (field1 field2) #:transparent)
  (send e add-item! (test-struct-transparent 'a 'b))

  (struct test-struct-prefab (field1 field2) #:prefab)
  (send e add-item! (test-struct-prefab 'a 'b))

  (struct test-substruct-transparent test-struct-transparent (field3 field4) #:transparent)
  (send e add-item! (test-substruct-transparent 'a 'b 'c 'd))

  (struct test-opaque-transparent test-struct-opaque (field3 field4) #:transparent)
  (send e add-item! (test-opaque-transparent 'a 'b 'c 'd))

  (struct test-transparent-opaque test-struct-transparent (field3 field4))
  (send e add-item! (test-transparent-opaque 'a 'b 'c 'd))

  (struct x ()
	  #:methods gen:explorable
	  [(define (->explorer-item x)
	     (explorer-item "Hi" '() 'bar))])
  (send e add-item! (x)))
