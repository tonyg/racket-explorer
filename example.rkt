#lang racket/gui

(require "main.rkt")

(module+ main
  (define e (explore (explorer-item "Hello, world" '(1 2 3) 'foo)))

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

  ;; (send e add-item!
  ;; 	(with-input-from-file "main.rkt"
  ;; 	  (lambda ()
  ;; 	    (read-line)
  ;; 	    (port->list))))

  (send e add-item!
	(with-input-from-file "example.rkt"
	  (lambda ()
	    (parameterize ((read-accept-reader #t)
			   (read-accept-lang #t))
	      (read-syntax)))))

  (struct x ()
	  #:methods gen:explorable
	  [(define (->explorer-item x)
	     (explorer-item "Hi" '() 'bar))])
  (send e add-item! (x)))
