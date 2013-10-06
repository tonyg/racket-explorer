#lang racket/gui

;; TODO
;; - copy and paste
;; - lazy creation of subitems
;; - cycle detection (if we had lazy unfolding this would not be a problem)

(require racket/generic)
(require racket/port)
(require mrlib/hierlist)
(require "workspace.rkt")

(provide (struct-out explorer-item)
	 gen:explorable
	 ->explorer-item
	 explorer%
	 explore)

(struct explorer-item (label children value) #:prefab)

(define-generics explorable
  (->explorer-item explorable))

(define (fill-hierlist-item! hierlist-item label value)
  (define e (send hierlist-item get-editor))
  (send e erase)
  (send e insert label)
  (send hierlist-item user-data value)
  hierlist-item)

(define explorer-hierlist%
  (class hierarchical-list%
    (init-field explorer)
    (super-new)
    (define/override (on-select i)
      (send explorer select-value! (if i (send i user-data) (void))))))

(define (make-comfortable-namespace)
  (define n (make-gui-namespace))
  (eval '(require racket) n)
  n)

(define explorer%
  (class object%
    (init-field parent)
    (define hierlist (new explorer-hierlist% [parent parent] [explorer this]))
    (field [namespace (make-comfortable-namespace)])
    (field [interaction-panel (new workspace% [parent parent] [namespace namespace])])
    (super-new)

    (define/public (object->hash o)
      (for/hash ((k (in-list (field-names o))))
	(values k (dynamic-get-field k o))))

    (define/public (hash-item->explorer-item kv)
      (match-define (cons k v) kv)
      (explorer-item (format "~a: ~v" k v)
		     (if (or (string? k) (symbol? k))
			 (list v)
			 (list k v))
		     kv))

    (define/public (hash-items->explorer-items kvs)
      (map (lambda (kv) (hash-item->explorer-item kv)) kvs))

    (define/public (procedure-explorer-items p)
      (define-values (required-kws accepted-kws) (procedure-keywords p))
      (define optional-kws (if (eq? accepted-kws #f)
			       '...
			       (set->list (set-subtract (list->set accepted-kws)
							(list->set required-kws)))))
      (hash-items->explorer-items
       `((arity . ,(procedure-arity p))
	 ,@(if (object-name p)
	       `((name . ,(object-name p)))
	       '())
	 ,@(if (null? required-kws)
	       '()
	       `((required-keywords . ,required-kws)))
	 ,@(if (null? optional-kws)
	       '()
	       `((optional-keywords . ,optional-kws)))
	 )))

    (define/public (item-label x)
      (with-output-to-string (lambda ()
			       ;; (when (object-name x)
			       ;;   (display (object-name x))
			       ;;   (display ": "))
			       (write x))))

    (define/public (add-explorer-item! parent label children value)
      (if (null? children)
	  (fill-hierlist-item! (send parent new-item) label value)
	  (add-list-like!* children (fill-hierlist-item! (send parent new-list) label value))))

    (define/public (add-item! x)
      (add-item!* x hierlist))

    (define/public (add-item!* x parent)
      (define (container children)
	(add-explorer-item! parent (item-label x) children x))
      (match x
	[(explorer-item label children value)
	 (add-explorer-item! parent label children value)]
	[(? exact-integer?)
	 (container (map (lambda (p b)
			   (explorer-item (string-append p (number->string x b))
					  '()
					  x))
			 (list "#b" "#o" "" "#x")
			 (list 2 8 10 16)))]
	[(? cons?) (container x)]
	[(? hash?) (container (hash-items->explorer-items (hash->list x)))]
	[(? set?) (container (set->list x))]
	[(? vector?) (container (vector->list x))]
	[(? object?) (container (hash-items->explorer-items (hash->list (object->hash x))))]
	[(? struct?) (container (vector->list (struct->vector x '#:opaque)))]
	[(? procedure?) (container (procedure-explorer-items x))]
	[(? explorable?) (add-item!* (->explorer-item x) parent)]
	[_ (container '())])
      (void))

    (define/public (add-list-like!* xs parent)
      (match xs
	['() parent]
	[(cons h t)
	 (add-item!* h parent)
	 (add-list-like!* t parent)]
	[other
	 ;; (add-explorer-item! parent "." '() (void))
	 (add-item!* other parent)]))

    (define/public (select-value! v)
      (if (void? v)
	  (namespace-undefine-variable! 'this namespace)
	  (namespace-set-variable-value! 'this v #t namespace))
      (send interaction-panel focus))

    ))

(define (explore v)
  (define f (new frame%
		 [label "Racket Explorer"]
		 [width 300]
		 [height 700]))
  (define e (new explorer%
		 [parent f]))
  (send e add-item! v)
  (send f show #t)
  e)

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
	(with-input-from-file "main.rkt"
	  (lambda ()
	    (read-line)
	    (port->list))))

  (struct x ()
	  #:methods gen:explorable
	  [(define (->explorer-item x)
	     (explorer-item "Hi" '() 'bar))])
  (send e add-item! (x)))
