#lang racket/gui
;; Utility for interactive exploration of complex data structures.

;; Copyright (c) 2013 Tony Garnock-Jones <tonygarnockjones@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


(require racket/generic)
(require racket/port)
(require mrlib/hierlist)
(require "workspace.rkt")

(provide (struct-out explorer-item)
	 gen:explorable
	 ->explorer-item
	 explorer%
	 explore
	 explore/refresh)

(struct explorer-item (label children value) #:prefab)

(define-generics explorable
  (->explorer-item explorable))

(define (fill-hierlist-item! hierlist-item label value children-producer)
  (define e (send hierlist-item get-editor))
  (send e erase)
  (send e insert label)
  (send hierlist-item user-data (cons value children-producer))
  hierlist-item)

(define explorer-hierlist%
  (class hierarchical-list%
    (init-field explorer)
    (super-new)
    (define/override (on-item-opened i)
      ;; (Re)compute children on-demand
      ((cdr (send i user-data)) i))
    (define/override (on-item-closed i)
      ;; Remove children - they'll be readded when next i is opened.
      (for [(item (send i get-items))] (send i delete-item item)))
    (define/override (on-select i)
      (send explorer select-value! (if i (car (send i user-data)) (void))))))

(module interaction-anchor racket
  (require racket/class racket/gui/base)
  (provide anchor)
  (define-namespace-anchor anchor))

(require 'interaction-anchor)

(define (make-comfortable-namespace)
  ;; (define n (namespace-anchor->empty-namespace anchor))
  ;; (eval '(require racket racket/class racket/gui/base) n)
  (define n (namespace-anchor->namespace anchor))
  n)

(define explorer%
  (class object%
    (init-field parent)
    (define hierlist (new explorer-hierlist% [parent parent] [explorer this]))
    (field [namespace (make-comfortable-namespace)])
    (field [interaction-panel (new workspace% [parent parent] [namespace namespace])])
    (super-new)

    (define/public (erase)
      (for [(item (send hierlist get-items))] (send hierlist delete-item item)))

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

    (define/public (syntax->explorer-items stx)
      (append (let ((loc-info (filter cdr
				      (map (match-lambda
					    [(list label extractor) (cons label (extractor stx))])
					   `((source ,syntax-source)
					     (line ,syntax-line)
					     (column ,syntax-column)
					     (position ,syntax-position)
					     (span ,syntax-span)
					     (source-module ,syntax-source-module))))))
		(if (null? loc-info)
		    '()
		    (list (explorer-item "- location"
					 (hash-items->explorer-items loc-info)
					 stx))))
	      (let ((keys (syntax-property-symbol-keys stx)))
		(if (null? keys)
		    '()
		    (list (explorer-item "- properties"
					 (hash-items->explorer-items
					  (map (lambda (k) (cons k (syntax-property stx k))) keys))
					 stx))))
	      (syntax-e stx)))

    (define/public (path->explorer-items p)
      (map (lambda (portion) (if (path-for-some-system? portion)
				 (path->string portion)
				 portion))
	   (explode-path p)))

    (define/public (item-label x)
      (if (syntax? x)
	  (string-append "#<syntax " (item-label (syntax->datum x)) ">")
	  (with-output-to-string (lambda ()
				   ;; (when (object-name x)
				   ;;   (display (object-name x))
				   ;;   (display ": "))
				   (write x)))))

    (define/public (add-explorer-item! parent label children value)
      (fill-hierlist-item! (if (null? children)
                               (send parent new-item)
                               (send parent new-list))
                           label
                           value
                           (lambda (i)
                             (add-list-like!* (if (procedure? children)
                                                  (children)
                                                  children)
                                              i))))

    (define/public (add-item! x)
      (add-item!* x hierlist))

    (define/public (add-item!* x parent)
      (define-syntax-rule (container children)
        (add-explorer-item! parent (item-label x) (lambda () children) x))
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
	[(? string?) (container (hash-items->explorer-items `((length . ,(string-length x)))))]
	[(? cons?) (container x)]
	[(? hash?) (container (hash-items->explorer-items (hash->list x)))]
	[(? set?) (container (set->list x))]
	[(? vector?) (container (vector->list x))]
	[(? object?) (container (hash-items->explorer-items (hash->list (object->hash x))))]
	[(? struct?) (container (vector->list (struct->vector x '#:opaque)))]
	[(? procedure?) (container (procedure-explorer-items x))]
	[(? syntax?) (container (syntax->explorer-items x))]
	[(? path-for-some-system?) (container (path->explorer-items x))]
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

(define (explore/refresh thunk)
  (define f (new frame%
		 [label "Racket Explorer"]
		 [width 300]
		 [height 700]))
  (define b (new button%
		 [label "Refresh"]
		 [parent f]
		 [callback (lambda (b evt) (refresh))]))
  (define e (new explorer%
		 [parent f]))
  (define (refresh)
    (send e erase)
    (send e add-item! (thunk)))
  (refresh)
  (send f show #t)
  refresh)
