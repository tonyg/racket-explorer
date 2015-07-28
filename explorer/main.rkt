#lang racket/gui
;; Utility for interactive exploration of complex data structures.

;; Copyright (c) 2013, 2015 Tony Garnock-Jones <tonygarnockjones@gmail.com>
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

;; An ExplorerItem is an
;;   (explorer-item (U (-> String) String)
;;                  (U (-> (Listof Any)) (Listof Any))
;;                  Any)
;; where label and children, if procedures, must be nullary procedures
;; yielding the *actual* label and children at the time of the call.
(struct explorer-item (label children value) #:prefab)

(define-generics explorable
  (->explorer-item explorable))

(define (refresh-hierlist-item-label! hierlist-item)
  (define ei (send hierlist-item user-data))
  (define l (explorer-item-label ei))
  (define e (send hierlist-item get-editor))
  (send e erase)
  (send e insert (if (procedure? l) (l) l))
  hierlist-item)

(define (fill-hierlist-item! hierlist-item ei)
  (send hierlist-item user-data ei)
  (refresh-hierlist-item-label! hierlist-item))

(define explorer-hierlist%
  (class hierarchical-list%
    (init-field explorer)
    (super-new)
    (define/override (on-item-opened i)
      (refresh-hierlist-item-label! i)
      ;; (Re)compute children on-demand
      (define ei (send i user-data))
      (define kids (explorer-item-children ei))
      (send explorer add-list-like!*
            (if (procedure? kids)
                (with-handlers [(exn? (lambda (e) (list e)))]
                  (kids))
                kids)
            i))
    (define/override (on-item-closed i)
      (refresh-hierlist-item-label! i)
      ;; Remove children - they'll be readded when next i is opened.
      (for [(item (send i get-items))] (send i delete-item item)))
    (define/override (on-select i)
      (cond
        [i
         (refresh-hierlist-item-label! i)
         (define ei (send i user-data))
         (send explorer select-value! #:define? #t (explorer-item-value ei))]
        [else
         (send explorer select-value! #:define? #f (void))]))))

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
              (let ((debug-info (syntax-debug-info stx)))
                (list (explorer-item "- debug info"
                                     (hash-items->explorer-items (hash->list debug-info))
                                     stx)))
	      (syntax-e stx)))

    (define/public (path->explorer-items p)
      (append (list (explorer-item "- path components"
                                   (reverse
                                    (foldl (lambda (piece acc)
                                             (if (null? acc)
                                                 (list piece)
                                                 (cons (build-path (car acc) piece) acc)))
                                           '()
                                           (explode-path (normalize-path (path->complete-path p)))))
                                   (explode-path p)))
              (cond
                [(file-exists? p)
                 (define s (file-size p))
                 (list (explorer-item (format "- length: ~a bytes" s) '() s)
                       (explorer-item "- as bytes"
                                      (lambda () (list (file->bytes p)))
                                      (void))
                       (explorer-item "- as a string"
                                      (lambda () (list (file->string p)))
                                      (void)))]
                [(directory-exists? p)
                 (define ps (directory-list p))
                 (cons (explorer-item (format "- count: ~a" (length ps))
                                      '()
                                      (length ps))
                       (map (lambda (sub-p) (build-path p sub-p)) ps))]
                [else
                 (list (explorer-item "- file or directory does not exist" '() (void)))])))

    (define/public (exn->explorer-items e)
      (define message (exn-message e))
      (define context (continuation-mark-set->context (exn-continuation-marks e)))
      (define formatted (parameterize ([current-error-port (open-output-string)])
                          ((error-display-handler) message e)
                          (get-output-string (current-error-port))))
      (list (explorer-item formatted '() formatted)
            (explorer-item "continuation-mark context" context context)))

    (define/public (struct->explorer-items x)
      (define-values (maybe-type skipped?) (struct-info x))
      (define elts (vector->list (struct->vector x '#:opaque)))
      (cons (struct-type->explorer-items maybe-type skipped?)
            (cdr elts)))

    (define/public (struct-type->explorer-items maybe-type skipped?)
      (cond [(not maybe-type)
             (if skipped? '#:opaque '#:none)]
            [else (define-values (name
                                  init-field-count
                                  auto-field-count
                                  accessor-proc
                                  mutator-proc
                                  immutable-k-list
                                  super-type
                                  super-skipped?)
                    (struct-type-info maybe-type))
                  (explorer-item (if skipped?
                                     (format "type: subtype of ~a" name)
                                     (format "type: ~a" name))
                                 (hash-items->explorer-items
                                  `((name . ,name)
                                    (init-field-count . ,init-field-count)
                                    (auto-field-count . ,auto-field-count)
                                    (accessor-proc . ,accessor-proc)
                                    (mutator-proc . ,mutator-proc)
                                    (immutable-k-list . ,immutable-k-list)
                                    (super-type . ,(struct-type->explorer-items super-type
                                                                                super-skipped?))))
                                 (list maybe-type skipped?))]))

    (define/public (item-label x)
      (if (syntax? x)
	  (string-append "#<syntax " (item-label (syntax->datum x)) ">")
	  (with-output-to-string (lambda ()
				   ;; (when (object-name x)
				   ;;   (display (object-name x))
				   ;;   (display ": "))
				   (write x)))))

    (define/public (add-explorer-item! parent ei)
      (fill-hierlist-item! (if (null? (explorer-item-children ei))
                               (send parent new-item)
                               (send parent new-list))
                           ei))

    (define/public (add-item! x)
      (add-item!* x hierlist))

    (define/public (add-item!* x parent)
      (define-syntax-rule (container children)
        (add-explorer-item! parent
                            (explorer-item (lambda () (item-label x))
                                           (lambda () children)
                                           x)))
      (define-syntax-rule (container/nodelay children)
        (add-explorer-item! parent
                            (explorer-item (lambda () (item-label x))
                                           children
                                           x)))
      (match x
	[(? explorer-item? ei) (add-explorer-item! parent ei)]
	[(? exact-integer?)
	 (container (map (lambda (p b)
			   (explorer-item (string-append p (number->string x b))
					  '()
					  x))
			 (list "#b" "#o" "" "#x")
			 (list 2 8 10 16)))]
	[(? string?) (container (hash-items->explorer-items `((length . ,(string-length x)))))]
	[(? bytes?) (container (hash-items->explorer-items `((bytes . ,(string-length x)))))]
	[(? cons?) (container x)]
	[(? hash?) (container (hash-items->explorer-items (hash->list x)))]
	[(? set?) (container (set->list x))]
	[(? vector?) (container (vector->list x))]
	[(? object?) (container (hash-items->explorer-items (hash->list (object->hash x))))]
        [(? exn?) (container (exn->explorer-items x))]
	[(? struct?) (container (struct->explorer-items x))]
	[(? procedure?) (container (procedure-explorer-items x))]
	[(? syntax?) (container (syntax->explorer-items x))]
	[(? path-for-some-system?) (container (path->explorer-items x))]
        [(? box?) (container (unbox x))]
        [(? promise?) (container (force x))]
	[(? explorable?) (add-item!* (->explorer-item x) parent)]
	[_ (container/nodelay '())])
      (void))

    (define/public (add-list-like!* xs parent)
      (match xs
	['() parent]
	[(cons h t)
	 (add-item!* h parent)
	 (add-list-like!* t parent)]
	[other
	 (add-item!* other parent)]))

    (define/public (select-value! #:define? [define? #t] v)
      (if define?
	  (namespace-set-variable-value! 'this v #t namespace)
	  (namespace-undefine-variable! 'this namespace))
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
