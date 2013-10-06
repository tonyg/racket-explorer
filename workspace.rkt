#lang racket/gui
;; A Smalltalk-like embeddable workspace.
;; Extracted and generalized from mred/private/repl.rkt.

(provide workspace%)

(define repl-text%
  (class text%
    (init-field eval-string)
    (inherit insert last-position get-text erase change-style clear-undos set-max-undo-history)
    (rename-super [super-on-char on-char])
    (define prompt-pos 0)
    (define locked? #f)
    (augment*
     [can-insert? (lambda (start end) (and (>= start prompt-pos) (not locked?)))]
     [can-delete? (lambda (start end) (and (>= start prompt-pos) (not locked?)))])
    (override*
     [on-char (lambda (c)
		(super-on-char c)
		(when (and (memq (send c get-key-code) '(#\return #\newline #\003))
			   (not locked?))
		  (set! locked? #t)
		  (eval-string (get-text prompt-pos (last-position)))))])
    (public*
     [new-prompt (lambda ()
		   (output "> ")
		   (set! prompt-pos (last-position))
		   (set! locked? #f)
		   (clear-undos))]
     [output (lambda (str)
	       (let ([l? locked?])
		 (set! locked? #f)
		 (insert str)
		 (set! locked? l?)))]
     [reset (lambda ()
	      (set! locked? #f)
	      (set! prompt-pos 0)
	      (erase)
	      (new-prompt))])
    (super-new)
    (reset)
    (set-max-undo-history 'forever)))

(define workspace%
  (class object%
    (init-field parent)
    (init-field [namespace (current-namespace)])

    (field [buffer (make-object repl-text% (lambda (s) (eval-string s)))])
    (field [canvas
	    (new editor-canvas%
		 [parent parent]
		 [style '(no-border auto-hscroll resize-corner)])])

    (define queue-output
      (let ((repl-eventspace (current-eventspace)))
	(lambda (proc)
	  (parameterize ((current-eventspace repl-eventspace))
	    (queue-callback proc #f)))))
    
    (field [user-custodian (make-custodian)])
    
    (field [user-output-port
	    (let ([leftover #""]
		  [cvt (bytes-open-converter "UTF-8-permissive" "UTF-8")])
	      (make-output-port
	       'console
	       always-evt
	       (lambda (s start end flush? breakable?) 
		 (queue-output (lambda () 
				 ;; s might end in the middle of a UTF-8 encoding.
				 ;;  Get a complete prefix, and save the rest.
				 (let ([s (bytes-append leftover (subbytes s start end))])
				   (let-values ([(res used status) (bytes-convert cvt s)])
				     (send buffer output (bytes->string/utf-8 res))
				     (set! leftover (subbytes s used))))))
		 (- end start))
	       void))])			; no close action
    
    (init-field [user-eventspace
		 (parameterize ((current-custodian user-custodian))
		   (make-eventspace))])

    (define/public (eval-term expr)
      (parameterize ((current-eventspace user-eventspace))
	(queue-callback
	 (lambda ()
	   (dynamic-wind
	     void
	     (lambda () 
	       (call-with-values
		   (lambda () (call-with-continuation-prompt
			       (lambda ()
				 (parameterize ((current-output-port user-output-port)
						(current-error-port user-output-port)
						(current-input-port (open-input-bytes #"")))
				   (eval (cons '#%top-interaction expr) namespace)))))
		 (lambda results
		   (for-each 
		    (lambda (v) 
		      (when (not (void? v))
			(parameterize ([current-output-port user-output-port])
			  (print v) 
			  (newline))))
		    results))))
	     (lambda ()
	       (queue-output (lambda () (send buffer new-prompt)))))))))

    (define/public (eval-string expr-str)
      (eval-term (read (open-input-string expr-str))))

    (define/public (focus)
      (send canvas focus))

    (super-new)
    ((current-text-keymap-initializer) (send buffer get-keymap))
    (send buffer auto-wrap #t)
    (send canvas set-editor buffer)))
