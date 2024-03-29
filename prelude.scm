(cond-expand
  (guile
   (use-modules
    (srfi srfi-1)
    (ice-9 match)
    (srfi srfi-69)))

    (chicken
   [require-extension srfi-1]
   (require-extension matchable)
   [require-extension srfi-69]))

(define-syntax infix/postfix
  (syntax-rules ()
    ((infix/postfix x somewhat?)
     (somewhat? x))

    ((infix/postfix left related-to? right)
     (related-to? left right))

    ((infix/postfix left related-to? right . likewise)
     (let ((right* right))
       (and (infix/postfix left related-to? right*)
	    (infix/postfix right* . likewise))))))

(define-syntax extract-placeholders
  (syntax-rules (_)
    ((extract-placeholders final () () body)
     (final (infix/postfix . body)))

    ((extract-placeholders final () args body)
     (lambda args (final (infix/postfix . body))))

    ((extract-placeholders final (_ op . rest)
			   (args ...) (body ...))
     (extract-placeholders final rest (args ... arg)
			   (body ... arg op)))

    ((extract-placeholders final (arg op . rest) args
			   (body ...))
     (extract-placeholders final rest args
			   (body ... arg op)))

    ((extract-placeholders final (_) (args ...)
			   (body ...))
     (extract-placeholders final () (args ... arg)
			   (body ... arg)))

    ((extract-placeholders final (arg) args
			   (body ...))
     (extract-placeholders final () args
			   (body ... arg)))))

(define-syntax identity-syntax
  (syntax-rules ()
    ((identity-syntax form)
     form)))

(define-syntax is
  (syntax-rules ()
    ((is . something)
     (extract-placeholders identity-syntax
			   something () ()))))

(define-syntax isnt
  (syntax-rules ()
    ((isnt . something)
     (extract-placeholders not something () ()))))

(define (only cool? stuff)
  (if (null? stuff)
      '()
      (if (cool? (car stuff))
	  `(,(car stuff) . ,(only cool? (cdr stuff)))
	  (only cool? (cdr stuff)))))

(define empty? null?)

(define-syntax fn
  (syntax-rules ()
    ((fn args . body)
     (lambda params
       (match params
	 (args . body)
	 (_ (error params
		   " do not match "
		   'args)))))))

[define override
  [make-hash-table eq?]]

[define-syntax assign!
  [syntax-rules []
    [[assign! [procedure keys ...] value]
     [hash-table-set!
      [hash-table-ref override procedure]
      [list keys ...] value]]
    [[assign! variable value]
     [set! variable value]]]]

[define-syntax fn+
  [syntax-rules []
    [[fn+ args . body]
     [let* [[f [fn args . body]]
	    [h [make-hash-table equal?]]
	    [f+ [fn params
		    [if [hash-table-exists? h params]
			[hash-table-ref h params]
			[apply f params]]]]]
       [hash-table-set! override f+ h]
       f+]]]]

[define-syntax define+
  [syntax-rules []
    [[define+ [name . args] . body]
     [define name [fn+ args . body]]]]]

(define-syntax e.g.
  (syntax-rules (===>)
    ((_ expression ===> value)
     (let ((result expression))
       (unless (equal? result 'value)
	 (error 'expression
		": expected "'value
		", got "result))))
    ((_ expression)
     (unless expression
       (error 'expression " does not hold")))))

(define (map1 f l)
  [match l
    ['[] '[]]
    [`[,h . ,t]
     `[,[f h] . ,[map1 f t]]]])

(cond-expand
 (chicken
  (define (map f l . ls)
    (match l
      ['[] '[]]
      [`[,h . ,t]
       `[,[apply f h
		 [map1 [fn [`[,h* . ,_]]
			   h]
		       ls]]
	 . ,[apply map f t [map1 [fn [`[,_ . ,t*]]
				     t*]
				 ls]]]]))


  (define (string-join los glue)
    (match los
      (`(,s . ,l)
       (foldl (fn (a b)
		  (string-append a glue b))
	      s l))
      (_ "")))

  (e.g.
   (string-join '("a" "b" "c") ",")
   ===> "a,b,c")

  (define break-string string-split)
  
  )
 (guile
  (define-syntax-rule (assert condition)
    (let ((result condition))
      (unless result
	(error "Assertion failed: "'condition))))


  (define (break-string string break)
    (let ((n (string-length break)))
      (if (= n 0)
	  '()
	  (let ((index (string-contains string break)))
	    (if index
		`(,(substring string 0 index)
		  . ,(break-string (substring
				    string
				    (+ index n))
				   break))
		`(,string))))))

   (define (print . args)
     (for-each display args)
     (newline))
   
   ))

(define (none satisfying? elements)
  (not (any satisfying? elements)))

(define (string-substitute pattern
			   #;with replacement
				  #;in string)
  (string-join
   (break-string string pattern)
   replacement))

(e.g.
 (string-substitute "," ";" "a,b,c") ===> "a;b;c")

(define (append-map f l)
  (match l
    ('() '())
    (`(,h . ,t)
     `(,@(f h) ,@(append-map f t)))))

(define-syntax and-let*
  (syntax-rules ()
    ((_) #t)
    ((_ ()) #t)

    ((_ () body ...)
     (let () body ...))

    ((_ ((value binding) rest ...) body ...)
     (match binding
       (value
	(and-let* (rest ...)
	  body ...))
       (_ #f)))

    ((_ ((condition) rest ...)
	body ...)
     (and condition
	  (and-let* (rest ...)
	    body ...)))

    ((_ ((values ... expression) rest ...) body ...)
     (call-with-values (lambda () expression)
       (match-lambda* 
	 ((values ... . _)
	  (and-let* (rest ...)
	    body ...))
	 (_ #f))))

    ))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
	 ((name . pattern)
	  template))))))

(define (writeln . text)
  (for-each write text)
  (newline))
