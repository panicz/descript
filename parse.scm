[include "prelude.scm"]

(define (parse input
               grammar 
               initial-token
	       . context)

  (define (parses/token token input forbidden)
    (append-map 
     (lambda (rule)
       (match rule
	 (`(,category ,=> ,structure
		      ,semantics)
	  (cond ((and (eq? category token)
		     (isnt rule member forbidden))
		 (map (fn (`(,parsed ,input))
			  `(,(semantics token
					parsed)
			    ,input))
		      (matches/rule structure
				    input
				    `(,rule
				      . ,forbidden))))
		(else
		 '())))))
	 grammar))

  (define (non-terminal? symbol)
    (symbol? symbol))

  (define (terminal? symbol)
    (not (non-terminal? symbol)))

  (define (matches/rule rule input forbidden)
    ;;(writeln rule input)
    (match rule
      ('()  `((() ,input)))
      (`(,token . ,rule*)
       (if (terminal? token)
          (matches/terminal token 
                            rule*
                            input
			    forbidden)
       ;else
          (matches/non-terminal token
				rule*
				input
				forbidden)))))
;;34567890123456789012345678901234567890
  (define (matches/terminal token
                            rule* 
                            input
			    forbidden)
    (or 
     (and-let* 
         ((`(,word . ,words) input)
          ((recognizes? token word)))
       (map (fn (`(,parsed ,input))
              `((,word . ,parsed)
                ,input))
            (matches/rule rule*
			  words
			  '())))
     '()))

  (define (recognizes? terminal word)
    (assert (string? word))
    (or (and (string? terminal)
             (string-ci=? word
                          terminal))
        (and (procedure? terminal)
             (apply terminal word context))))

  (define (matches/non-terminal token
                                rule* 
                                input
				forbidden)
    (let* ((parses (parses/token
		    token
		    input
		    forbidden)))
      (append-map 
        (fn (`(,parsed ,input))
          (map (fn (`(,rest ,input))
		   ;;(writeln token parsed)
		   `((,parsed . ,rest) 
                    ,input))
               (matches/rule rule* 
                             input
			     '())))
        parses)))

  (append-map
   (fn (`(,parsed ,input))
       (match input
	 ('() `(,parsed))
	 (_ '())))
   (parses/token initial-token
		 input '()))
  )

(define-syntax language-spec
  (syntax-rules (unquote)
    ((_)
     '())
    
    ((_ (<category> -> ,expression)
	. <rest>)
     `((<category> -> (,expression)
		   ,(fn (_ `(,<category>))
			    <category>))
       . ,(language-spec . <rest>)))

    ((_ (<category> -> ,expression <meaning>)
	. <rest>)
     `((<category> -> (,expression)
		   ,(fn (_ `(,<category>))
			    <meaning>))
       . ,(language-spec . <rest>)))
    
    ((_ (<category> -> (<structure> ...)
		    <meaning>)
	. <rest>)
     `((<category> -> (<structure> ...)
		   ,(lambda (category structure)
		      (let ((<category> category))
			(match structure
			  (`(,<structure> ...)
			   <meaning>)))))
       . ,(language-spec . <rest>)))
    #|
    ((_ (<category> -> (<structure>))
	. <rest>)
     `((<category> -> (<structure>)
		   ,(fn (_ `(,<structure>))
			    `(,<structure>)))
       . ,(language-spec . <rest>)))
    |#
    ((_ (<category> -> (<structure> ...))
	. <rest>)
     `((<category> -> (<structure> ...)
		   ,(fn (first second)
			    second))
       . ,(language-spec . <rest>)))

    ((_ (<category> -> <structure>)
	. <rest>)
     `((<category> -> (<structure>)
		   ,(fn (_ `(,<structure>))
			    <structure>))
       . ,(language-spec . <rest>)))

    ((_ (<category> -> <structure> <meaning>)
	. <rest>)
     `((<category> -> (<structure>)
		   ,(fn (<category>
			     `(,<structure>))
			    <meaning>))
       . ,(language-spec . <rest>)))
    ))

(define (sentences text)
  (map (fn (s)
	   (string-substitute
	    "," #;with " , "
	    #;in (string-append s " .")))
       (only (fn(x)(isnt x equal? ""))
	     (break-string text "."))))

(define (words sentence)
  (only (fn(x)(isnt x equal? ""))
	(break-string sentence " ")))
