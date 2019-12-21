

[include "parse.scm"]
[include "maps.scm"]
[require-extension srfi-113]
[require-extension srfi-128]

(define (listify x)
  (if x `(,x) '()))

(define (stem description)
  `(,@(listify (map-ref description #:qualifier))
    ,@(map-ref description #:qualities '())
    ,(map-ref description #:stem)))

(define (description? x)
  (and (map? x)
       (eq? (map-ref x type:) 'description)))

(define (description . attributes)
  (apply persistent-map
	 #:type 'description
	 attributes))

(define (predicate . attributes)
  (apply persistent-map
	 #:type 'predicate
	 attributes))

(define (subordinate-clauses description)
  (let ((subordinate (map-ref description
			      #:subordinate)))
    (if subordinate
	(let ((subordinate-descriptions
	       (only description? subordinate)))
	  `(,@(append-map subordinate-clauses
			  subordinate-descriptions)
	    (,@(stem description)
	     ,@(append-map
		(fn (x)
		    (if (description? x)
			(stem x)
			`(,x)))
		subordinate))))
	'())))

(define subordinate-clause-expansion
  (language-spec
   (<sentence> ::= (<subject> <verb> <object>)
	       `(,@(subordinate-clauses <subject>)
		 ,@(subordinate-clauses <object>)
		 (,@(stem <subject>)
		  ,<verb>
		  ,@(stem <object>))))
   
   (<sentence> ::= (there is <object>)
	       `(,@(subordinate-clauses <object>)
		 (,there ,is ,@(stem <object>))))
   (<object> ::= <noun>
	     (description #:stem <noun>))

   (<subject> ::= <object>)

   (<object> ::= (a <noun>)
	     (description
	      #:stem <noun>
	      #:qualifier a))
   
   (<object> ::= (a <qualities> <noun>)
	     (description
	      #:stem <noun>
	      #:qualifier a
	      #:qualities <qualities>))

   (<object> ::= (<subject> "," which <verb> <object>)
	     (join <subject>
		   (description
		    #:subordinate `(,<verb> ,<object>))))
   
   (<qualities> ::= (<quality>))
   (<quality> ::= (rather <quality>) <quality>)
   (<qualities> ::= (<quality> <qualities>)
		`(,<quality> . ,<qualities>))
   (<qualities> ::= (<quality> & <qualities>)
		`(,<quality> . ,<qualities>))
   (& ::= "and")
   (& ::= ",")
   (rather ::= "rather")
   (rather ::= "quite")
   (rather ::= "fairly")
   
   (there ::= "there")
   (which ::= "which")
   (is ::= "is")
   (a ::= "a")
   (a ::= "an")
   (a ::= "each")
   (<verb> ::= "is")
   (<verb> ::= "controls")
   (<quality> ::= "white")
   (<quality> ::= "thin")
   (<quality> ::= "wide")
   (<quality> ::= "small")
   (<quality> ::= "opposite")
   (<noun> ::= ,(fn _ #t))
   ))

"the paddles are located on the opposite sides of the screen and can move either up or down"

(define multiple-predicate-expansion
  (language-spec
   (<sentence> ::= (<subject> <predicates>)
	       (map (fn (<predicate>)
			`(,<subject> . ,<predicate>))
		    <predicates>))
   (<subject> ::= <object>)

   (<object> ::= (<noun>))
   
   (<object> ::= (a <noun>))
   
   (<object> ::= (a <qualities> <noun>))

   (<object> ::= (both <object> & <object>))

   (<object> ::= (<subject> "of" <object>))
   
   (<qualities> ::= (<quality>))
   (<quality> ::= (rather <quality>) <quality>)
   (<qualities> ::= (<quality> <qualities>)
		`(,<quality> . ,<qualities>))
   (<qualities> ::= (<quality> & <qualities>)
		`(,<quality> . ,<qualities>))
   (<predicates> ::= (<predicate>))
   (<predicates> ::= (<predicate> & <predicates>)
		 `(,<predicate> . ,<predicates>))
   (<predicate> ::= (be <participle> <locative>))
   (a ::= "a")
   (a ::= "an")
   (a ::= "the")

   (be ::= "is")
   (be ::= "are")
   (<locative> ::= (<preposition> <object>))
   (<direction> ::= "up")
   (<direction> ::= "down")
   (<preposition> ::= "on")
   (<participle> ::= "located")
   (<noun> ::= ,(fn _ #t))
   (& ::= "and")
   (& ::= ",")
   (& ::= "or")
   (both ::= "both")
   (both ::= "either")
   (rather ::= "rather")
   (rather ::= "quite")
   (rather ::= "fairly")))


(define equal-comparator
  (make-equal-comparator))
  
(define (preview grammar sentence)
  (let ((words (words sentence)))
    (print sentence)
    (writeln
     (set->list
      (list->set
       equal-comparator
       (parse
	words
	grammar '<sentence>))))
    (newline)))

(preview
 subordinate-clause-expansion
 "each player controls a paddle , which is a rather thin and wide white rectangle")

(preview
 subordinate-clause-expansion
 "there is a ball , which is a small white square")

(exit)

