#|
"pong is a video game for two players"

"each player controls a paddle, which is
a rather thin and wide white rectangle"
===>
"a paddle is a rather thin and wide white rectangle"
"each player controls a paddle"

		   
|#

[include "parse.scm"]
[include "maps.scm"]

(define (listify x)
  (if x `(,x) '()))

(define (stem description)
  `(,@(listify (map-ref description qualifier:))
    ,@(map-ref description qualities: '())
    ,(map-ref description stem:)))

(define description? map?)

(define (subordinate-clauses description)
  (let ((subordinate (map-ref description
			      subordinate:)))
    (if subordinate
	(let ((subordinate-descriptions
	       (only description? subordinate)))
	  `(,@(append-map subordinate-clauses
			  subordinate-descriptions)
	    (,@(stem description)
	     ,@(append-map (fn (x)
			(cond
			 ((description? x)
			  (stem x))
			 (else ;(list? x)
			  `(,x))
			 #;(else
			 `(,x))))
		    subordinate))))
	'())))

(define clause-expansion
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
	     (~ stem: <noun>))

   (<subject> ::= <object>)

   (<object> ::= (a <noun>)
	     (~ stem: <noun>
		qualifier: a))
   
   (<object> ::= (a <qualities> <noun>)
	     (~ stem: <noun>
		qualifier: a
		qualities: <qualities>))

   (<object> ::= (<subject> "," which <verb> <object>)
	     (join <subject>
		   (~ subordinate:
		      `(,<verb> ,<object>))))
   
   (<qualities> ::= <quality> `(,<quality>))
   (<quality> ::= (rather <quality>) <quality>)
   (<qualities> ::= (<quality> <qualities>)
		`(,<quality> . ,<qualities>))
   (<qualities> ::= (<quality> & <qualities>)
		`(,<quality> . ,<qualities>))
   (& ::= "and")
   (& ::= ",")
   (rather ::= "rather")
   (rather ::= "quite")
   (there ::= "there")
   (which ::= "which")
   (is ::= "is")
   (a ::= "a")
   (a ::= "an")
   (a ::= "each")
   (<verb> ::= "is" )
   (<verb> ::= "controls")
   (<quality> ::= "white")
   (<quality> ::= "thin")
   (<quality> ::= "wide")
   (<quality> ::= "small")
   #|
   (<noun> ::= "player")
   (<noun> ::= "paddle")
   (<noun> ::= "rectangle")
   (<noun> ::= "square")
   (<noun> ::= "ball")
   |#
   (<noun> ::= ,(fn _ #t))
   ))

#|

(define (preview sentence)
  (let ((words (words sentence)))
    (print sentence)
    (writeln
     (parse
      words
      clause-expansion '<sentence>))
    (newline)))

(preview "each player controls a paddle , which is a rather thin and wide white rectangle")


(preview "there is a ball , which is a small white square")
(exit)
|#
