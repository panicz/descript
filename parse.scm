[load "prelude.scm"]

(define (parse input
               grammar 
               initial-token
	       . context)

  (define (parses/token token input)
    (append-map 
     (fn (`(,category ,=> ,structure
	    ,semantics))
	 (cond ((eq? category token)
		(map (fn (`(,parsed ,input))
			 `(,(semantics token
				       parsed)
			   ,input))
		     (matches/rule structure
				   input)))
	       (else
		'())))
	 grammar))

  (define (non-terminal? symbol)
    (symbol? symbol))

  (define (terminal? symbol)
    (not (non-terminal? symbol)))

  (define (matches/rule rule input)
    ;;(writeln rule input)
    (match rule
      ('()  `((() ,input)))
      (`(,token . ,rule*)
       (if (terminal? token)
          (matches/terminal token 
                            rule*
                            input)
       ;else
          (matches/non-terminal token
				rule*
				input)))))
;;34567890123456789012345678901234567890
  (define (matches/terminal token
                            rule* 
                            input)
    (or 
     (and-let* 
         ((`(,word . ,words) input)
          ((recognizes? token word)))
       (map (fn (`(,parsed ,input))
              `((,word . ,parsed)
                ,input))
            (matches/rule rule*
			  words)))
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
                                input)
    (let* ((parses (parses/token
		    token
		    input)))
      (append-map 
        (fn (`(,parsed ,input))
          (map (fn (`(,rest ,input))
		   ;;(writeln token parsed)
		   `((,parsed . ,rest) 
                    ,input))
               (matches/rule rule* 
                             input)))
        parses)))

  (append-map
   (fn (`(,parsed ,input))
       (match input
	 ('() `(,parsed))
	 (_ '())))
   (parses/token initial-token
		 input))
  )

(define-syntax language-spec
  (syntax-rules (unquote)
    ((_)
     '())

    ((_ (<category> -> ,expression)
	. <rest>)
     `((<category> -> (,expression)
		   ,cons #;(fn (_ `(,<category>))
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
    
    ((_ (<category> -> (<structure>))
	. <rest>)
     `((<category> -> (<structure>)
		   ,cons #;(fn (_ `(,<structure>))
			    <structure>))
       . ,(language-spec . <rest>)))

    ((_ (<category> -> (<structure> ...))
	. <rest>)
     `((<category> -> (<structure> ...)
		   ,cons #;(fn (first second)
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
	   (string-replace
	    "," " , "
	    (string-append s " .")))
       (only (isnt _ equal? "")
	     (string-split text "."))))

(define (words sentence)
  (only (isnt _ equal? "")
	(string-split sentence " ")))


#|
(define dog-grammar*
  [language-spec
   [<sentence> ::= ["a" <noun> <verb>]]
   [<noun> ::= ,(fn (x) (string=? x "dog"))]
   [<verb> ::= "sleeps"]])

(writeln dog-grammar*)


(define dog-grammar
  `[[<sentence> ::= ["a" <noun> <verb>] ,cons]
    [<noun> ::= [,(lambda (x . y)
		    (string-ci=? "dog" x))] ,cons]
    [<verb> ::= ["sleeps"] ,cons]])

(writeln
(parse '("a" "dog" "sleeps") dog-grammar '<sentence>))

#;(define dog-grammar*
  [language-spec
   [<sentence> ::= ["a" <noun> <verb>]]
   [<noun> ::= "dog"]
   [<verb> ::= "sleeps"]])


(writeln
 (parse '("a" "dog" "sleeps")
	dog-grammar*
	'<sentence>))

|#

;; kiedy wpisujemy
;; "run pong"
;; to co dokładnie się dzieje?
;; 1. w dyskursie wyszukiwane jest znaczenie słowa
;; "pong".
;; 2. znaczeniem słowa "pong" jesti konstruktor
;; obiektu typu "video game". konstruktor zostaje
;; wywołany, a jego wartość zostanie zachowana
;; na liście 'threads' obiektu 'situations'
;; 3. wywołanie konstruktora powoduje:
;;    1. stworzenie obiektu "video game"
;;    2. stworzenie dwóch obiektów graczy
;;    3. stworzenie dwóch obiektów paletek
;;       i umiejscowienie ich na przeciwległych
;;       stronach ekranu
;;    4. podpięcie sterowania graczy do paletek
;;    5. nadanie ograniczeń na ruch paletek (tylko
;;       do góry albo na dół)
;;    6. stworzenie obiektu piłki, umiejscowienie jej
;;       na środku ekranu, i nadanie losowego wektora
;;       prędkości
;;    7. dodanie procedury kolizji dla par
;;       (piłka, górna krawędź ekranu),
;;       (piłka, dolna krawędź ekranu)
;;       -- odbicie piłki
;;    8. dodanie procedury kolizji dla par
;;       (piłka, lewa krawędź ekranu) oraz
;;       (piłka, prawa krawędź ekranu)
;;    9. uruchomienie nowego wątku gry

(define descript-grammar
  (language-spec
   (<sentence> ::= (<subject> <predicates>)
	       #;`(fn (denotation)
		    (,<subject> denotation)
	       ))
   (<sentence> ::= ("There" be <object>))
   (<sentence> ::= (when <sentence> then <sentence>))
   (when ::= "when")
   (when ::= "whenever")
   (when ::= ("every" "time"))
   (when ::= ("each" "time"))
   (then ::= "then")
   (then ::= ",")
   (<predicates> ::= <predicate>)
   (<predicates> ::= (<predicate> & <predicates>))
   (<predicate> ::= (<verb> <object>))
   (<predicate> ::= (<verb> <roles>))
   (<predicate> ::= (<phrasal-verb>))
   (<role> ::= (<locator> <object>))
   (<roles> ::= (<role>))
   (<roles> ::= (<role> <roles>)
	      `(,<role> . ,<roles>))
   (<verb> ::= be)
   (<verb> ::= "controls")
   (<verb> ::= ("can" <verb>))
   (<verb> ::= "move")
   (<verb> ::= "hits")
   (<verb> ::= "moves")
   (<phrasal-verb> ::= ("bounces" "off"))
   (<phrasal-verb> ::= ("starts" <object> "over"))
   (be ::= "is")
   (be ::= "are")
   (<verb> ::= (be <participle> <preposition>))
   (<participle> ::= "located")
   (<object> ::= (<simple-object> "," which <predicate>))
   (<object> ::= (<simple-object>))
   (<object> ::= (<simple-object> "or" <object>))
   (<object> ::= ("either" <simple-object> "or" <object>))
   (<simple-object> ::= (a <qualifiers> <noun>))
   (<simple-object> ::= (a <noun>))
   (<subject> ::= <object>)
   (<simple-object> ::= (<qualifiers> <noun>))
   (<simple-object> ::= <noun>)
   (<object> ::= (<simple-object>
		  <preposition>
		  <object>))
   (<qualifiers> ::= <qualifier> `(,<qualifier>))
   (<qualifiers> ::= (<qualifier> & <qualifiers>)
		 `(,<qualifier> . ,<qualifiers>))
   (<qualifiers> ::= (<qualifier> <qualifiers>)
		 `(,<qualifier> . ,<qualifiers>))
   (<qualifier> ::= <quantifier>)
   (<qualifier> ::= ("rather" <qualifier>) <qualifier>)
   (<qualifier> ::= "thin")
   (<qualifier> ::= "wide")
   (<qualifier> ::= "white")
   (<qualifier> ::= "random")
   (<qualifier> ::= "small")
   (<qualifier> ::= "top")
   (<qualifier> ::= "bottom")
   (<qualifier> ::= "opposite")
   (<qualifier> ::= (<noun> "'s"))
   (<quantifier> ::= "two" 2)
   (& ::= ",")
   (& ::= "and")
   (& ::= "or")
   (a ::= "a")
   (a ::= "an")
   (a ::= "Each")
   (a ::= "the")
   (which ::= "which")
   (which ::= "who")
   (which ::= "that")
   (<preposition> ::= "for")
   (<preposition> ::= "on")
   (<preposition> ::= "of")
   (<locator> ::= "from")
   (<locator> ::= "in")
   (<locator> ::= "behind")
   (<noun> ::= ("video" "game"))
   (<noun> ::= "it")
   (<noun> ::= "players")
   (<noun> ::= "edge")
   (<noun> ::= "player")
   (<noun> ::= "middle")
   (<noun> ::= "direction")
   (<noun> ::= "paddle")
   (<noun> ::= "paddles")
   (<noun> ::= "rectangle")
   (<noun> ::= "screen")
   (<noun> ::= "ball")
   (<noun> ::= "square")   
   (<noun> ::= "sides")
   (<noun> ::= "up")
   (<noun> ::= "down")
   (<noun> ::= "Pong")))

(define grammar1
  (language-spec
   (<sentence> ::= (<subject> <verb> <object>))
   (<verb> ::= ("is"))
   (<subject> ::= (<object>))
   (<object> ::= (<simple-object>
		  <preposition>
		  <object>))
   (<object> ::= (<simple-object>))
   (<simple-object> ::= (<noun>))
   (<simple-object> ::= ("a" <noun>))
   (<noun> ::= ("video" "game"))
   (<noun> ::= ("Pong"))
   (<noun> ::= ("players"))
   (<simple-object> ::= (<number> <noun>))
   (<number> ::= ("two"))
   (<preposition> ::= ("for"))))


(define (preview sentence)
  (print sentence)
  (writeln
   (parse
    (words sentence)
    descript-grammar '<sentence>))
  (newline))

(define (preview1 sentence)
  (print sentence)
  (writeln
   (parse
    (words sentence)
    grammar1 '<sentence>))
  (newline))

;; 3. wywołanie konstruktora powoduje:
;;    1. stworzenie obiektu "video game"

(preview1 "Pong is a video game for two players")



;;    2. stworzenie dwóch obiektów graczy i paletek dla nich


(set! grammar1
  `(,@(language-spec
       (<simple-object> ::= ("each" <noun>))
       (<object> ::= (<simple-object> "," "which"
				      <verb> <object>))
       (<simple-object> ::= ("a" <qualities> <noun>))
       (<qualities> ::= (<quality>))
       (<qualities> ::= (<quality> <qualities>))
       (<qualities> ::= (<quality> & <qualities>))
       (& ::= ("and"))
       (<quality> ::= ("rather" <quality>))
       (<quality> ::= ("thin"))
       (<quality> ::= ("wide"))
       (<quality> ::= ("white"))
       (<verb> ::= ("controls"))
       (<noun> ::= ("paddle"))
       (<noun> ::= ("player"))
       (<noun> ::= ("rectangle")))
    ,@grammar1))

(preview1
"each player controls a paddle , which is a rather thin and wide white rectangle")


(exit)

;;    3.  umiejscowienie paletek na przeciwległych
;;       stronach ekranu


(preview "the paddles are located on the opposite sides of the screen and can move either up or down")


;;    4. podpięcie sterowania graczy do paletek
;;    5. nadanie ograniczeń na ruch paletek (tylko
;;       do góry albo na dół)
;;    6. stworzenie obiektu piłki, umiejscowienie jej
;;       na środku ekranu, i nadanie losowego wektora
;;       prędkości
;;    7. dodanie procedury kolizji dla par
;;       (piłka, górna krawędź ekranu),
;;       (piłka, dolna krawędź ekranu)
;;       -- odbicie piłki
;;    8. dodanie procedury kolizji dla par
;;       (piłka, lewa krawędź ekranu) oraz
;;       (piłka, prawa krawędź ekranu)
;;    9. uruchomienie nowego wątku gry


(preview "There is a ball , which is a small white square")

(preview "the ball moves from the middle of the screen in a random direction")

(preview "it hits the top or bottom edge of the screen")


(preview "it bounces off")


(preview "when it hits the top or bottom edge of the screen , it bounces off")

("when it hits the side of the screen behind player 's paddle , the other player gains a point , and the ball starts its movement over from the middle of the screen"
