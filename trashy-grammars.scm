[include "parse.scm"]


(define descript-grammar
  (language-spec
   (<sentence> ::= (<subject> <predicates>)
	       (map (fn (<predicate>)
			`(,<subject> . ,<predicate>))
		    <predicates>))
   (<sentence> ::= ("there" be <object>)
	       `(("there" ,be ,<object>)))
   (<sentence> ::= (when <sentence> then <sentence>))
   (when ::= "when")
   (when ::= "whenever")
   (when ::= ("every" "time"))
   (when ::= ("each" "time"))
   (then ::= "then")
   (then ::= ",")
   (<predicates> ::= (<predicate>))
   (<predicates> ::= (<predicate> & <predicates>)
		 `(,<predicate> . ,<predicates>))
   (<predicate> ::= (<verb> <object>))
   (<predicate> ::= (<verb> <roles>))
   (<predicate> ::= <phrasal-verb>)
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
   (<object> ::= <simple-object>)
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
   (a ::= "each")
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
   (<verb> ::= "is")
   (<subject> ::= <object>)
   (<object> ::= (<object>
		  <preposition>
		  <object>))
;;   (<object> ::= (object>))
   (<object> ::= <noun>)
   (<object> ::= ("a" <noun>))
   (<noun> ::= ("video" "game"))
   (<noun> ::= "Pong")
   (<noun> ::= ("players"))
   (<object> ::= (<number> <noun>))
   (<number> ::= ("two"))
   (<preposition> ::= ("for"))))


(set! grammar1
  `(,@(language-spec
       (<object> ::= ("each" <noun>))
       (<object> ::= (<object> "," "which"
				      <verb> <object>))
       (<object> ::= ("a" <qualities> <noun>))
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

(define (flatten x)
  (match x
    (`(,a . ,d)
     `(,@(flatten a) ,@(flatten d)))
    ('()
     '())
    (_
     `(,x))))

(define (preview sentence)
  (print sentence)
  (writeln
   (map (fn (interpretation)
	    (map flatten interpretation))
	(parse
	 (words sentence)
	 descript-grammar '<sentence>)))
  (newline))


(preview "Pong is a video game for two players")

;; a video game for two players
;; "for two players"
;; rama znaczeniowa słowa "player" jest związana
;; ze znaczeniem sytuacji "game". Ale! w naszym
;; frameworku znaczenie to funkcja dyskursu.
;; dlatego jak mamy:
;; a video game for two players
;; czy raczej
;; (<object>
;;  (<object>
;;   "a"
;;   (<quality> "video")
;;   (<noun> "game"))
;;  (<attribute>
;;   "for"
;;   2
;;   (<noun> "players")))


;;(designate "players")
	    


;;    2. stworzenie dwóch obiektów graczy i paletek dla nich

(preview
"each player controls a paddle , which is a rather thin and wide white rectangle")




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


(preview "there is a ball , which is a small white square")

(preview "the ball moves from the middle of the screen in a random direction")


(preview "when it hits the top or bottom edge of the screen , it bounces off")

(preview "when it hits the side of the screen behind player 's paddle , the other player gains a point , and the ball starts its movement over from the middle of the screen")

(exit)
