;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname animal) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Gürteltier hat folgende Eigenschaften:
; - Gewicht (in g)
; - lebendig oder tot
(define-record-functions dillo
  make-dillo
  dillo? ; \(\Longleftarrow\)
  (dillo-weight natural)
  (dillo-alive? boolean))

(: make-dillo (natural boolean -> dillo))
(: dillo? (any -> boolean))
(: dillo-weight (dillo -> natural))
(: dillo-alive? (dillo -> boolean))

(define d1 (make-dillo 55000 #t)) ; 55 kg, lebendig 
(define d2 (make-dillo 58000 #f)) ; 58 kg, tot
(define d3 (make-dillo 60000 #t)) ; 60 kg, lebendig
(define d4 (make-dillo 63000 #f)) ; 63 kg, tot

; Gürteltier mit 500g Futter füttern
(: feed-dillo (dillo -> dillo))

(check-expect (feed-dillo d1) (make-dillo 55500 #t))
(check-expect (feed-dillo d2) d2)

#;(define feed-dillo
  (lambda (d)
    (if (dillo-alive? d)
        (make-dillo (+ (dillo-weight d) 500)
                    #t)
        d)))

(define feed-dillo
  (lambda (d)
    (make-dillo (if (dillo-alive? d)
                    (+ (dillo-weight d) 500)
                    (dillo-weight d))
                (dillo-alive? d))))


; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo d1) (make-dillo 55000 #f))
(check-expect (run-over-dillo d2) d2)
(check-expect (run-over-dillo d2) d2)

(define run-over-dillo
  (lambda (d)
    (make-dillo (dillo-weight d)
                #f)))


; Ein Papagei hat folgende Eigenschaften:
; - Gewicht in Gramm
; - Satz, den er sagt
(define-record-functions parrot
  make-parrot
  parrot? ; \(\Longleftarrow\)
  (parrot-weight   natural)
  (parrot-sentence string))

(: make-parrot (natural string -> parrot))
(: parrot? (any -> boolean))
(: parrot-weight (parrot -> natural))
(: parrot-sentence (parrot -> string))

(define p1 (make-parrot 10000 "Der Gärtner war's.")) ; 10kg, Miss Marple
(define p2 (make-parrot 5000 "Ich liebe Dich.")) ; 5kg, Romantiker 

; Papagei mit 50 g Futter füttern
(: feed-parrot (parrot -> parrot))

(check-expect (feed-parrot p1) (make-parrot 10050 "Der Gärtner war's."))
(check-expect (feed-parrot p2) (make-parrot 5050 "Ich liebe Dich."))

(define feed-parrot
  (lambda (p)
    (make-parrot (+ (parrot-weight p) 50)
                 (parrot-sentence p))))

; Ein Tier ist eins der folgenden:
; - Gürteltier
; - Papagei
(define animal
  (signature
    (mixed dillo parrot)))

; Gewicht eines Tiers feststellen
(: animal-weight (animal -> natural))

(check-expect (animal-weight d1) 55000)
(check-expect (animal-weight d2) 58000)
(check-expect (animal-weight p1) 10000)
(check-expect (animal-weight p2) 5000)

(define animal-weight
  (lambda (a)
    (cond
      ((dillo? a) (dillo-weight a))
      ((parrot? a) (parrot-weight a)))))
