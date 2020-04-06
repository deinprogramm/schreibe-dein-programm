;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname accumulator) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Liste umdrehen
(: invert ((list-of %a) -> (list-of %a)))

(check-expect (invert empty) empty)
(check-expect (invert (list 1 2 3 4)) (list 4 3 2 1))

(define invert
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element (invert (rest list))
                       (first list))))))

; Element an Liste anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

(check-expect (append-element (list 1 2 3) 4) (list 1 2 3 4))
(check-expect (append-element empty 4) (list 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty)) ; list geht hier nicht
      ((cons? list)
       (cons (first list)
             (append-element (rest list) element))))))


; Hilfsfunktion zum Umdrehen einer Liste
(: invert-helper ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (invert-helper (list 4 5 6) (list 3 2 1))
              (list 6 5 4 3 2 1))
(check-expect (invert-helper (list 1 2 3) empty)
              (list 3 2 1))
                             

(define invert-helper
  (lambda (list inverted)
    (cond
      ((empty? list) inverted)
      ((cons? list)
       (invert-helper (rest list)
                      (cons (first list) inverted))))))

; Summe der Elemente einer Liste von Zahlen berechnen
(: list-sum ((list-of number) -> number))

(check-expect (list-sum (list 2 3 5 7)) 17)

#;(define list-sum
  (lambda (list0)
    (list-sum-helper list0 0)))

;; sum ist die Summe der Zahlen in list0 vor list
#;(define list-sum-helper
  ;; sum ist die Summer aller Elemente in list0 vor list
  (lambda (list sum)
    (cond
      ((empty? list) sum)
      ((cons? list)
       (list-sum-helper (rest list) (+ (first list) sum))))))

(define list-sum
  (lambda (list0)
    (define accumulate
      ;; sum ist die Summer aller Elemente in list0 vor list
      (lambda (list sum)
        (cond
          ((empty? list) sum)
          ((cons? list)
           (accumulate (rest list) (+ (first list) sum))))))
    (accumulate list0 0)))

(: factorial (natural -> natural))

(check-expect (factorial 5) 120)

(define factorial
  (lambda (n0)    
    (define accumulate
      ;; acc ist das Produkt aller Zahlen von (+ n 1) bis n0
      (lambda (n acc)
        (cond
          ((zero? n) acc)
          ((positive? n)
           (accumulate (- n 1) (* n acc))))))
    (accumulate n0 1)))
           
        
        
