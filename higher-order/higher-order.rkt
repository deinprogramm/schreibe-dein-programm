;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname higher-order) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Summe der Elemente einer Liste von Zahlen berechnen
(: list-sum ((list-of number) -> number))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list) (list-sum (rest list)))))))

; Produkt der Elemente einer Liste von Zahlen berechnen
(: list-product ((list-of number) -> number))

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list) (list-product (rest list)))))))

#;(define xxx
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list) (xxx (rest list)))))))

(: xxx (%a (%b %a -> %a) (list-of %b) -> %a))

(define xxx
  (lambda (for-empty for-cons list)
    (cond
      ((empty? list) for-empty)
      ((cons? list)
       (for-cons (first list)
                 (xxx for-empty for-cons (rest list)))))))

; im Stepper:
; (xxx 0 + (list 1 2 3))

(: list-fold (%a (%b %a -> %a) (list-of %b) -> %a))

(define list-fold
  (lambda (for-empty for-cons list)
    (cond
      ((empty? list) for-empty)
      ((cons? list)
       (for-cons (first list)
                 (list-fold for-empty for-cons (rest list)))))))

(: list-apply ((%a -> %b) (list-of %a) -> (list-of %b)))

(check-expect (list-apply even? (list 1 2 3))
              (list #f #t #f))

(define list-apply
  (lambda (f list)
    (list-fold empty
               (lambda (first-list result)
                 (cons (f first-list)
                       result))
               list)))
