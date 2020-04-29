#lang deinprogramm/sdp/vanilla

; Eine natürliche Zahl ist eine der folgenden:
; - 0
; - der Nachfolger einer natürlichen Zahl
;   (eine positive Zahl)

; Nachfolger einer Zahl
(: successor (natural -> natural))

(define successor
  (lambda (n)
    (+ n 1)))

; Vorgänger einer Zahl
(: predecessor (natural -> natural))

(define predecessor
  (lambda (n)
    (cond
      ((zero? n)
       (violation "0 does not have a predecessor"))
      ((positive? n)
       (- n 1)))))

; Potenz einer Zahl berechnen
(: power (number natural -> number))

(check-expect (power 5 0) 1)
(check-expect (power 5 3) 125)

(define power
  (lambda (base exponent)
    (cond
      ((zero? exponent) 1)
      ((positive? base)
       (* base
          (power base (predecessor exponent)))))))

; Liste aus Kopien eines Werts erzeugen
(: copies (natural %element -> (list-of %element)))

(check-expect (copies 5 "Mike")
              (list "Mike" "Mike" "Mike" "Mike" "Mike"))
(check-expect (copies 4 23)
              (list 23 23 23 23))

(define copies
  (lambda (count element)
    (cond
      ((zero? count)
       empty)
      ((positive? count)
       (cons element
             (copies (- count 1) element))))))


; Nummeriertes Element aus einer Liste holen
(: nth ((list-of %element) natural -> %element))

(check-expect (nth (list 1 2 3 4 5) 0) 1)
(check-expect (nth (list 1 2 3 4 5) 2) 3)
(check-error (nth (list 1 2 3 4 5) 5))

(define nth
  (lambda (list index)
    (cond
      ((empty? list) (violation "nth: Die Liste ist nicht lang genug"))
      ((cons? list)
       (cond
         ((zero? index) (first list))
         ((positive? index)
          (nth (rest list) (- index 1))))))))

; Liste aufeinanderfolgender ganzer Zahlen berechnen
(: between (integer integer -> (list-of integer)))

(check-expect (between 3 7) (list 3 4 5 6 7))

(define between
  (lambda (from to)
    (count-from from (+ (- to from) 1))))

; Aufeinanderfolgende Zahlen aber einer Zahl generieren
(: count-from (integer natural -> (list-of integer)))

(check-expect (count-from 3 5) (list 3 4 5 6 7))

(define count-from
  (lambda (from count)
    (cond
      ((zero? count) empty)
      ((positive? count)
       (cons from
             (count-from (+ from 1) (- count 1)))))))

#;(define between
  (lambda (from to)
    (cond
      ((= from to) (list from))
      ((< from to)
       (cons from (sequence (+ from 1) to))))))

; Eine natürliche Zahl ist:
; - 0
; - das Doppelte einer natürlichen Zahl
; - der Nachfolger des Doppelten einer natürlichen Zahl

; Potenz einer Zahl berechnen
(: power2 (number natural -> number))

(check-expect (power2 5 0) 1)
(check-expect (power2 5 3) 125)

(define power2
  (lambda (base exponent)
    (cond
      ((zero? exponent) 1)
      ((even? exponent)
       (define half (power2 base (quotient exponent 2)))
       (* half half))
      ((odd? exponent)
       (define half (power2 base (quotient (- exponent 1) 2)))
       (* half half base)))))
