#lang deinprogramm/sdp

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

(define between
  (lambda (from to)
    (cond
      ((= from to) (list from))
      ((< from to)
       (cons from (between (+ from 1) to))))))

