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
