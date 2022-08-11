#lang deinprogramm/sdp/advanced

; Ein Ausdruck ist eins der folgenden:
; - eine Zahl
; - eine Liste der Form (+ Ausdruck Ausdruck)
; - eine Liste der Form (* Ausdruck Ausdruck)

; Ist ein Wert ein Additionsausdruck?
(: addition? (any -> boolean))

(check-expect (addition? '5) #f)
(check-expect (addition? '(+ 1 2)) #t)
(check-expect (addition? '(* 1 2)) #f)

(define addition?
  (lambda (x)
    (and (cons? x)
         (equal? '+ (first x)))))
(define addition
  (signature (predicate addition?)))

; Ist ein Wert ein Multiplikationsausdruck?
(: multiplication? (any -> boolean))

(check-expect (multiplication? '5) #f)
(check-expect (multiplication? '(* 1 2)) #t)
(check-expect (multiplication? '(+ 1 2)) #f)

(define multiplication?
  (lambda (x)
    (and (cons? x)
         (equal? '* (first x)))))
(define multiplication
  (signature (predicate multiplication?)))

(define expression
  (signature (mixed number addition multiplication)))

; Wert eines Ausdrucks berechnen
(: compute (expression -> number))

(check-expect (compute '23) 23)
(check-expect (compute '(+ 23 42)) 65)
(check-expect (compute '(+ 23 (* 6 7))) 65)

(define compute
  (lambda (exp)
    (cond
      ((number? exp) exp)
      ((addition? exp)
       (+ (compute (first (rest exp)))
          (compute (first (rest (rest exp))))))
      ((multiplication? exp)
       (* (compute (first (rest exp)))
          (compute (first (rest (rest exp)))))))))

