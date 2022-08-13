#lang deinprogramm/sdp/beginner

; Eine Liste ist eins der folgenden:
; - eine leere Liste
; - eine Cons-Liste
(define list-of
  (lambda (element)
    (signature
     (mixed empty-list
            (cons-list-of element)))))

(define-record empty-list
  make-empty-list
  empty?)

(define empty (make-empty-list))

; Eine Cons-Liste besteht aus:
; - dem ersten Element
; - einer Liste mit den restlichen Elementen
(define-record (cons-list-of element)
  cons
  cons?
  (first element)
  (rest  (list-of element)))

(: cons-list-of (signature -> signature))

(: cons (%element (list-of %element) -> (cons-list-of %element)))
(: first ((cons-list-of %element) -> %element))
(: rest ((cons-list-of %element) -> (list-of %element)))

(define list-of-numbers (signature (list-of number)))
; leere Liste
(define list0 empty)
; einelementige Liste mit der Zahl 42
(define list1 (cons 42 empty))
; Liste mit den Zahlen 1 2 3
(define list3 (cons 1 (cons 2 (cons 3 empty))))
; Liste mit den Zahlen e und pi
(define list2 (cons 2.7183 (cons 3.14159 empty)))
; Liste mit den Zahlen 2 3 5 7
(define list4 (cons 2 (cons 3 (cons 5 (cons 7 empty)))))
; Liste mit den Zahlen 1 2 3 5 7
(define list5 (cons 1 list4))

; Summe der Elemente einer Liste von Zahlen berechnen
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list2) 5.85989)
(check-expect (list-sum list3) 6)
(check-expect (list-sum list4) 17)

#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list) ...))))
       
#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (list-sum (rest list))
       ...))))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list) (list-sum (rest list)))))))

; Produkt der Elemente einer Liste von Zahlen berechnen
(: list-product (list-of-numbers -> number))

(check-expect (list-product list1) 42)
(check-expect (list-product list3) 6)
(check-expect (list-product list4) 210)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list) (list-product (rest list)))))))


(define string-list (cons "Mike" (cons "Sperber" empty)))
