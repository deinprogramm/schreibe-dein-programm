#lang deinprogramm/sdp
; Eine Menge besteht aus:
; - Repräsentation
; - Methode, um Elemente einzufügen
; - Methode, um festzustellen, ob Wert Element in Menge ist
(define-record (set-of element)
  make-set
  set?
  (set-representation any)
  (set-insert-method (element (set-of element)-> (set-of element)))
  (set-member?-method (element (set-of element)-> boolean)))

; Element in Menge einfügen
(: set-insert (%a (set-of %a) -> (set-of %a)))

(define set-insert
  (lambda (value set)
    ((set-insert-method set) value set)))

; Feststellen, ob Wert Element einer Menge ist
(: set-member? (%a (set-of %a) -> boolean))

(define set-member?
  (lambda (value set)
    ((set-member?-method set) value set)))

; Repräsentation einer Menge aktualisieren
(: set-update-representation ((set-of %a) (any -> any) -> (set-of %a)))

(define set-update-representation
  (lambda (set update)
    (make-set (update (set-representation set))
              (set-insert-method set)
              (set-member?-method set))))

; ist Wert Element einer Liste?
(: member? ((%a %a -> boolean) %a (list-of %a) -> boolean))

(check-expect (member? = 5 empty) #f)
(check-expect (member? = 5 (list 1 2 3)) #f)
(check-expect (member? = 1 (list 1 2 3)) #t)
(check-expect (member? = 2 (list 1 2 3)) #t)
(check-expect (member? = 3 (list 1 2 3)) #t)
(check-expect (member? string=? "Slash" (list "Axl" "Slash")) #t)
(check-expect (member? string=? "Buckethead" (list "Axl" "Slash")) #f)

(define member?
  (lambda (equals? value list)
    (cond
      ((empty? list) #f)
      ((cons? list)
       (if (equals? value (first list))
           #t
           (member? equals? value (rest list)))))))

; Menge mit Listenrepräsentation erzeugen
(: make-list-set ((%a %a -> boolean) (list-of %a) -> (set-of %a)))

(check-expect (set-member? 2 list-set1) #t)
(check-expect (set-member? 5 list-set1) #f)
(check-expect (set-member? 5 list-set2) #t)

(define make-list-set
  (lambda (element-=? list)
    (define list-set-insert
      (lambda (value set)
        (if (list-set-member? value set)
            set
            (make-set (cons value list)
                      list-set-insert list-set-member?))))
    (define list-set-member?
      (lambda (value set)
        (member? element-=?
                 value
                 (set-representation set))))
        
    (make-set list list-set-insert list-set-member?)))

; Menge mit Elementen 1 2 3 4
(define list-set1
  (make-list-set = (list 1 2 3 4)))

; Menge mit Elementen 1 2 3 4 5
(define list-set2
  (set-insert 5 list-set1))