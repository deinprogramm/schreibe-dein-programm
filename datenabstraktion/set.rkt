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

; Feststellen, ob ein Element mit erfülltem Prädikat in Liste ist
(: any? ((%a -> boolean) (list-of %a) -> boolean))

(define any?
  (lambda (p? list)
    (cond
      ((empty? list) #f)
      ((cons? list)
       (or (p? (first list))
           (any? p? (rest list)))))))

; Menge mit Listenrepräsentation erzeugen
(: make-list-set ((%a %a -> boolean) (list-of %a) -> (set-of %a)))

(check-expect (set-member? 2 list-set1) #t)
(check-expect (set-member? 5 list-set1) #f)
(check-expect (set-member? 5 list-set2) #t)

(define make-list-set
  (lambda (element-equal? list)
    (define insert
      (lambda (value set)
        (if (member? value set)
            set
            (make-set (cons value list)
                      insert member?))))
    (define member?
      (lambda (value set)
        (any? (lambda (element)
                       (element-equal? value element))
                     (set-representation set))))
        
    (make-set listinsert member?)))

; Menge mit Elementen 1 2 3 4
(define list-set1
  (make-list-set = (list 1 2 3 4)))

; Menge mit Elementen 1 2 3 4 5
(define list-set2
  (set-insert 5 list-set1))