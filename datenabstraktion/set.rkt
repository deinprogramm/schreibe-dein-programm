#lang deinprogramm/sdp
; Eine Menge besteht aus:
; - Repräsentation
; - Methode, um Elemente einzufügen
; - Methode, um festzustellen, ob Wert Element in Menge ist
(define-record (set-of element)
  make-set
  set?
  (set-representation any)
  (set-insert-method ((set-of element) element -> (set-of element)))
  (set-member?-method ((set-of element) element -> boolean)))

; Element in Menge einfügen
(: set-insert ((set-of %a) %a -> (set-of %a)))

(define set-insert
  (lambda (set new-element)
    ((set-insert-method set) set new-element)))

; Feststellen, ob Wert Element einer Menge ist
(: set-member? ((set-of %a) %a -> boolean))

(define set-member?
  (lambda (set maybe-element)
    ((set-member?-method set) set maybe-element)))

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

(check-expect (set-member? list-set1 2) #t)
(check-expect (set-member? list-set1 5) #f)
(check-expect (set-member? list-set2 5) #t)

(define make-list-set
  (lambda (element-equal? list)
    (define insert
      (lambda (set new-element)
        (if (member? set new-element)
            set
            (set-update-representation
             set
             (lambda (list)
               (cons new-element list))))))
    (define member?
      (lambda (set maybe-element)
        (any? (lambda (element)
                       (element-equal? maybe-element element))
                     (set-representation set))))
        
    (make-set list
              insert member?)))

; Menge mit Elementen 1 2 3 4
(define list-set1
  (make-list-set = (list 1 2 3 4)))

; Menge mit Elementen 1 2 3 4 5
(define list-set2
  (set-insert list-set1 5))