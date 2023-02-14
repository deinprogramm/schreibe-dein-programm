#lang deinprogramm/sdp
(define-record (set-of element)
  make-set
  set?
  (element=?-function (element element -> boolean))
  (set-list (list-of element)))

(define list-set1
  (make-set = (list 1 2 3 4)))

; Feststellen, ob Wert Element einer Menge ist
(: set-member? ((set-of %a) %a -> boolean))

(check-expect (set-member? list-set1 2) #t)
(check-expect (set-member? list-set1 5) #f)

(define set-member?
  (lambda (set maybe-element)
    (any? (lambda (element)
            ((element=?-function set) element maybe-element))
          (set-list set))))

; Feststellen, ob ein Element mit erfülltem Prädikat in Liste ist
(: any? ((%a -> boolean) (list-of %a) -> boolean))

(define any?
  (lambda (p? list)
    (cond
      ((empty? list) #f)
      ((cons? list)
       (or (p? (first list))
           (any? p? (rest list)))))))


; Element in Menge einfügen
(: set-insert ((set-of %a) %a -> (set-of %a)))

(define set-insert
  (lambda (set new-element)
    (if (set-member? set new-element)
        set
        (make-set (element=?-function set)
                  (cons new-element (set-list set))))))

 
  
