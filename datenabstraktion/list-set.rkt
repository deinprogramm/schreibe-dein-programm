#lang deinprogramm/sdp
; Eine Listenmenge besteht aus:
; - Gleichheitsfunktion
; - einer Liste der Elemente
(define-record (list-set-of element)
  make-list-set
  list-set?
  (element-=?-function (element element -> boolean))
  (list-set-list (list-of element)))

; Menge mit den Elementen 1 2 3 4
(define list-set1
  (make-list-set = (list 1 2 3 4)))

; Feststellen, ob Wert Element einer Menge ist
(: list-set-member? (%a (list-set-of %a) -> boolean))

(check-expect (list-set-member? 2 list-set1) #t)
(check-expect (list-set-member? 5 list-set1) #f)

(define list-set-member?
  (lambda (value list-set)
    (member? (element-=?-function list-set)
             value (list-set-list list-set))))

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

; Element in Menge einfügen
(: list-set-insert (%a (list-set-of %a) -> (list-set-of %a)))

(check-expect (list-set-member? 5 (list-set-insert 5 list-set1)) #t)
(check-expect (list-set-member? 5 (list-set-insert 3 list-set1)) #f)

(define list-set-insert
  (lambda (value list-set)
    (if (list-set-member? value list-set)
        list-set
        (make-list-set (element-=?-function list-set)
                       (cons value (list-set-list list-set))))))

; Liefert eine Funktion für alle Elemente einer Liste #t?
(: every? ((%a -> boolean) (list-of %a) -> boolean))

(check-expect (every? even? (list 2 4 6)) #t)
(check-expect (every? positive? (list 1 2 3)) #t)
(check-expect (every? positive? (list 1 0 3)) #f)

(define every?
  (lambda (p? list)
    (cond
      ((empty? list) #t)
      ((cons? list)
       (and (p? (first list))
            (every? p? (rest list)))))))

; Aus allen Elementen einer Liste eine Listenmenge machen
(: list->list-set ((%a %a -> boolean)
                   (list-of %a) -> (list-set-of %a)))

(define list->list-set
  (lambda (= elements)
    (fold (make-list-set = empty)
          list-set-insert
          elements)))

(check-property
 (for-all ((elements (list-of natural)))
   (let ((list-set (list->list-set = elements)))
     (every? (lambda (element)
               (list-set-member? element list-set))
             elements))))

(check-property
 (for-all ((elements (list-of natural))
           (element natural))
   (==> (not (member? = element elements))
        (not (list-set-member? element (list->list-set = elements))))))


; Menge mit Slashs Freunden 2001
(define friends-of-slash-2001
  (list->list-set
   string=?
   "Duff McKagan"
   "Matt Sorum"
   "Dave Kushner"
   "Scott Weiland"))

; Ist jemand Freund von Slash im Jahr 2001?
(: friend-of-slash? (string -> boolean))

(check-expect (friend-of-slash? "Duff McKagan") #t)
(check-expect (friend-of-slash? "Buckethead") #f)

(define friend-of-slash?
  (lambda (person)
    (list-set-member? person friends-of-slash)))