#lang deinprogramm/sdp

; Ein Bandmitglied hat folgende Eigenschaften:
; - Name
; - Geburtsjahr
(define-record band-member
  make-band-member
  (band-member-name string)
  (band-member-born natural))

(define axl (make-band-member "Axl Rose" 1962))
(define duff (make-band-member "Duff McKagan" 1964))
(define slash (make-band-member "Slash" 1965))
(define dizzy (make-band-member "Dizzy Reed" 1963))
(define richard (make-band-member "Richard Fortus" 1966))
(define frank (make-band-member "Frank Ferrer" 1966))
(define melissa (make-band-member "Melissa Reese" 1990))

(define guns-n-roses
  (list axl duff slash dizzy richard frank melissa))

; Bandmitglied in eine sortierte Liste einfügen
(: insert (band-member (list-of band-member) -> (list-of band-member)))

(define insert
  (lambda (band-member list)
    (cond
      ((empty? list) (cons band-member empty))
      ((cons? list)
       (if (<= (band-member-born band-member)
               (band-member-born (first list)))
           (cons band-member list)
           (cons (first list)
                 (insert band-member (rest list))))))))

; Band nach Alter sortieren
(: sort-band ((list-of band-member) -> (list-of band-member)))

#;(check-expect (sort-band guns-n-roses)
              (list axl dizzy duff slash richard frank melissa))

(check-property
 (for-all ((list (list-of band-member)))
   (band-sorted? (sort-band list))))

(check-property
 (for-all ((list (list-of band-member)))
   (= (length (sort-band list))
      (length list))))
 
(define sort-band
  (lambda (list0)
    ; Invariante: list enthält die Bandmitglieder
    ; zwischen list0 und list, sortiert.
    (define accumulate     
      (lambda (list acc)
        (cond
          ((empty? list) acc)
          ((cons? list)
           (accumulate (rest list)
                       (insert (first list) acc))))))
    (accumulate list0 empty)))

#;(define sort-band
  (lambda (list0)
    empty))

#;(define sort-band
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (map (lambda (band-member)
              (first list))
            list)))))
     

; Band sortiert?
(: band-sorted? ((list-of band-member) -> boolean))

(check-expect (band-sorted? (list axl dizzy duff slash frank richard melissa))
              #t)
(check-expect (band-sorted? (list axl dizzy duff slash richard frank melissa))
              #t)
(check-expect (band-sorted? (list dizzy axl duff slash richard frank melissa))
              #f)
(check-expect (band-sorted? (list axl dizzy duff richard slash frank melissa))
              #f)

(define band-sorted?
  (lambda (list)
    (cond
      ((empty? list) #t)
      ((cons? list)
       (cond
         ((empty? (rest list)) #t)
         ((cons? (rest list))
          (and (<= (band-member-born (first list))
                   (band-member-born (first (rest list))))
               (band-sorted? (rest list)))))))))
