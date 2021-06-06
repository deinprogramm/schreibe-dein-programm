#lang deinprogramm/sdp/vanilla

; Eine Person hat folgende Eigenschaften:
; - Name
; - Elternteil #1
; - Elternteil #2
(define-record person
  make-person
  person?
  (person-name string)
  (person-parent-1 parent)
  (person-parent-2 parent))

; Ein Elternteil ist eins der folgenden:
; - eine Person
; - ein unbekannter Elternteil
(define parent
  (signature
   (mixed person unknown-parent)))

; Ein unbekannter Elternteil hat keine Eigenschaften
(define-record unknown-parent
  make-unknown-parent
  unknown-parent?)

(define an-unknown-parent (make-unknown-parent))

(define slash
  (make-person "Slash"
               (make-person "Ola Hudson"
                            an-unknown-parent
                            an-unknown-parent)
               (make-person "Anthony Hudson"
                            an-unknown-parent
                            an-unknown-parent)))
(define london-hudson
  (make-person "London Hudson"
               slash
               (make-person "Perla Ferrar"
                            an-unknown-parent
                            an-unknown-parent)))

; Ist jemand Vorfahr:in einer Person?
(: ancestor? (string person -> boolean))

(check-expect (ancestor? "Slash" london-hudson) #t)
(check-expect (ancestor? "Axl" london-hudson) #f)

#;(define ancestor?
    (lambda (name person)
      ...
      (person-name person)
      (person-parent-1 person)
      (person-parent-2 person)
      ...))
      

#;(define ancestor?
  (lambda (name person)
    (if (string=? name (person-name person))
        #t
        (if (or (parent-ancestor? name (person-parent-1 person))
                (parent-ancestor? name (person-parent-2 person)))
            #t
            #f))))
           
    
(define ancestor?
  (lambda (name person)
    (or (string=? name (person-name person))
        (parent-ancestor? name (person-parent-1 person))
        (parent-ancestor? name (person-parent-2 person)))))


; Ist jemand Vorfahr:in eines Elternteils?
(: parent-ancestor? (string parent -> boolean))

(define parent-ancestor?
  (lambda (name parent)
    (cond
      ((person? parent)
       (ancestor? name parent))
      ((unknown-parent? parent)
       #f))))

