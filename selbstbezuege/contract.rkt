;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname contract) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Zero-Bond hat folgende Eigenschaften:
; - Datum
; - Betrag
; - Währung
#;(define-record zero-bond
  make-zero-bond
  (zero-bond-date     date)
  (zero-bond-amount   rational)
  (zero-bond-currency currency))

; Eine Währung ist eins der Folgenden:
; - Euro
; - britisches Pfund
; - Schweizer Franke
(define currency
  (signature (enum "EUR" "GPB" "CHF")))

(define contract
  (signature
   (mixed one-euro multiple later both nothing)))

; Ein Nichts-Vertrag hat keine Eigenschaften
(define-record nothing
  make-nothing
  nothing?)

; Ein Euro hat keine Eigenschaften
(define-record one-euro
  make-one-euro
  one-euro?)

(define-record one
  make-one
  one?
  (one-currency currency))

(define month (signature (integer-from-to 1 12)))

; Ein Datum besteht aus:
; - Jahr
; - Monat
; - Tag
(define-record date
  make-date
  date?
  (date-year  natural)
  (date-month month)
  (date-day   natural))

(define date1 (make-date 2019 07 26)) ; 26. Juli 2019
(define date2 (make-date 2019 12 24)) ; Weihnachten 2019

; Sind zwei Daten gleich?
(: date=? (date date -> boolean))

(check-expect (date=? date1 date1) #t)
(check-expect (date=? date1 date2) #f)
(check-expect (date=? date2 date1) #f)
(check-expect (date=? date2 date2) #t)

(define date=?
  (lambda (date1 date2)
    (and (= (date-year date1) (date-year date2))
         (= (date-month date1) (date-month date2))
         (= (date-day date1) (date-day date2)))))

; Ist ein Datum früher als ein anderes?
(: date<=? (date date -> boolean))

(check-expect (date<=? date1 date1) #t)
(check-expect (date<=? date1 date2) #t)
(check-expect (date<=? date2 date1) #f)
(check-expect (date<=? date2 date2) #t)

(define date3 (make-date 2020 1 1)) ; Neujahr 2020
(define date4 (make-date 2019 07 27)) ; 27. Juli 2019

(check-expect (date<=? date1 date3) #t)
(check-expect (date<=? date1 date4) #t)
(check-expect (date<=? date3 date1) #f)
(check-expect (date<=? date4 date1) #f)

#;(define date<=?
  (lambda (date1 date2)
    ...
    (date-year date1) (date-year date2)
    (date-month date1) (date-month date2)
    (date-day date1) (date-day date2)
    ...))
    
(define date<=?
  (lambda (date1 date2)
    (cond
      ((< (date-year date1) (date-year date2)) #t)
      ((> (date-year date1) (date-year date2)) #f)
      ((< (date-month date1) (date-month date2)) #t)
      ((> (date-month date1) (date-month date2)) #f)
      ((< (date-day date1) (date-day date2)) #t)
      ((> (date-day date1) (date-day date2)) #f)
      (else #t))))

; Mehrere haben folgende Eigenschaften:
; - wieviele
; - wovon

; Ein Vielfaches besteht aus:
; - Anzahl
; - Vertrag
#;(define-record multiple
    make-multiple
    multiple?
    (multiple-number rational)
    (multiple-of     contract))

(define-record multiple
  really-make-multiple
  multiple?
  (multiple-number   rational)
  (multiple-of       contract))

; multiple-Vertrag konstruieren, dabei vereinfachen
(: make-multiple (rational contract -> contract))

(check-expect (make-multiple 100 (make-nothing)) (make-nothing))

(define make-multiple
  (lambda (factor contract)
    (if (nothing? contract)
        (make-nothing)
        (really-make-multiple factor contract))))

; Eine Verzögerung besteht aus:
; - Datum
; - Vertrag, der zu dem Datum gültig wird
#;(define-record later
  make-later
  later?
  (later-date     date)
  (later-contract contract))

(define-record later
  really-make-later
  later?
  (later-date     date)
  (later-contract contract))

(define make-later really-make-later)

#;(define make-later
  (lambda (date contract)
    (if (nothing? contract)
        (make-nothing)
        (really-make-later date contract))))


; Eine Kombinationsvertrag besteht aus:
; - Vertrag Nr. 1
; - Vertrag Nr. 2
#;(define-record both
  make-both
  both?
  (both-contract-1 contract)
  (both-contract-2 contract))

(define-record both
  really-make-both
  both?
  (both-contract-1 contract)
  (both-contract-2 contract))

; Kombinationsvertrag konstruieren, dabei vereinfachen
(: make-both (contract contract -> contract))

(check-expect (make-both (make-nothing) (make-nothing)) (make-nothing))
(check-expect (make-both (make-one-euro) (make-nothing)) (make-one-euro))
(check-expect (make-both (make-nothing) (make-one-euro)) (make-one-euro))

(define make-both
  (lambda (contract-1 contract-2)
    (cond
      ((nothing? contract-1) contract-2)
      ((nothing? contract-2) contract-1)
      (else
       (really-make-both contract-1 contract-2)))))

(define euro100 (make-multiple 100 (make-one-euro))) ; 100 Euros

; Euro-Betrag auszahlen
(: make-euros (rational -> contract))

(check-expect (make-euros 100) euro100)

(define make-euros
  (lambda (amount)
    (make-multiple amount (make-one-euro))))

(define euro200 (make-euros 200)) ; 200 Euros

; Ich bekomme am 31. Juni 2030 1000 Euros.
(define zero1 (make-later (make-date 2030 06 31)
                          (make-euros 1000)))
; heute 1000 Euros und am 31. Juni 2030 nochmal
(define now-and-later (make-both (make-euros 1000)
                                 zero1))

; Ich bekomme am 24. Dezember 2040 1000 Euros
(define zero2 (make-later (make-date 2040 12 24)
                          (make-euros 2000)))


(define later1 (make-later date1 euro100))
(define later2 (make-later date2 euro200))

(define both1 (make-both euro100 later1))
(define both2 (make-both both1 later2))


; Für einen Vertrag berechnen, wieviel Geld wir bekommen

(: contract-payment (contract -> rational))

(check-expect (contract-payment euro100) 100)
(check-expect (contract-payment euro200) 200)
(check-expect (contract-payment later1) 100)
(check-expect (contract-payment later2) 200)
(check-expect (contract-payment both1) 200)
(check-expect (contract-payment both2) 400)

(define contract-payment
  (lambda (contract)
    (cond
      ((nothing? contract) 0)
      ((one-euro? contract) 1)
      ((multiple? contract)
       (* (multiple-number contract)
          (contract-payment (multiple-of contract))))
      ((later? contract)
       (contract-payment (later-contract contract)))
      ((both? contract)
       (+ (contract-payment (both-contract-1 contract))
          (contract-payment (both-contract-2 contract)))))))

; Was muss bis zu einem gegebenen Datum ausgezahlt werden?
(: contract-payment-until (contract date -> rational))

(check-expect (contract-payment-until euro100 date1) 100)
(check-expect (contract-payment-until euro200 date1) 200)
(check-expect (contract-payment-until later1 date1) 100)
(check-expect (contract-payment-until later2 date1) 0)
(check-expect (contract-payment-until later2 date2) 200)
(check-expect (contract-payment-until both1 date1) 200)
(check-expect (contract-payment-until both2 date2) 400)

(define contract-payment-until
  (lambda (contract date)
    (cond
      ((nothing? contract) 0)
      ((one-euro? contract) 1)
      ((multiple? contract)
       (* (multiple-number contract)
          (contract-payment-until (multiple-of contract) date)))
      ((later? contract)
       (if (date<=? (later-date contract) date)
           (contract-payment-until (later-contract contract) date)
           0))
      ((both? contract)
       (+ (contract-payment-until (both-contract-1 contract) date)
          (contract-payment-until (both-contract-2 contract) date))))))


; Was bleibt übrig, nachdem zu einem gegebenen Datum ausgezahlt wurde?

(: contract-rest (contract date -> contract))

(check-expect (contract-rest euro100 date1) (make-nothing))
(check-expect (contract-rest euro200 date1) (make-nothing))
(check-expect (contract-rest later1 date1) (make-nothing))
(check-expect (contract-rest later2 date1) later2)
(check-expect (contract-rest both1 date1) (make-nothing))
(check-expect (contract-rest both2 date1) later2)
(check-expect (contract-rest both2 date2) (make-nothing))

(define contract-rest
  (lambda (contract date)
    (cond
      ((nothing? contract) (make-nothing))
      ((one-euro? contract) (make-nothing))
      ((multiple? contract)
       (make-multiple (multiple-number contract)
                      (contract-rest (multiple-of contract) date)))
      ((later? contract)
       (if (date<=? (later-date contract) date)
           (contract-rest (later-contract contract) date)
           contract))
      ((both? contract)
       (make-both (contract-rest (both-contract-1 contract) date)
                  (contract-rest (both-contract-2 contract) date))))))
 
