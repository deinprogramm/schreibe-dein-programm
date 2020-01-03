;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname zuckerampel) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Zuckeranteile bestehen aus:
; - Fruktose-Menge (in g)
; - Glukose-Menge (in g)
(define-record-functions sugars
  make-sugars
  sugars?
  (sugars-fructose-g rational)
  (sugars-glucose-g  rational))

(define sugars1 (make-sugars 1 1)) ; 1 g Fruktose, 1 g Glukose
(define sugars2 (make-sugars 2 3)) ; 2 g Fruktose, 3 g Glukose
(define sugars3 (make-sugars 5 5)) ; 5 g Fruktose, 5 g Glukose
(define sugars4 (make-sugars 10 2.5)) ; 10 g Fruktose, 2.5 g Glukose
(define sugars5 (make-sugars 10 13)) ; 10 g Fruktose, 13 g Glukose
(define sugars6 (make-sugars 15 10)) ; 15 g Fruktose, 10 g Glukose

; Eine Ampel ist einer der folgenden Werte:
; - rot
; - gelb
; - grün
(define traffic-light
  (signature
   (one-of "red" "yellow" "green")))

; Ein Zuckergehalt ist eins der folgenden:
; - Gewicht in Gramm
; - Zuckeranteile
; - Ampelbezeichnung
(define sugar-content
  (signature
   (mixed rational
          sugars
          traffic-light)))


; Ampelbezeichnung für Zuckergehalt ermitteln
(: sugar-traffic-light (sugar-content -> traffic-light))

(check-expect (sugar-traffic-light 2) "green")
(check-expect (sugar-traffic-light 5) "yellow")
(check-expect (sugar-traffic-light 10) "yellow")
(check-expect (sugar-traffic-light 12.5) "yellow")
(check-expect (sugar-traffic-light 23) "red")

(check-expect (sugar-traffic-light sugars1) "green")
(check-expect (sugar-traffic-light sugars2) "yellow")
(check-expect (sugar-traffic-light sugars3) "yellow")
(check-expect (sugar-traffic-light sugars4) "yellow")
(check-expect (sugar-traffic-light sugars5) "red")
(check-expect (sugar-traffic-light sugars6) "red")

(check-expect (sugar-traffic-light "green") "green")
(check-expect (sugar-traffic-light "yellow") "yellow")
(check-expect (sugar-traffic-light "red") "red")

(define sugar-traffic-light
  (lambda (sugar-content)
    (cond
      ((rational? sugar-content)
       (sugar-weight->traffic-light sugar-content))
      ((sugars? sugar-content)
       (sugar-weight->traffic-light (+ (sugars-fructose-g sugar-content)
                                       (sugars-glucose-g sugar-content))))
      ((string? sugar-content) sugar-content))))

; Zuckeranteil in g in Ampel umwandeln
(: sugar-weight->traffic-light (rational -> traffic-light))

(check-expect (sugar-weight->traffic-light 2) "green")
(check-expect (sugar-weight->traffic-light 5) "yellow")
(check-expect (sugar-weight->traffic-light 10) "yellow")
(check-expect (sugar-weight->traffic-light 12.5) "yellow")
(check-expect (sugar-weight->traffic-light 23) "red")

(define sugar-weight->traffic-light
  (lambda (sugar-weight)
    (cond
      ((< sugar-weight 5) "green")
      ((and (>= sugar-weight 5) (<= sugar-weight 22.5)) "yellow")
      ((> sugar-weight 12.5) "red"))))
