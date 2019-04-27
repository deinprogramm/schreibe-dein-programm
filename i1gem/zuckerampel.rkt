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

(define s1 (make-sugars 1 1)) ; 1 g Fruktose, 1 g Glukose
(define s2 (make-sugars 2 3)) ; 2 g Fruktose, 3 g Glukose
(define s3 (make-sugars 5 5)) ; 5 g Fruktose, 5 g Glukose
(define s4 (make-sugars 10 2.5)) ; 10 g Fruktose, 2.5 g Glukose
(define s5 (make-sugars 10 3)) ; 10 g Fruktose, 3 g Glukose
(define s6 (make-sugars 15 10)) ; 15 g Fruktose, 10 g Glukose

; Eine Ampel ist einer der folgenden Werte:
; - rot
; - gelb
; - grün
(define traffic-light
  (signature
   (one-of "rot" "gelb" "grün")))

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

(check-expect (sugar-traffic-light 2) "grün")
(check-expect (sugar-traffic-light 5) "gelb")
(check-expect (sugar-traffic-light 10) "gelb")
(check-expect (sugar-traffic-light 12.5) "gelb")
(check-expect (sugar-traffic-light 20) "rot")

(check-expect (sugar-traffic-light s1) "grün")
(check-expect (sugar-traffic-light s2) "gelb")
(check-expect (sugar-traffic-light s3) "gelb")
(check-expect (sugar-traffic-light s4) "gelb")
(check-expect (sugar-traffic-light s5) "rot")
(check-expect (sugar-traffic-light s6) "rot")

(check-expect (sugar-traffic-light "grün") "grün")
(check-expect (sugar-traffic-light "gelb") "gelb")
(check-expect (sugar-traffic-light "rot") "rot")

(define sugar-traffic-light
  (lambda (f)
    (cond
      ((rational? f)
       (sugar-weight->traffic-light f))
      ((sugars? f)
       (sugar-weight->traffic-light (+ (sugars-fructose-g f)
                                       (sugars-glucose-g f))))
      ((string? f) f))))

; Zuckeranteil in g in Ampel umwandeln
(: sugar-weight->traffic-light (rational -> traffic-light))

(check-expect (sugar-weight->traffic-light 2) "grün")
(check-expect (sugar-weight->traffic-light 5) "gelb")
(check-expect (sugar-weight->traffic-light 10) "gelb")
(check-expect (sugar-weight->traffic-light 12.5) "gelb")
(check-expect (sugar-weight->traffic-light 20) "rot")

(define sugar-weight->traffic-light
  (lambda (w)
    (cond
      ((< w 5) "grün")
      ((and (>= w 5) (<= w 12.5)) "gelb")
      ((> w 12.5) "rot"))))
