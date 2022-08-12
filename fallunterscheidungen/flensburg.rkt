;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname flensburg) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Fallunterscheidungen und Verzweigungen

; Punkte in Flensburg

;0 bis 3 Punkte
;Keine Sanktionen
;4 bis 8 Punkte
;Bei freiwilliger Teilnahme an Aufbauseminaren: 4 Punkte Abzug
;8 bis 13 Punkte
;Verwarnung und Hinweis auf freiwilliges Aufbauseminar
;9 bis 13 Punkte
;Bei freiwilliger Teilnahme an Aufbauseminaren: 2 Punkte Abzug
;14 bis 17 Punkte
;Teilnahme an Aufbauseminar wird angeordnet
;14 bis 17 Punkte
;Bei freiwilliger Teilnahme an verkehrspsychologischer Beratung: 2 Punkte Abzug
;Ab 18 Punkte
;Führerschein wird entzogen

; Eine Zwangsmaßnahme ist:
; - "nichts"
; - "Aufbauseminar"
; - "Führerscheinentzug"
(define must-do
  (signature
   (enum "nichts"
         "Aufbauseminar"
         "Führerscheinentzug")))

; Zwangsmaßnahme bei Flensburg-Punktestand errechnen
(: points-must-do (natural -> must-do))

(check-expect (points-must-do 0) "nichts")
(check-expect (points-must-do 13) "nichts")
(check-expect (points-must-do 14) "Aufbauseminar")
(check-expect (points-must-do 17) "Aufbauseminar")
(check-expect (points-must-do 18) "Führerscheinentzug")
(check-expect (points-must-do 100) "Führerscheinentzug")
               
(define points-must-do
  (lambda (p)
    (cond
      ((<= p 13) "nichts")
      ((and (>= p 14) (<= p 17)) "Aufbauseminar")
      ((>= p 18) "Führerscheinentzug"))))

; verkürzte Fassung, aber abhängig von der Reihenfolge der Zweige:
;(define points-must-do
;  (lambda (p)
;    (cond
;      ((<= p 13) "nichts")
;      ((<= p 17) "Aufbauseminar")
;      (else "Führerscheinentzug"))))

(define action
  (signature
   (enum "nichts"
         "Aufbauseminar"
         "verkehrspsychologische Beratung"
         "Führerscheinentzug")))

; Punktestand in Flensburg senken
(: improve-points (natural action -> natural)) 

(check-expect (improve-points 3 "Aufbauseminar") 3)
(check-expect (improve-points 4 "nichts") 4)
(check-expect (improve-points 4 "Aufbauseminar") 0)
(check-expect (improve-points 8 "Aufbauseminar") 4)
(check-expect (improve-points 9 "Aufbauseminar") 7)
(check-expect (improve-points 13 "Aufbauseminar") 11)
(check-expect (improve-points 14 "verkehrspsychologische Beratung") 12)
(check-expect (improve-points 17 "verkehrspsychologische Beratung") 15)
(check-expect (improve-points 18 "Aufbauseminar") 18)
(check-expect (improve-points 18 "verkehrspsychologische Beratung") 18)
; (Es fehlen noch Testfälle für volle Abdeckung.)

(define improve-points
  (lambda (p a)
    (cond
      ((<= p 3) p)
      ((and (>= p 4) (<= p 8))
       (if (string=? a "Aufbauseminar")
           (- p 4)
           p))
      ((and (>= p 9) (<= p 13))
       (if (string=? a "Aufbauseminar")
           (- p 2)
           p))
      ((and (>= p 14) (<= p 17))
       (if (string=? a "verkehrspsychologische Beratung")
           (- p 2)
           p))
      ((>= p 18) p))))

#;(define improve-points
  (lambda (p a)
    (cond
      ((string=? a "Aufbauseminar")
       (cond
         ((and (>= p 4) (<= p 8))
          (- p 4))
         ((and (>= p 9) (<= p 13))
          (- p 2))
         (else p)))
      ((string=? a "verkehrspsychologische Beratung")
       (cond
         ((and (>= p 14) (<= p 17))
          (- p 2))
         (else p)))
      (else p))))

; Anzahl der PKWs aus Anzahl der Fahrzeuge & Räder berechnen
; (Kurzbeschreibung: 1 Zeile)

; Signatur:
(: parking-lot-cars (natural natural -> natural))

(check-expect (parking-lot-cars 2 6) ; 2 Fahrzeuge, 6 Räder
              1) ; 1 PKW
(check-expect (parking-lot-cars 2 8) ; 2 Fahrzeuge, 8 Räder
              2) ; 2 PKW
(check-expect (parking-lot-cars 3 10) ; 3 Fahrzeuge, 10 Räder
              2) ; 2 PKW

(check-error (parking-lot-cars 10 10)) ; 10 Fahrzeuge, 10 Räder
(check-error (parking-lot-cars 3 9)) ; 3 Fahrzeuge, 9 Räder
(check-error (parking-lot-cars 2 10)) ; 2 Fahrzeuge, 10 Räder

(define parking-lot-cars
  (lambda (vehicle-count wheel-count)
    (if (and (even? wheel-count)
             (<= (* 2 vehicle-count) wheel-count)
             (<= wheel-count (* 4 vehicle-count)))
        (/ (- wheel-count (* 2 vehicle-count))
           2)
        (violation "unsinnige Daten"))))
    


; aus einer Temperatur den Aggregatzustand von Wasser bestimmen
(: water-phase (real -> (enum "frozen" "liquid" "gaseous")))
(check-expect (water-phase -10) "frozen")
(check-expect (water-phase 0) "frozen")
(check-expect (water-phase 10) "liquid")
(check-expect (water-phase 100) "gaseous")

(define water-phase
  (lambda (T)
    (cond
      ((<= T 0) "frozen")
      ((and (> T 0) (< T 100)) "liquid")
      ((>= T 100) "gaseous"))))

; kürzer, aber abhängig von der Reihenfolge der Zweige:

;(define water-phase
;  (lambda (T)
;    (cond
;      ((<= T 0) "frozen")
;      ((< T 100) "liquid")
;      (else "gaseous"))))

; Absolutbetrag einer Zahl berechnen
(: absolute (number -> number))
; (define absolute
;   (lambda (x)
;     (cond
;      ((>= x 0) x)
;      (else (- x)))))
(check-expect (absolute 0) 0)
(check-expect (absolute -5) 5)
(check-expect (absolute 7) 7)

(define absolute
  (lambda (x)
    (if (>= x 0)
        x
        (- x))))

; feststellen, ob Temperatur mild ist
(: temperature-mild? (number -> boolean))
; (define temperature-mild?
;   (lambda (T)
;     (if (>= T 4)
;         (if (<= T 12)
;             #t
;             #f)
;         #f)))

; (define temperature-mild?
;   (lambda (T)
;     (if (and (>= T 4) (<= T 12))
;         #t
;         #f)))
(check-expect (temperature-mild? 10) #t)
(check-expect (temperature-mild? 14) #f)
(check-expect (temperature-mild? -1) #f)
(check-expect (temperature-mild? 4) #t)

(define temperature-mild?
  (lambda (T)
    (and (>= T 4) (<= T 12))))

; feststellen, ob Temperatur unangenehm ist
(: temperature-uncomfortable? (number -> boolean))
; (define temperature-uncomfortable?
;   (lambda (T)
;     (or (< T 14) (> T 34))))
(check-expect (temperature-uncomfortable? 23) #f)
(check-expect (temperature-uncomfortable? 23.5) #t)

(define temperature-uncomfortable?
  (lambda (T)
    (not (= T 23))))
