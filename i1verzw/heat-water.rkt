;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname heat-water) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Wassertemperatur nach Erhitzen berechnen, naiv
(: heat-water-0 (real real -> real))
                 
(check-expect (heat-water-0 -10 20) 10)
(check-expect (heat-water-0 10 20) 30)
(check-expect (heat-water-0 90 20) 110)

(define heat-water-0
  (lambda (temp heat)
    (+ temp heat)))

; Wassertemperatur nach Erhitzen berechnen, Sieden berücksichtigen
(: heat-water-1 (real real -> real))

(check-expect (heat-water-1 -10 20) 10)
(check-expect (heat-water-1 10 20) 30)
(check-expect (heat-water-1 90 20) 100)
(check-expect (heat-water-1 99 1) 100)
(check-expect (heat-water-1 99 2) 100)

(define heat-water-1
  (lambda (temp heat)
    (cond
      ((< (+ temp heat) 100) (+ temp heat))
      ((>= (+ temp heat) 100) 100))))

; Wassertemperatur nach Erhitzen berechnen, mit Eis & Sieden
(: heat-water (real real -> real))

(check-expect (heat-water -10 20) 0)
(check-expect (heat-water 10 20) 30)
(check-expect (heat-water 90 20) 100)
(check-expect (heat-water 99 1) 100)
(check-expect (heat-water 99 2) 100)
(check-expect (heat-water -10 5) -5)
(check-expect (heat-water -5 60) 0)
(check-expect (heat-water -5 90) 5)
(check-expect (heat-water -1 81) 0)
(check-expect (heat-water -1 82) 1)
(check-expect (heat-water -1 191) 100)
(check-error (heat-water 150 0))

#;(define heat-water
  (lambda (temp heat)
    (cond
      ; Die Anfangstemperatur ist unter 0°C, es wird also Eis erwärmt.
      ((< temp 0)
       (cond
         ; Die Erwärmung bleibt unter 0°C.
         ((< (+ temp heat) 0) (+ temp heat))
         ; Die Erwärmung bleibt bei  0°C "stecken"
         ((and (>= (+ temp heat) 0)
               (< (+ temp heat) 80))
          0)
         ; Die Erwärmung erhöht die Temperatur über den Nullpunkt hinaus.
         ((and (>= (+ temp heat) 0)
               (>= (+ temp heat) 80))
          (- (+ temp heat) 80))))
      ; Die Erwärmung würde die Wassertemperatur auf über 100°C erhöhen.
      ((>= (+ temp heat) 100) 100)
      ; Das Wasser fängt flüssig an und bleibt durch die Erwärmung flüssig.
      ((and (>= temp 0) (< (+ temp heat) 100))
       (+ temp heat)))))

#;(define heat-water
  (lambda (temp heat)
    (cond
      ; Die Anfangstemperatur ist unter 0°C, es wird also Eis erwärmt.
      ((< temp 0)
       (cond
         ; Die Erwärmung bleibt unter 0°C.
         ((< (+ temp heat) 0) (+ temp heat))
         ; Die Erwärmung bleibt bei  0°C "stecken"
         ((and (>= (+ temp heat) 0)
               (< (+ temp heat) 80))
          0)
         ; Die Erwärmung erhöht die Temperatur über den Nullpunkt hinaus.
         ((>= (+ temp heat) 80)
          (- (+ temp heat) 80))))
      ; Die Erwärmung würde die Wassertemperatur auf über 100°C erhöhen.
      ((>= (+ temp heat) 100) 100)
      ; Das Wasser fängt flüssig an und bleibt durch die Erwärmung flüssig.
      ((and (>= temp 0) (< (+ temp heat) 100))
       (+ temp heat)))))

#;(define heat-water
  (lambda (temp heat)
    (cond
      ; Die Anfangstemperatur ist unter 0°C, es wird also Eis erwärmt.
      ((< temp 0)
       (cond
         ; Die Erwärmung bleibt unter 0°C.
         ((< (+ temp heat) 0) (+ temp heat))
         ; Die Erwärmung bleibt bei  0°C "stecken"
         ((< (+ temp heat) 80) 0)
         ; Die Erwärmung erhöht die Temperatur über den Nullpunkt hinaus.
         (else
          (- (+ temp heat) 80))))
      ; Die Erwärmung würde die Wassertemperatur auf über 100°C erhöhen.
      ((>= (+ temp heat) 100) 100)
      ; Das Wasser fängt flüssig an und bleibt durch die Erwärmung flüssig.
      (else
       (+ temp heat)))))

#;(define heat-water
  (lambda (temp heat)
    (cond
      ; Die Anfangstemperatur ist unter 0°C, es wird also Eis erwärmt.
      ((< temp 0)
       (cond
         ; Die Erwärmung bleibt unter 0°C.
         ((< (+ temp heat) 0) (+ temp heat))
         ; Die Erwärmung bleibt bei  0°C "stecken"
         ((< (+ temp heat) 80) 0)
         ; Die Erwärmung würde die Temperatur auf über 100°C bringen
         ((>= (- (+ temp heat) 80) 100) 100)
         ; Die Erwärmung erhöht die Temperatur über den Nullpunkt hinaus.
         (else
          (- (+ temp heat) 80))))
      ; Die Erwärmung würde die Wassertemperatur auf über 100°C erhöhen.
      ((>= (+ temp heat) 100) 100)
      ; Das Wasser fängt flüssig an und bleibt durch die Erwärmung flüssig.
      (else
       (+ temp heat)))))

(define heat-water
  (lambda (temp heat)
    (cond
      ; Die Anfangstemperatur ist über 100°C, also unzulässig
      ((> temp 100)
       (violation "Anfangstemperatür über 100°C"))
      ; Die Anfangstemperatur ist unter 0°C, es wird also Eis erwärmt.
      ((< temp 0)
       (cond
         ; Die Erwärmung bleibt unter 0°C.
         ((< (+ temp heat) 0) (+ temp heat))
         ; Die Erwärmung bleibt bei  0°C "stecken"
         ((< (+ temp heat) 80) 0)
         ; Die Erwärmung würde die Temperatur auf über 100°C bringen
         ((>= (- (+ temp heat) 80) 100) 100)
         ; Die Erwärmung erhöht die Temperatur über den Nullpunkt hinaus.
         (else
          (- (+ temp heat) 80))))
      ; Die Erwärmung würde die Wassertemperatur auf über 100°C erhöhen.
      ((>= (+ temp heat) 100) 100)
      ; Das Wasser fängt flüssig an und bleibt durch die Erwärmung flüssig.
      (else
       (+ temp heat)))))

; Aus Wärme Temperatur berechnen
(: heat->temperature (real -> real))

(check-expect (heat->temperature -50) -50)
(check-expect (heat->temperature 0) 0)
(check-expect (heat->temperature 20) 0)
(check-expect (heat->temperature 80) 0)
(check-expect (heat->temperature 81) 1)
(check-expect (heat->temperature 180) 100)
(check-expect (heat->temperature 200) 100)                                            

(define heat->temperature
  (lambda (heat)
    (cond
      ((<= heat 0) heat)
      ((and (< 0 heat)
            (<= heat 80))
       0)
      ((and (< 80 heat)
            (<= heat 180))
       (- heat 80))
      ((< 180 heat) 100))))

; Aus Temperatur Wärme berechnen
(: temperature->heat (real -> real))

(check-expect (temperature->heat -20) -20)
(check-expect (temperature->heat -1) -1)
(check-expect (temperature->heat 1) 81)
(check-expect (temperature->heat 100) 180)

(define temperature->heat
  (lambda (temp)
    (cond
      ((< temp 0) temp)
      ((and (> temp 0)
            (<= temp 100))
       (+ temp 80)))))

; Wassertemperatur nach Erhitzen berechnen, mit Eis & Sieden
(: heat-water-2 (real real -> real))

(check-expect (heat-water-2 -10 20) 0)
(check-expect (heat-water-2 10 20) 30)
(check-expect (heat-water-2 90 20) 100)
(check-expect (heat-water-2 99 1) 100)
(check-expect (heat-water-2 99 2) 100)
(check-expect (heat-water-2 -10 5) -5)
(check-expect (heat-water-2 -5 60) 0)
(check-expect (heat-water-2 -5 90) 5)
(check-expect (heat-water-2 -1 81) 0)
(check-expect (heat-water-2 -1 82) 1)
(check-expect (heat-water-2 -1 191) 100)
(check-error (heat-water-2 150 0))

(define heat-water-2
  (lambda (temp heat)
    (heat->temperature
     (+ (temperature->heat temp) heat))))
