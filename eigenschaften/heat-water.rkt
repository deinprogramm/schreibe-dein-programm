;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname heat-water) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; 3 Testfälle schlagen fehl

; Aus Wärme Temperatur berechnen
(: heat->temperature (real -> real))

(check-property
 (for-all ((heat real))
   (<= (heat->temperature heat) 100)))

(define heat->temperature
  (lambda (heat)
    (cond
      ((<= heat 0) heat)
      ((<= heat 80) 0)
      ((<= heat 180) (- heat 80))
      (else 100))))

; Aus Temperatur Wärme berechnen
(: temperature->heat (real -> real))

#;(check-property
 (for-all ((temp real))
   (==> (and (not (= temp 0))
             (<= temp 100))
        (or (< (temperature->heat temp) 0)
            (> (temperature->heat temp) 80)))))

(check-property
 (for-all ((heat real))
   (define temp (heat->temperature heat))
   (==> (and (not (= temp 0))
             (< temp 100))
        (= (temperature->heat temp)
           heat))))

(check-property
 (for-all ((temp real))
   (= (heat->temperature (temperature->heat temp))
      temp)))

(check-property
 (for-all ((temp real))
   (==> (not (= temp 0))
        (= (heat->temperature (temperature->heat temp))
           temp))))

(: within=? (real real real -> boolean))

(check-expect (within=? 1 2 0.5) #f)
(check-expect (within=? 1 2 1) #t)

(define within=?
  (lambda (a b epsilon)
    (<= (abs (- a b))
       epsilon)))

(check-property
 (for-all ((temp real))
   (==> (not (= temp 0))
        (<= (abs
             (- (heat->temperature (temperature->heat temp))
                temp))
            0.0000001))))

(check-property
 (for-all ((temp real))
   (==> (and (not (= temp 0))
             (< temp 100))
        (<= (abs
             (- (heat->temperature (temperature->heat temp))
                temp))
            0.0000001))))
   
(define temperature->heat
  (lambda (temp)
    (cond
      ((< temp 0) temp)
      ((and (> temp 0)
            (<= temp 100))
       (+ temp 80)))))

; Wassertemperatur nach Erhitzen berechnen, mit Eis & Sieden
(: heat-water (real real -> real))

(check-property
 (for-all ((temperature real)
           (heat real))
   (==> (and (not (= temperature 0))
             (<= temperature 100))
        (<= (heat-water temperature heat) 100))))
 
(define heat-water
  (lambda (temp heat)
    (heat->temperature
     (+ (temperature->heat temp) heat))))

