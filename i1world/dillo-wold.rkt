;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname dillo-wold) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp") (lib "universe.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp") (lib "universe.rkt" "teachpack" "deinprogramm" "sdp")))))
; TODO: Nur Gürteltiere?

; Ein Gürteltier hat folgende Eigenschaften:
; - Gewicht (in g)
; - lebendig oder tot
(define-record dillo
  make-dillo
  dillo?
  (dillo-weight natural)
  (dillo-alive? boolean))

(: make-dillo (natural boolean -> dillo))
(: dillo-weight (dillo -> natural))
(: dillo-alive? (dillo -> boolean))

(define dillo1 (make-dillo 55000 #t)) ; 55 kg, lebendig 
(define dillo2 (make-dillo 58000 #f)) ; 58 kg, tot
(define dillo3 (make-dillo 60000 #t)) ; 60 kg, lebendig
(define dillo4 (make-dillo 63000 #f)) ; 63 kg, tot

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1) (make-dillo 55000 #f))
(check-expect (run-over-dillo dillo2) dillo2)
(check-expect (run-over-dillo dillo2) dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo (dillo-weight dillo)
                #f)))

; Ein Papagei hat folgende Eigenschaften:
; - Gewicht in Gramm
; - Satz, den er sagt
(define-record parrot
  make-parrot
  parrot?
  (parrot-weight   natural)
  (parrot-sentence string))

(: make-parrot (natural string -> parrot))
(: parrot? (any -> boolean))
(: parrot-weight (parrot -> natural))
(: parrot-sentence (parrot -> string))

(define parrot1 (make-parrot 10000 "Der Gärtner war's.")) ; 10kg, Miss Marple
(define parrot2 (make-parrot 5000 "Ich liebe Dich.")) ; 5kg, Romantiker 

(: parrot-alive? (parrot -> boolean))

(define parrot-alive?
  (lambda (parrot)
    (not (string=? (parrot-sentence parrot) ""))))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot (parrot-weight parrot)
                 "")))

; Ein Tier ist eins der folgenden:
; - Gürteltier
; - Papagei
(define animal
  (signature
    (mixed dillo parrot)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; Ist Tier noch am Leben?
(: animal-alive? (animal -> boolean))

(define animal-alive?
  (lambda (animal)
    (cond
      ((dillo? animal) (dillo-alive? animal))
      ((parrot? animal) (parrot-alive? animal)))))

; Videospiel

(define dillo-body
  (add-polygon (rectangle 100 60
                          "solid" "black")
               (list (make-pulled-point
                      0.3 30
                      5 (- 60 5)
                      0.2 5)
                     (make-pulled-point
                      0.4 20
                      20 (- 60 32)
                      0.5 -70)
                     (make-pulled-point
                      0.5 120 
                      90 (- 60 22)
                      0.2 -30)
                     (make-pulled-point
                      0.5 90
                      60 (- 60 10)
                      0.5 90)
                     (make-pulled-point
                      0 0 
                      90 (- 60 22)
                      0.2 -30)
                     (make-pulled-point
                      0 0
                      55 (- 60 20)
                      0.3 -30)
                     (make-pulled-point
                      0 0
                      29 (- 60 17)
                      0.5 20))
               "solid"
               "brown"))

(define dead-eyes
  (overlay (line 10 10 "green")
           (line -10 10 "green")))

; Gürteltier-Bild erzeugen

(: dillo-image (dillo -> image))

(define dillo-image
  (lambda (dillo)
    (scale
     (+ 1
        (/ (- (dillo-weight dillo) 50000)
           15000))
     (if (dillo-alive? dillo)
         dillo-body
         (overlay/xy dead-eyes
                     -25 -25
                     dillo-body)))))

; Papagei
; 0 is straight down, from there clockwise
; no. Relative to incoming angle?
; Or to incoming line?
(define parrot-image
  (lambda (parrot)
    (add-text-center
     (parrot-sentence parrot) 20 "dark red"
     (scale
      (/ (parrot-weight parrot)
         10000)
      (overlay/xy
       (polygon (list (make-pulled-point 0.5 0
                                         0 42
                                        0.5 -30)
                      (make-pulled-point 0.2 30
                                         40 0
                                         0.5 -45)
                      (make-pulled-point 0.5 0
                                         100 60
                                         0.4 -30)
                      (make-pulled-point 0.4 40
                                         110 200
                                         0.5 20)
                      (make-pulled-point 0.5 0
                                         100 280
                                         0.5 20)
                      (make-pulled-point 0.5 0
                                         60 200
                                         0.5 -20)
                      (make-pulled-point 0 0
                                         20 40
                                         0 0))
               
                "solid"
                "green")
       0 0
       (rectangle 130 280 "solid" "black"))))))

; Text in der Mitte eines Tiers abbilden
(: add-text-center (string natural image-color image -> image))

(define add-text-center
  (lambda (txt size color scene)
    (place-image/align
     (text txt size color)
     (/ (image-width scene) 2)
     (/ (image-height scene) 2)
     "center" "center"
     scene)))

; Tier abbilden
(: animal-image (animal -> image))

(define animal-image
  (lambda (animal)
    (cond
      ((dillo? animal) (dillo-image animal))
      ((parrot? animal) (parrot-image animal)))))

; Meter in Pixel umwandeln
(: meters->pixels (real -> real))

(define meters->pixels
  (lambda (meters)
    (* meters 100)))

; Pixel in Meter umwandeln
(: pixels->meters (real -> real))

(define pixels->meters
  (lambda (pixels)
    (/ pixels 100)))

(define marking-height 2)
(define gap-height 1)

; Straßenmarkierung mit bestimmter Anzahl von Streifen malen
(: markings (natural -> image))

(define markings
  (lambda (n)
    (cond
      ((zero? n) empty-image)
      ((positive? n)
       (above (rectangle (meters->pixels .20)
                         (meters->pixels marking-height)
                         "solid"
                         "white")
              (rectangle (meters->pixels .20)
                         (meters->pixels gap-height)
                         "solid"
                         "black")
              (markings (- n 1)))))))

; in meters
(define road-width 5)

; in meters
(define road-window-height 12)

(define blank-road-window
  (empty-scene (meters->pixels road-width)
               (meters->pixels road-window-height)
               "black"))

(define marking-count
  (+ 1
     (quotient road-window-height (+ marking-height gap-height))))

(define visible-markings
  (markings marking-count))

; Meter pro Tick
(define meters-per-tick 0.1)

; Ticks in Meter umwandeln
(define ticks->meters
  (lambda (ticks)
    (* meters-per-tick ticks)))

(define marking-segment-pixels
  (meters->pixels (+ marking-height gap-height)))

; Straßenausschnitt zu Zeitpunkt anzeigen
(: road-window (natural -> image))

(define road-window
  (lambda (ticks)
    (place-image/align visible-markings
                       (/ (image-width blank-road-window) 2)
                       (-
                        (remainder (meters->pixels (* ticks meters-per-tick))
                                   marking-segment-pixels)
                        marking-segment-pixels)
                       "center" "top"
                       blank-road-window)))


; Rad
(define wheel
  (rectangle (meters->pixels 0.2) (meters->pixels .5) "outline" "white"))

; Zwei Räder auf einer Seite des Autos
(define wheels-on-one-side
 (above wheel (rectangle 0 (meters->pixels 1.2) "solid" "black") wheel))

; Breite des Autos
(define car-width 1.5)
; Länge des Autos
(define car-length 3.0)

; Bild des Autos
(define car
  (beside
   wheels-on-one-side
   (rectangle (meters->pixels car-width) (meters->pixels car-length) "solid" "blue")
   wheels-on-one-side))

; Seite der Straße
(define side
  (signature (enum "left" "right")))

; Eine Position auf der Straße besteht aus:
; - Abstand vom Straßenanfang in Meter
; - Seite

(define-record position
  make-position
  position?
  (position-m-from-start real)
  (position-side side))

(define-record animal-on-road
  make-animal-on-road
  animal-on-road?
  (animal-on-road-animal animal)
  (animal-on-road-position position))

; Die Welt des Spiels besteht aus:
; - Ticks seit Spielanfang
; - Seite, auf der das Auto fährt
; - Tiere auf der Straße
; - Punktzahl
(define-record world
  make-world
  world?
  (world-ticks natural)
  (world-car-side side)
  (world-animals-on-road (list-of animal-on-road))
  (world-score natural))

; Position des Autos
(: world-car-position (world -> position))

(check-expect (world-car-position (make-world 0 "left" empty 0))
              (make-position (/ road-window-height 2)
                             "left"))
(check-expect (world-car-position (make-world 100 "left" empty 0))
              (make-position (+ (ticks->meters 100) (/ road-window-height 2))
                             "left"))

                           
(define world-car-position
  (lambda (world)
    (define meters (ticks->meters (world-ticks world)))
    (make-position (+ meters
                      (/ road-window-height 2))
                   (world-car-side world))))
    

(: place-image-on-road (image real image position -> image))

(define place-image-on-road
  (lambda (road-image road-bottom-m image position)
    (define image-m-from-start (position-m-from-start position))
    (define side (position-side position))
    (define road-top-m (+ road-bottom-m road-window-height))
    (define image-height-m (pixels->meters (image-height image)))
    (define middle-pixels (/ (image-width road-image) 2))
    (define pixels-from-left
      (cond
        ((string=? side "left")
         (* middle-pixels 0.5)) ; Mitte der linken Spur
        ((string=? side "right")
         (* middle-pixels 1.5)))) ; Mitte der rechten Spur
    (if (and (>= (+ image-m-from-start (/ image-height-m 2))
                 road-bottom-m)
             (<= (- image-m-from-start (/ image-height-m 2))
                 road-top-m))
        (place-image/align
         image
         pixels-from-left
         (- (image-height road-image)
            (meters->pixels (- image-m-from-start road-bottom-m)))
         "center" "center"
         road-image)
        road-image)))

; Auto auf die Straße setzen
(: place-car-on-road (natural position image -> image))
                      
(define place-car-on-road
  (lambda (ticks car-position road-image)
    (define meters (ticks->meters ticks))
    (place-image-on-road road-image
                         meters
                         car car-position)))

; Berührt das Auto eine Position?
(: car-on-position? (position position -> boolean))

(check-expect (car-on-position? (make-position 10 "left")
                                (make-position 10 "right"))
              #f)
(check-expect (car-on-position? (make-position 10 "left")
                                (make-position 10 "left"))
              #t)
(check-expect (car-on-position? (make-position 11 "left")
                                (make-position 10 "left"))
              #t)
(check-expect (car-on-position? (make-position 10 "left")
                                (make-position 11 "left"))
              #t)

(define car-on-position?
  (lambda (car-position position)
    (and (string=? (position-side car-position) (position-side position))
         (<= (abs (- (position-m-from-start car-position)
                     (position-m-from-start position)))
             (/ car-length 2)))))

; Wieviele Tiere werden vom Auto berührt?
(: live-animals-under-car-count (position (list-of animal-on-road) -> natural))

(check-expect (live-animals-under-car-count (make-position 10 "left")
                                            (list (make-animal-on-road dillo1 (make-position 10 "left"))
                                                  (make-animal-on-road dillo1 (make-position 10 "right"))
                                                  (make-animal-on-road dillo1 (make-position 11 "left"))
                                                  (make-animal-on-road dillo1 (make-position 9 "left"))
                                                  (make-animal-on-road dillo1 (make-position 12 "left"))))
              3)

(define live-animals-under-car-count
  (lambda (car-position animals-on-road)
    (length
     (filter (lambda (animal-on-road)
               (and (car-on-position? car-position (animal-on-road-position animal-on-road))
                    (animal-alive? (animal-on-road-animal animal-on-road))))
             animals-on-road))))

; Alle Tiere überfahren, die das Auto berührt
(: run-over-animals-on-road (position (list-of animal-on-road) -> (list-of animal-on-road)))

(define dead-dillo1 (run-over-dillo dillo1))

(check-expect (run-over-animals-on-road (make-position 10 "left")
                                        (list (make-animal-on-road dillo1 (make-position 10 "left"))
                                              (make-animal-on-road dillo1 (make-position 10 "right"))
                                              (make-animal-on-road dillo1 (make-position 11 "left"))
                                              (make-animal-on-road dillo1 (make-position 9 "left"))
                                              (make-animal-on-road dillo1 (make-position 12 "left"))))
              (list (make-animal-on-road dead-dillo1 (make-position 10 "left"))
                    (make-animal-on-road dillo1 (make-position 10 "right"))
                    (make-animal-on-road dead-dillo1 (make-position 11 "left"))
                    (make-animal-on-road dead-dillo1 (make-position 9 "left"))
                    (make-animal-on-road dillo1 (make-position 12 "left"))))

(define run-over-animals-on-road
  (lambda (car-position animals-on-road)
    (map (lambda (animal-on-road)
           (if (car-on-position? car-position
                                 (animal-on-road-position animal-on-road))
               (make-animal-on-road (run-over-animal (animal-on-road-animal animal-on-road))
                                    (animal-on-road-position animal-on-road))
               animal-on-road))
         animals-on-road)))

; Ein Tier auf die Straße malen
(: place-animal-on-road (natural animal-on-road image -> image))

(define place-animal-on-road
  (lambda (ticks animal-on-road road-image)
    (place-image-on-road road-image
                         (ticks->meters ticks)
                         (animal-image (animal-on-road-animal animal-on-road))
                         (animal-on-road-position animal-on-road))))

; Alle Tiere auf die Straße malen
(: place-animals-on-road (natural (list-of animal-on-road) image -> image))

(define place-animals-on-road
  (lambda (ticks animals-on-road road-image)
    (fold road-image
          (lambda (animal-on-road image)
            (place-animal-on-road ticks animal-on-road image))
          animals-on-road)))

; Punktzahl anzeigen
(: place-score (natural image -> image))

(define place-score
  (lambda (score image)
    (place-image/align
     (text (number->string score) 100 "blue")
     0 0
     "left" "top"
     image)))

; Spiel anzeigen
(: world->image (world -> image))

(define world->image
  (lambda (world)
    (define ticks (world-ticks world))
    (place-score
     (world-score world)
     (place-car-on-road
      ticks
      (world-car-position world)
      (place-animals-on-road
       ticks
       (world-animals-on-road world)
       (road-window ticks))))))

; Auf Tastendrück reagieren
(: react-to-key (world string -> world))

(define react-to-key
  (lambda (world key)
    (cond
      ((key=? key "left")
       (make-world (world-ticks world)
                   "left"
                   (world-animals-on-road world)
                   (world-score world)))
      ((key=? key "right")
       (make-world (world-ticks world)
                   "right"
                   (world-animals-on-road world)
                   (world-score world)))
      (else world))))

; Wie verändert sich die Welt, wenn ein Tick Zeit vergeht?
(: next-world (world -> world))

(define next-world
  (lambda (world)
    (define car-position (world-car-position world))
    (define animals-on-road (world-animals-on-road world))
    (make-world (+ 1 (world-ticks world))
                (world-car-side world)
                (run-over-animals-on-road car-position animals-on-road)
                (+ (live-animals-under-car-count car-position animals-on-road)
                   (world-score world)))))

(define animals-on-road
  (list (make-animal-on-road dillo1 (make-position 20 "left"))
        (make-animal-on-road parrot1 (make-position 26 "right"))
        (make-animal-on-road dillo2 (make-position 30 "left"))
        (make-animal-on-road parrot2 (make-position 42 "left"))))

#;(big-bang (make-world 0 "left" animals-on-road 0)
  (to-draw world->image)
  (on-tick next-world)
  (on-key react-to-key))
