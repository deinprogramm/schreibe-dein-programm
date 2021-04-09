;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname dillo-world) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp") (lib "universe.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp") (lib "universe.rkt" "teachpack" "deinprogramm" "sdp")))))
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

; Körper des Gürteltiers
(define dillo-body
  (overlay/xy (polygon
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
               "brown")
              0 -30
              (rectangle 100 60
                         "solid" "transparent")))

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

(define dead-eyes
  (overlay (line 10 10 "green")
           (line -10 10 "green")))

; Straßenseite
(define side
  (signature (enum "left" "right")))

; Eine Position auf der Straße besteht aus:
; - Straßenmeter (Abstand vom Straßenanfang in Meter)
; - Seite
(define-record position
  make-position
  position?
  (position-m-from-start real)
  (position-side side))

; Ein Gürteltier auf der Straße hat folgende Eigenschaften:
; - Gürteltier-Zustand
; - Position auf der Straße
(define-record dillo-on-road
  make-dillo-on-road
  dillo-on-road?
  (dillo-on-road-state dillo)
  (dillo-on-road-position position))

; Position des Autos
(: world-car-position (world -> position))

(check-expect (world-car-position (make-world 0 "left" empty 0))
              (make-position 0 "left"))
(check-expect (world-car-position (make-world 100 "right" empty 0))
              (make-position (ticks->meters 100) "right"))
                           
(define world-car-position
  (lambda (world)
    (make-position (ticks->meters (world-ticks world))
                   (world-car-side world))))

; Breite des Autos
(define car-width 1.5)
; Länge des Autos
(define car-length 3.0)


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
(: live-dillos-under-car-count (position (list-of dillo-on-road) -> natural))

(check-expect (live-dillos-under-car-count (make-position 10 "left")
                                            (list (make-dillo-on-road dillo1 (make-position 10 "left"))
                                                  (make-dillo-on-road dillo2 (make-position 10 "right"))
                                                  (make-dillo-on-road dillo2 (make-position 11 "left"))
                                                  (make-dillo-on-road dillo1 (make-position 9 "left"))
                                                  (make-dillo-on-road dillo1 (make-position 12 "left"))))
              2)

(define live-dillos-under-car-count
  (lambda (car-position dillos-on-road)
    (length
     (filter (lambda (dillo-on-road)
               (and (car-on-position? car-position (dillo-on-road-position dillo-on-road))
                    (dillo-alive? (dillo-on-road-state dillo-on-road))))
             dillos-on-road))))

; Alle Tiere überfahren, die das Auto berührt
(: run-over-dillos-on-road
   (position (list-of dillo-on-road) -> (list-of dillo-on-road)))

(define dead-dillo1 (run-over-dillo dillo1))

(check-expect
 (run-over-dillos-on-road
  (make-position 10 "left")
  (list (make-dillo-on-road dillo1 (make-position 10 "left"))
        (make-dillo-on-road dillo1 (make-position 10 "right"))
        (make-dillo-on-road dillo1 (make-position 11 "left"))
        (make-dillo-on-road dillo1 (make-position 9 "left"))
        (make-dillo-on-road dillo1 (make-position 12 "left"))))
 (list (make-dillo-on-road dead-dillo1 (make-position 10 "left"))
       (make-dillo-on-road dillo1 (make-position 10 "right"))
       (make-dillo-on-road dead-dillo1 (make-position 11 "left"))
       (make-dillo-on-road dead-dillo1 (make-position 9 "left"))
       (make-dillo-on-road dillo1 (make-position 12 "left"))))

(define run-over-dillos-on-road
  (lambda (car-position dillos-on-road)
    (map (lambda (dillo-on-road)
           (if (car-on-position?
                car-position
                (dillo-on-road-position dillo-on-road))
               (make-dillo-on-road
                (run-over-dillo (dillo-on-road-state dillo-on-road))
                (dillo-on-road-position dillo-on-road))
               dillo-on-road))
         dillos-on-road)))



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

(define marking-height 2) ; Höhe der Streifen
(define gap-height 1) ; Höhe der Lücken

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

(define road-window-height 12) ; Höhe des Straßenausschnitts

; Anzahl der nötigen Markierungen
(define marking-count
  (+ 1
     (quotient road-window-height
               (+ marking-height gap-height))))

; sichtbare Markierungen
(define visible-markings
  (markings marking-count))

(define road-width 5) ; Breite der Straße

; leerer Straßenausschnitt
(define blank-road-window
  (empty-scene (meters->pixels road-width)
               (meters->pixels road-window-height)
               "black"))

; Höhe eines Markierungssegments (Streifen und Lücke) in Pixeln
(define marking-segment-pixels
  (meters->pixels (+ marking-height gap-height)))

; Straßenausschnitt anzeigen
(: road-window (real -> image))

(define road-window
  (lambda (meters)
    (place-image/align visible-markings
                       (/ (image-width blank-road-window) 2)
                       (- (remainder (meters->pixels meters)
                                     marking-segment-pixels)
                          marking-segment-pixels)
                       "center" "top"
                       blank-road-window)))

; Meter pro Tick
(define meters-per-tick 0.1)

; Ticks in Meter umwandeln
(: ticks->meters (natural -> rational))

(define ticks->meters
  (lambda (ticks)
    (* meters-per-tick ticks)))


; Straßenausschnitt zu Zeitpunkt anzeigen
(: road-window-at-ticks (natural -> image))

(define road-window-at-ticks
  (lambda (ticks)
    (road-window (ticks->meters ticks))))
    
; Rad
(define wheel
  (rectangle (meters->pixels 0.2) (meters->pixels .5) "outline" "white"))

; Zwei Räder auf einer Seite des Autos
(define wheels-on-one-side
 (above wheel (rectangle 0 (meters->pixels 1.2) "solid" "black") wheel))

; Bild des Autos
(define car
  (beside
   wheels-on-one-side
   (rectangle (meters->pixels car-width) (meters->pixels car-length) "solid" "blue")
   wheels-on-one-side))

; Bild auf der Straße platzieren
(: place-image-on-road (image natural image position -> image))

(define place-image-on-road
  (lambda (road-image ticks image position)
    ; X-Koordinate der Mitte der Straße, in Pixeln
    (define middle-pixels (/ (image-width road-image) 2))
    ; X-Koordinate des Mittelpunkts des Bilds
    (define pixels-from-left
      (cond
        ((string=? (position-side position) "left")
         (* middle-pixels 0.5)) ; Mitte der linken Spur
        ((string=? (position-side position) "right")
         (* middle-pixels 1.5)))) ; Mitte der rechten Spur

    (place-image/align
     image
     pixels-from-left
     (- (image-height road-image)
        (meters->pixels (+ (- (position-m-from-start position)
                              (ticks->meters ticks))
                           (/ road-window-height 2))))
     "center" "center"
     road-image)))

; Auto auf die Straße setzen
(: place-car-on-road (natural position image -> image))
                      
(define place-car-on-road
  (lambda (ticks car-position road-image)
    (place-image-on-road road-image
                         ticks
                         car car-position)))


; Ein Tier auf die Straße malen
(: place-dillo-on-road (natural dillo-on-road image -> image))

(define place-dillo-on-road
  (lambda (ticks dillo-on-road road-image)
    (place-image-on-road road-image
                         ticks
                         (dillo-image (dillo-on-road-state dillo-on-road))
                         (dillo-on-road-position dillo-on-road))))

; Alle Tiere auf die Straße malen
(: place-dillos-on-road (natural (list-of dillo-on-road) image -> image))

(define place-dillos-on-road
  (lambda (ticks dillos-on-road road-image)
    (fold road-image
          (lambda (dillo-on-road image)
            (place-dillo-on-road ticks dillo-on-road image))
          dillos-on-road)))

; Punktzahl anzeigen
(: place-score (natural image -> image))

(define place-score
  (lambda (score image)
    (place-image/align
     (text (number->string score) 100 "blue")
     0 0
     "left" "top"
     image)))

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
  (world-dillos-on-road (list-of dillo-on-road))
  (world-score natural))

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
      (place-dillos-on-road
       ticks
       (world-dillos-on-road world)
       (road-window-at-ticks ticks))))))

; Vier Gürteltiere auf der Straße
(define dillos-on-road
  (list (make-dillo-on-road dillo1 (make-position 20 "left"))
        (make-dillo-on-road dillo2 (make-position 26 "right"))
        (make-dillo-on-road dillo3 (make-position 30 "left"))
        (make-dillo-on-road dillo4 (make-position 42 "left"))))

; Welt am Anfang, Auto steht links
(define initial-world
  (make-world 0 "left" dillos-on-road 0))

; Statische Welt zu einem bestimmten Zeitpunkt darstellen
(: world-at-ticks (natural -> world))

(define world-at-ticks
  (lambda (ticks)
    (make-world ticks "left" dillos-on-road 0)))

; Wie verändert sich die Welt, wenn ein Tick Zeit vergeht?
(: next-world (world -> world))

#;(define next-world
  (lambda (world)
    (make-world (+ 1 (world-ticks world))
                (world-car-side world)
                (world-dillos-on-road world)
                (world-score world))))

(define next-world
  (lambda (world)
    (define car-position (world-car-position world))
    (define dillos-on-road (world-dillos-on-road world))
    (make-world (+ 1 (world-ticks world))
                (world-car-side world)
                (run-over-dillos-on-road car-position dillos-on-road)
                (+ (live-dillos-under-car-count car-position dillos-on-road)
                   (world-score world)))))


; Auf Tastendruck reagieren
(: react-to-key (world string -> world))

(check-expect (react-to-key (make-world 12 "right" dillos-on-road 5) "left")
              (make-world 12 "left" dillos-on-road 5))

(check-expect (react-to-key (make-world 12 "left" dillos-on-road 5) "right")
              (make-world 12 "right" dillos-on-road 5))
              
(check-expect (react-to-key (make-world 12 "left" dillos-on-road 5) "a")
              (make-world 12 "left" dillos-on-road 5))

(define react-to-key
  (lambda (world key)
    (cond
      ((key=? key "left")
       (make-world (world-ticks world)
                   "left"
                   (world-dillos-on-road world)
                   (world-score world)))
      ((key=? key "right")
       (make-world (world-ticks world)
                   "right"
                   (world-dillos-on-road world)
                   (world-score world)))
      (else world))))


(big-bang initial-world
  (to-draw world->image)
  (on-tick next-world)
  (on-key react-to-key))
