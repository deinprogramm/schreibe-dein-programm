;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname dillo-wold) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp") (lib "universe.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp") (lib "universe.rkt" "teachpack" "deinprogramm" "sdp")))))
; TODO: Auto in der Mitte?

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

; Gürteltier mit 500g Futter füttern
(: feed-dillo (dillo -> dillo))

(check-expect (feed-dillo dillo1) (make-dillo 55500 #t))
(check-expect (feed-dillo dillo2) dillo2)

#;(define feed-dillo
  (lambda (dillo)
    (if (dillo-alive? dillo)
        (make-dillo (+ (dillo-weight dillo) 500)
                    #t)
        dillo)))

(define feed-dillo
  (lambda (dillo)
    (make-dillo (if (dillo-alive? dillo)
                    (+ (dillo-weight dillo) 500)
                    (dillo-weight dillo))
                (dillo-alive? dillo))))


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

; Gewicht eines Tiers feststellen
(: animal-weight (animal -> natural))

(check-expect (animal-weight dillo1) 55000)
(check-expect (animal-weight dillo2) 58000)
(check-expect (animal-weight parrot1) 10000)
(check-expect (animal-weight parrot2) 5000)

(define animal-weight
  (lambda (animal)
    (cond
      ((dillo? animal) (dillo-weight animal))
      ((parrot? animal) (parrot-weight animal)))))


; Papagei mit 50 g Futter füttern
(: feed-parrot (parrot -> parrot))

(check-expect (feed-parrot parrot1) (make-parrot 10050 "Der Gärtner war's."))
(check-expect (feed-parrot parrot2) (make-parrot 5050 "Ich liebe Dich."))

(define feed-parrot
  (lambda (parrot)
    (make-parrot (+ (parrot-weight parrot) 50)
                 (parrot-sentence parrot))))

; Tier füttern
(: feed-animal (animal -> animal))

(check-expect (feed-animal parrot1) (feed-parrot parrot1))
(check-expect (feed-animal parrot2) (feed-parrot parrot2))
(check-expect (feed-animal dillo1) (feed-dillo dillo1))
(check-expect (feed-animal dillo2) (feed-dillo dillo2))

(define feed-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (feed-dillo animal))
      ((parrot? animal) (feed-parrot animal)))))

(: run-over-animal (animal -> animal))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))



(: animal-alive? (animal -> boolean))

(define animal-alive?
  (lambda (animal)
    (cond
      ((dillo? animal) (dillo-alive? animal))
      ((parrot? animal) (parrot-alive? animal)))))

; Video-Spiel

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
  (overlay (flip-horizontal (line 10 10 "black"))
           (line 10 10 "black")))

(: dillo-image (dillo -> image))

(define dillo-image
  (lambda (dillo)
    (define live-dillo
       (overlay
        (add-curve dillo-body
                   92 (- 60 25) -90 0.5
                   60 (- 60 10) 0 0.5
                   (make-pen "brown" 4 "solid" "round" "round")) 
        
     
   
        (rectangle 120 70 "solid" "black")))

  (scale
   (+ 1
      (/ (- (dillo-weight dillo) 50000)
         15000))
   (if (dillo-alive? dillo)
       live-dillo
       (place-image dead-eyes
                    35 40
                    live-dillo)))))
        
      


(define body
  (add-solid-curve empty-image
                   0 0 60 0
                   80 0 -45 1
                   "brown"))
(define head
  (add-solid-curve empty-image
                   0 20 45 1
                   40 0 -20 0.5
                   "brown"))

(define tail
  (add-curve empty-image
             0 10 -10 1
             40 0 150 0.4
             (make-pen "brown" 5 "solid" "round" "round")))

(define body-with-tail
  (overlay/align/offset "right" "bottom" body 0 10 tail))

; frühe Version
#;(define dillo-image
  (overlay/align/offset "left" "bottom" head 30 0 body-with-tail))

(define snake-pen (make-pen "brown" 6 "solid" "round" "round"))

(define snake-link
  (add-curve
   (add-curve empty-image
              0 0 0 1
              0 20 180 1
              snake-pen)
   0 23 180 1
   0 40 0 1
   snake-pen))

(define snake
  (lambda (n)
    (if (= n 1)
        snake-link
        (overlay/align/offset "center"
                              "bottom"         
                              (snake (- n 1))
                              0
                              (- (image-height snake-link) 6)
                              snake-link))))

; Parrot
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
       (rectangle 130 280 "outline" "white"))))))


(: animal-image (animal -> image))

(define animal-image
  (lambda (animal)
    (cond
      ((dillo? animal) (dillo-image animal))
      ((parrot? animal) (parrot-image animal)))))

(: add-text-center (string natural image-color image -> image))

(define add-text-center
  (lambda (txt size color scene)
    (place-image/align
     (text txt size color)
     (/ (image-width scene) 2)
     (/ (image-height scene) 2)
     "center" "center"
     scene)))

(define meters->pixels
  (lambda (meters)
    (* meters 100)))

(define pixels->meters
  (lambda (pixels)
    (/ pixels 100)))

(define marking-height 2)
(define gap-height 1)

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

(define meters-per-tick 0.1)

(define ticks->meters
  (lambda (ticks)
    (* meters-per-tick ticks)))

(define marking-segment-pixels
  (meters->pixels (+ marking-height gap-height)))

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

    
(define wheel
  (rectangle (meters->pixels 0.2) (meters->pixels .5) "outline" "white"))

(define wheels-on-one-side
 (above wheel (rectangle 0 (meters->pixels 1.2) "solid" "black") wheel))

(define car-width 1.5)
(define car-length 3.0)

(define car
  (beside
   wheels-on-one-side
   (rectangle (meters->pixels car-width) (meters->pixels car-length) "solid" "blue")
   wheels-on-one-side))

(define side
  (signature (enum "left" "right")))

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

(define animals-on-road
  (list (make-animal-on-road dillo1 (make-position 20 "left"))
        (make-animal-on-road parrot1 (make-position 26 "right"))
        (make-animal-on-road dillo2 (make-position 30 "left"))
        (make-animal-on-road parrot2 (make-position 42 "left"))))
  
(define-record world
  make-world
  world?
  (world-ticks natural)
  (world-car-side side)
  (world-animals-on-road (list-of animal-on-road))
  (world-score natural))

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
         (* middle-pixels 1.5))))
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

(: place-car-on-road (natural side image -> image))
                      
(define place-car-on-road
  (lambda (ticks side road-image)
    (define meters (ticks->meters ticks))
    (place-image-on-road road-image
                         meters
                         car
                         (make-position (+ meters (/ car-length 2))
                                        side))))

(: car-on-position? (natural side position -> boolean))

(define car-on-position?
  (lambda (ticks car-side position)
    (and (string=? car-side (position-side position))
         (<= (abs (- (ticks->meters ticks)
                     (position-m-from-start position)))
             (/ car-length 2)))))

; FIXME: extract car position

(: live-animals-under-car-count (natural side (list-of animal-on-road) -> natural))

(define live-animals-under-car-count
  (lambda (ticks car-side animals-on-road)
    (length
     (filter (lambda (animal-on-road)
               (and (car-on-position? ticks car-side (animal-on-road-position animal-on-road))
                    (animal-alive? (animal-on-road-animal animal-on-road))))
             animals-on-road))))

(: run-over-animals-on-road (natural side (list-of animal-on-road) -> (list-of animal-on-road)))

(define run-over-animals-on-road
  (lambda (ticks car-side animals-on-road)
    (map (lambda (animal-on-road)
           (if (car-on-position? ticks car-side
                                 (animal-on-road-position animal-on-road))
               (make-animal-on-road (run-over-animal (animal-on-road-animal animal-on-road))
                                    (animal-on-road-position animal-on-road))
               animal-on-road))
         animals-on-road)))


(: car-on-position-count (natural side (list-of position) -> natural))

(define car-on-position-count
  (lambda (ticks car-side positions)
    (fold 0
          (lambda (position count)
            (if (car-on-position? ticks car-side position)
                (+ count 1)
                count))
          positions)))

(: place-animal-on-road (natural animal-on-road image -> image))

(define place-animal-on-road
  (lambda (ticks animal-on-road road-image)
    (place-image-on-road road-image
                         (ticks->meters ticks)
                         (animal-image (animal-on-road-animal animal-on-road))
                         (animal-on-road-position animal-on-road))))

(: place-animals-on-road (natural (list-of animal-on-road) image -> image))

(define place-animals-on-road
  (lambda (ticks animals-on-road road-image)
    (fold road-image
          (lambda (animal-on-road image)
            (place-animal-on-road ticks animal-on-road image))
          animals-on-road)))

(: place-score (natural image -> image))

(define place-score
  (lambda (score image)
    (place-image/align
     (text (number->string score) 100 "blue")
     0 0
     "left" "top"
     image)))

(: world->image (world -> image))

(define world->image
  (lambda (world)
    (define ticks (world-ticks world))
    (place-score
     (world-score world)
     (place-car-on-road
      ticks
      (world-car-side world)
      (place-animals-on-road
       ticks
       (world-animals-on-road world)
       (road-window ticks))))))

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

(define next-world
  (lambda (world)
    (define ticks (world-ticks world))
    (define car-side (world-car-side world))
    (define animals-on-road (world-animals-on-road world))
    (make-world (+ 1 ticks)
                car-side
                (run-over-animals-on-road ticks car-side animals-on-road)
                (+ (live-animals-under-car-count ticks car-side animals-on-road)
                   (world-score world)))))

#;(define display-game
  (lambda (ticks)
    (car-on-road
     (road-window ticks))))

(big-bang (make-world 0 "left" animals-on-road 0)
  (to-draw world->image)
  (on-tick next-world)
  (on-key react-to-key))
             

(define number-picture
  (lambda (number)
    (place-image/align
     (text (number->string number) 100 "blue")
     250 60
     "center" "center"
     (empty-scene 500 120))))



