;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname create-images) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(define flesh-out
  (lambda (image)
    (overlay image
             (rectangle (+ 1 (image-width image))
                        (+ 1 (image-height image))
                        0
                        "white"))))

(save-image (flesh-out (rectangle 100 30 "outline" "brown"))
            "rectangle.png")
(save-image (circle 50 "solid" "red")
            "circle.png")
(save-image (ellipse 50 100 "solid" "green")
            "ellipse.png")
(save-image (triangle 50 "solid" "gold")
            "triangle.png")
(save-image (line 150 100 "blue")
            "line1.png")
(save-image (line -150 100 "blue")
            "line2.png")
(save-image (text "Schreibe Dein Programm!" 20 "red")
            "text.png")
(save-image (polygon (list (make-posn 0 0)
                           (make-posn -20 40)
                           (make-posn 120 0)
                           (make-posn -20 -40))
                     "solid"
                     "plum")
            "polygon1.png")

(save-image (polygon (list (make-pulled-point 1/2 0 0 0 1/2 -20)
                           (make-posn -20 40)
                           (make-pulled-point 1/2 -20 120 0 1/2 20)
                           (make-posn -20 -40))
                     "solid"
                     "plum")
            "polygon2.png")
(save-image
 (overlay
  (circle 50 "solid" "gold")
  (rectangle 100 100 "solid" "blue"))
 "overlay.png")
(save-image
 (overlay/xy
  (circle 50 "solid" "gold")
  10 -20
  (rectangle 100 100 "solid" "blue"))
 "overlayxy.png")
(save-image
 (place-image
  (circle 50 "solid" "gold")
  40 70
  (rectangle 100 100 "solid" "blue"))
 "place-image.png")
(save-image
 (place-image/align
  (circle 20 "solid" "gold")
  40 70 "left" "center"
  (rectangle 100 100 "solid" "blue"))
 "place-image-align1.png")
(save-image
 (place-image/align
  (circle 20 "solid" "gold")
  40 70 "right" "center"
  (rectangle 100 100 "solid" "blue"))
 "place-image-align2.png")
(save-image
 (place-image/align
  (circle 20 "solid" "gold")
  40 70 "center" "center"
  (rectangle 100 100 "solid" "blue"))
 "place-image-align3.png")
(save-image
 (place-image/align
  (circle 20 "solid" "gold")
  40 70 "center" "top"
  (rectangle 100 100 "solid" "blue"))
 "place-image-align4.png")
(save-image
 (place-image/align
  (circle 20 "solid" "gold")
  40 70 "center" "bottom"
  (rectangle 100 100 "solid" "blue"))
 "place-image-align5.png")
(save-image
 (let ()
   (define c (circle 20 "solid" "green"))
   (beside (scale 0.5 c)
           c
           (scale 1 c)
           (scale 1.5 c)
           (scale 2 c)
           (scale 3 c)))
 "scale.png")
