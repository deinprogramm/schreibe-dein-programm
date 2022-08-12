;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname generate) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(save-image (star-polygon 20 10 3 "outline" "blue")
            "starpolygon_outline.png")

(define s1 (square 40 "solid" "red"))
(define c1 (circle 40 "solid" "green"))
(define p1 (star-polygon 20 10 3 "solid" "blue"))


(save-image (beside s1 p1)
            "beside.png")
            

(save-image (above s1 c1)
            "above.png")
            

(save-image (above (beside s1 p1) (beside p1 c1))
            "abovebeside.png")


(save-image
(above
 (beside s1 p1)
 (beside p1 s1))
"tile1.png")

(save-image
(above
 (beside p1 c1)
 (beside c1 p1))
"tile2.png")

(define tile
  (lambda (a b)
    (above
     (beside a b)
     (beside b a))))

(tile s1 p1)
(tile p1 c1)