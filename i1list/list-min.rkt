;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname list-min) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Kein Resultat
(define-record-functions no-result
  make-no-result
  no-result?)

; Minimum einer Liste von Zahlen berechnen
(: list-min ((list-of real) -> (mixed real no-result)))

(check-expect (list-min (list 5 3 1 4)) 1)
(check-expect (list-min (list 5 3 1 4 -4 3)) -4)
(check-expect (no-result? (list-min empty)) #t)

#;(define list-min
  (lambda (list)
    (cond
      ((empty? list) (make-no-result))
      ((cons? list)
       (if (no-result? (list-min (rest list)))
           (first list)
           (if (< (first list)
                  (list-min (rest list)))
               (first list)
               (list-min (rest list))))))))

(define list-min
  (lambda (list)
    (cond
      ((empty? list) (make-no-result))
      ((cons? list)
       (define rest-min (list-min (rest list)))
       (if (no-result? rest-min)
           (first list)
           (if (< (first list)
                  rest-min)
               (first list)
               rest-min))))))