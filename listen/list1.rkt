;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname list1) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Länge einer Liste berechnen
(: list-length ((list-of %element) -> natural))

(check-expect (list-length empty) 0)
(check-expect (list-length (list 2 5 7)) 3)

(define list-length
  (lambda (lis)
    (cond
      ((empty? lis) 0)
      ((cons? lis) 
       (+ 1 
          (list-length (rest lis)))))))

; zwei Listen aneinanderhängen
(: concatenate ((list-of %element) (list-of %element) -> (list-of %element)))

(check-expect (concatenate (list 1 2 3) (list 4 5 6))
              (list 1 2 3 4 5 6))

(define concatenate
  (lambda (list1 list2)
    (cond
      ((empty? list1) list2)
      ((cons? list1) 
       (cons (first list1)
             (concatenate (rest list1) list2))))))
