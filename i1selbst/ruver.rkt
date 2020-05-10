;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname ruver) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Fluss kommt entweder aus:
; - einer Quelle
; - einem Hauptfluss und einem Nebenfluss

; Version 2:
; Ein Fluss ist eins der folgenden:
; - ein Bach aus einer Quelle
; - ein Zusammentreffen von einem Hauptfluss und einem Nebenfluss
(define river
  (signature (mixed stream confluence)))

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
(define-record stream
  make-stream
  stream?
  (stream-origin string))

; Ein Zusammentreffen besteht aus:
; - Ort
; - Hauptfluss
; - Nebenfluss
(define-record confluence
  make-confluence
  confluence?
  (confluence-location  string)
  (confluence-main-stem river)
  (confluence-tributary river))


(define eschach (make-stream "Heimliswald"))
(define prim (make-stream "Dreifaltigkeitsberg"))
(define neckar-1 (make-confluence "Rottweil" eschach prim))
(define schlichem (make-stream "Tieringen"))
(define neckar-2 (make-confluence "Epfendorf" neckar-1 schlichem))

; Fließt Fluss durch den angegebenen Ort?
(: flows-through? (river string -> boolean))

(check-expect (flows-through? eschach "Heimliswald") #t)
(check-expect (flows-through? eschach "Tübingen" #f)
(check-expect (flows-through? neckar-2 "Heimliswald") #t)
(check-expect (flows-through? neckar-2 "Berlin") #f)

#;(define flows-through?
  (lambda (river location)
    ...))

#;(define flows-through?
  (lambda (river location)
    (cond
      ((stream? river) ...)
      ((confluence? river) ...))))

#;(define flows-through?
  (lambda (river location)
    (cond
      ((stream? river)
       (if (string=? (stream-origin river) location)
           #t
           #f))
      ((confluence? river)
       ...))))

(define flows-through?
  (lambda (river location)
    (cond
      ((stream? river)
       (string=? (stream-origin river) location))
      ((confluence? river)
       (flows-through? (confluence-main-stre