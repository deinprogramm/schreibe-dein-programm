;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname river) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
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
(define-record-functions stream
  make-stream
  stream?
  (stream-origin string))

; Ein Zusammentreffen besteht aus:
; - Ort
; - Hauptfluss
; - Nebenfluss
(define-record-functions confluence
  make-confluence
  confluence?
  (confluence-location  string)
  (confluence-main-stem river)
  (confluence-tributary river))


(define eschach (make-stream "Heimliswald")) ; Quelle des Neckar
(define prim (make-stream "Dreifaltigkeitsberg")) ; Quelle des Neckar
; erster Zusammenfluss des Neckar:
(define neckar-1 (make-confluence "Rottweil" eschach prim))
; Zufluss des Neckar:
(define schlichem (make-stream "Tieringen")) 
; zweiter Zusammenfluss des Neckar:
(define neckar-2 (make-confluence "Epfendorf" neckar-1 schlichem))

; Fließt Fluss durch den angegebenen Ort?
(: flows-through? (river string -> boolean))

(check-expect (flows-through? eschach "Heimliswald") #t)
(check-expect (flows-through? eschach "Tübingen") #f)
(check-expect (flows-through? neckar-2 "Heimliswald") #t)
(check-expect (flows-through? neckar-2 "Rottweil") #t)
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

#;(define flows-through?
  (lambda (river location)
    (cond
      ((stream? river)
       (string=? (stream-origin river) location))
      ((confluence? river)
       ...
       (confluence-location river)
       (confluence-main-stem river)
       (confluence-tributary river)
       ...))))

#;(define flows-through?
  (lambda (river location)
    (cond
      ((stream? river)
       (string=? (stream-origin river) location))
      ((confluence? river)
       (if (string=? (confluence-location river) location)
           #t
           ...
           (confluence-main-stem river)
           (confluence-tributary river)
           ...)))))

#;(define flows-through?
  (lambda (river location)
    (cond
      ((stream? river)
       (string=? (stream-origin river) location))
      ((confluence? river)
       (if (string=? (confluence-location river) location)
           #t
           ...
           (flows-through? (confluence-main-stem river) location)
           (flows-through? (confluence-tributary river) location)
           ...)))))

(define flows-through?
  (lambda (river location)
    (cond
      ((stream? river)
       (string=? (stream-origin river) location))
      ((confluence? river)
       (if (string=? (confluence-location river) location)
           #t
           (or
            (flows-through? (confluence-main-stem river) location)
            (flows-through? (confluence-tributary river) location)))))))