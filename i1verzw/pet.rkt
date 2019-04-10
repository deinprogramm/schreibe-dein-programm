;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname pet) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Haustier ist eins der folgenden:
; - Katze
; - Hund
; - Schlange
(define pet
  (signature (one-of "Katze" "Hund" "Schlange")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "Katze") #t)
(check-expect (cute? "Hund") #t)
(check-expect (cute? "Schlange") #f)

(define cute?
  (lambda (p)
    (cond
      ((string=? p "Katze") #t)
      ((string=? p "Hund") #t)
      ((string=? p "Schlange") #f))))


; Haustier ggf. gegen ein niedliches umtauschen
(: exchange-for-cute (pet -> pet))

(check-expect (exchange-for-cute "Katze") "Katze")
(check-expect (exchange-for-cute "Hund") "Hund")
(check-expect (exchange-for-cute "Schlange") "Katze")

#;(define exchange-for-cute
  (lambda (p)
    (cond
      ((cute? p) p)
      (else "Katze"))))

(define exchange-for-cute
  (lambda (p)
    (if (cute? p)
        p
        "Katze")))