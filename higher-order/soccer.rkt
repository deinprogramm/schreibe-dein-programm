;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname soccer) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))

; Eine Mannschaft ist durch seinen Namen identifiziert.
(define team (signature string))

; Ein Spiel hat folgende Eigenschaften:
; - Spieltag
; - Heimmannschaft
; - Heimmannschaft-Tore
; - Gastmannschaft
; - Gastmannschaft-Tore
(define-record game
  make-game game?
  (game-matchday natural)
  (game-home-team team)
  (game-home-goals natural)
  (game-guest-team team)
  (game-guest-goals natural))

; Spielzeit 2009/2010

; Spiele am ersten Tag
(define game1 (make-game 1 "Wolfsburg" 2 "Stuttgart" 0))
(define game2 (make-game 1 "Mainz" 2 "Bayer 04" 2))
(define game3 (make-game 1 "Hertha" 1 "Hannover" 0))
(define game4 (make-game 1 "Bremen" 2 "Frankfurt" 3))
(define game5 (make-game 1 "Nürnberg" 1 "Schalke" 2))
(define game6 (make-game 1 "Dortmund" 1 "1. FC Köln" 0))
(define game7 (make-game 1 "Hoffenheim" 1 "Bayern" 1))
(define game8 (make-game 1 "Bochum" 3 "Gladbach" 3))
(define game9 (make-game 1 "Freiburg" 1 "Hamburg" 1))

(define day1
  (list game1 game2 game3 game4 game5 game6 game7 game8 game9))

; Komplette Spielzeit
(define season-2009/2010
  (list
   (make-game 1 "Wolfsburg" 2 "Stuttgart" 0)
   (make-game 1 "Mainz" 2 "Bayer 04" 2)
   (make-game 1 "Hertha" 1 "Hannover" 0)
   (make-game 1 "Bremen" 2 "Frankfurt" 3)
   (make-game 1 "Nürnberg" 1 "Schalke" 2)
   (make-game 1 "Dortmund" 1 "1. FC Köln" 0)
   (make-game 1 "Hoffenheim" 1 "Bayern" 1)
   (make-game 1 "Bochum" 3 "Gladbach" 3)
   (make-game 1 "Freiburg" 1 "Hamburg" 1)
   
   (make-game 2 "Frankfurt" 1 "Nürnberg" 1)
   (make-game 2 "Hannover" 1 "Mainz" 1)
   (make-game 2 "Bayer 04" 1 "Hoffenheim" 0)
   (make-game 2 "Hamburg" 4 "Dortmund" 1)
   (make-game 2 "Stuttgart" 4 "Freiburg" 2)
   (make-game 2 "Bayern" 1 "Bremen" 1)
   (make-game 2 "1. FC Köln" 1 "Wolfsburg" 3)
   (make-game 2 "Gladbach" 2 "Hertha" 1)
   (make-game 2 "Schalke" 3 "Bochum" 0)
   
   (make-game 3 "Hoffenheim" 0 "Schalke" 0)
   (make-game 3 "Nürnberg" 0 "Hannover" 2)
   (make-game 3 "Mainz" 2 "Bayern" 1)
   (make-game 3 "Freiburg" 0 "Bayer 04" 5)
   (make-game 3 "1. FC Köln" 0 "Frankfurt" 0)
   (make-game 3 "Dortmund" 1 "Stuttgart" 1)
   (make-game 3 "Bochum" 1 "Hertha" 0)
   (make-game 3 "Bremen" 3 "Gladbach" 0)
   (make-game 3 "Wolfsburg" 2 "Hamburg" 4)
   
   (make-game 4 "Gladbach" 2 "Mainz" 0)
   (make-game 4 "Frankfurt" 1 "Dortmund" 1)
   (make-game 4 "Hannover" 0 "Hoffenheim" 1)
   (make-game 4 "Bayer 04" 2 "Bochum" 1)
   (make-game 4 "Schalke" 0 "Freiburg" 1)
   (make-game 4 "Stuttgart" 0 "Nürnberg" 0)
   (make-game 4 "Bayern" 3 "Wolfsburg" 0)
   (make-game 4 "Hertha" 2 "Bremen" 3)
   (make-game 4 "Hamburg" 3 "1. FC Köln" 1)
   
   (make-game 5 "Nürnberg" 1 "Gladbach" 0)
   (make-game 5 "Mainz" 2 "Hertha" 1)
   (make-game 5 "Freiburg" 0 "Frankfurt" 2)
   (make-game 5 "Hoffenheim" 3 "Bochum" 0)
   (make-game 5 "Dortmund" 1 "Bayern" 5)
   (make-game 5 "Wolfsburg" 2 "Bayer 04" 3)
   (make-game 5 "Hamburg" 3 "Stuttgart" 1)
   (make-game 5 "Bremen" 0 "Hannover" 0)
   (make-game 5 "1. FC Köln" 1 "Schalke" 2)
   
   (make-game 6 "Schalke" 1 "Wolfsburg" 2)
   (make-game 6 "Hannover" 1 "Dortmund" 1)
   (make-game 6 "Bochum" 2 "Mainz" 3)
   (make-game 6 "Gladbach" 2 "Hoffenheim" 4)
   (make-game 6 "Stuttgart" 0 "1. FC Köln" 2)
   (make-game 6 "Bayern" 2 "Nürnberg" 1)
   (make-game 6 "Frankfurt" 1 "Hamburg" 1)
   (make-game 6 "Bayer 04" 0 "Bremen" 0)
   (make-game 6 "Hertha" 0 "Freiburg" 4)
   
   (make-game 7 "Nürnberg" 0 "Bochum" 1)
   (make-game 7 "Frankfurt" 0 "Stuttgart" 3)
   (make-game 7 "1. FC Köln" 0 "Bayer 04" 1)
   (make-game 7 "Bremen" 3 "Mainz" 0)
   (make-game 7 "Dortmund" 0 "Schalke" 1)
   (make-game 7 "Wolfsburg" 4 "Hannover" 2)
   (make-game 7 "Hamburg" 1 "Bayern" 0)
   (make-game 7 "Freiburg" 3 "Gladbach" 0)
   (make-game 7 "Hoffenheim" 5 "Hertha" 1)
   
   (make-game 8 "Schalke" 2 "Frankfurt" 0)
   (make-game 8 "Mainz" 2 "Hoffenheim" 1)
   (make-game 8 "Bochum" 1 "Wolfsburg" 1)
   (make-game 8 "Hannover" 5 "Freiburg" 2)
   (make-game 8 "Bayer 04" 4 "Nürnberg" 0)
   (make-game 8 "Bayern" 0 "1. FC Köln" 0)
   (make-game 8 "Gladbach" 0 "Dortmund" 1)
   (make-game 8 "Stuttgart" 0 "Bremen" 2)
   (make-game 8 "Hertha" 1 "Hamburg" 3)
   
   (make-game 9 "Nürnberg" 3 "Hertha" 0)
   (make-game 9 "Freiburg" 1 "Bayern" 2)
   (make-game 9 "Frankfurt" 2 "Hannover" 1)
   (make-game 9 "1. FC Köln" 1 "Mainz" 0)
   (make-game 9 "Bremen" 2 "Hoffenheim" 0)
   (make-game 9 "Stuttgart" 1 "Schalke" 2)
   (make-game 9 "Hamburg" 0 "Bayer 04" 0)
   (make-game 9 "Wolfsburg" 2 "Gladbach" 1)
   (make-game 9 "Dortmund" 2 "Bochum" 0)
   
   (make-game 10 "Bayer 04" 1 "Dortmund" 1)
   (make-game 10 "Hoffenheim" 3 "Nürnberg" 0)
   (make-game 10 "Mainz" 3 "Freiburg" 0)
   (make-game 10 "Bayern" 2 "Frankfurt" 1)
   (make-game 10 "Hannover" 1 "Stuttgart" 0)
   (make-game 10 "Gladbach" 0 "1. FC Köln" 0)
   (make-game 10 "Hertha" 0 "Wolfsburg" 0)
   (make-game 10 "Bochum" 1 "Bremen" 4)
   (make-game 10 "Schalke" 3 "Hamburg" 3)
   
   (make-game 11 "Dortmund" 2 "Hertha" 0)
   (make-game 11 "Nürnberg" 2 "Bremen" 2)
   (make-game 11 "1. FC Köln" 0 "Hannover" 1)
   (make-game 11 "Hamburg" 2 "Gladbach" 3)
   (make-game 11 "Wolfsburg" 3 "Mainz" 3)
   (make-game 11 "Stuttgart" 0 "Bayern" 0)
   (make-game 11 "Schalke" 2 "Bayer 04" 2)
   (make-game 11 "Freiburg" 0 "Hoffenheim" 1)
   (make-game 11 "Frankfurt" 2 "Bochum" 1)
   
   (make-game 12 "Bayer 04" 4 "Frankfurt" 0)
   (make-game 12 "Mainz" 1 "Nürnberg" 0)
   (make-game 12 "Gladbach" 0 "Stuttgart" 0)
   (make-game 12 "Bochum" 1 "Freiburg" 2)
   (make-game 12 "Hoffenheim" 1 "Wolfsburg" 2)
   (make-game 12 "Bayern" 1 "Schalke" 1)
   (make-game 12 "Hannover" 2 "Hamburg" 2)
   (make-game 12 "Bremen" 1 "Dortmund" 1)
   (make-game 12 "Hertha" 0 "1. FC Köln" 1)
   
   (make-game 13 "Schalke" 2 "Hannover" 0)
   (make-game 13 "Freiburg" 0 "Bremen" 6)
   (make-game 13 "Stuttgart" 1 "Hertha" 1)
   (make-game 13 "1. FC Köln" 0 "Hoffenheim" 4)
   (make-game 13 "Wolfsburg" 2 "Nürnberg" 3)
   (make-game 13 "Frankfurt" 1 "Gladbach" 2)
   (make-game 13 "Dortmund" 0 "Mainz" 0)
   (make-game 13 "Bayern" 1 "Bayer 04" 1)
   (make-game 13 "Hamburg" 0 "Bochum" 1)
   
   (make-game 14 "Bochum" 0 "1. FC Köln" 0)
   (make-game 14 "Nürnberg" 0 "Freiburg" 1)
   (make-game 14 "Mainz" 1 "Hamburg" 1)
   (make-game 14 "Bremen" 2 "Wolfsburg" 2)
   (make-game 14 "Hoffenheim" 1 "Dortmund" 2)
   (make-game 14 "Hertha" 1 "Frankfurt" 3)
   (make-game 14 "Gladbach" 1 "Schalke" 0)
   (make-game 14 "Bayer 04" 4 "Stuttgart" 0)
   (make-game 14 "Hannover" 0 "Bayern" 3)
   
   (make-game 15 "Bayern" 2 "Gladbach" 1)
   (make-game 15 "Hannover" 0 "Bayer 04" 0)
   (make-game 15 "Dortmund" 4 "Nürnberg" 0)
   (make-game 15 "Hamburg" 0 "Hoffenheim" 0)
   (make-game 15 "Stuttgart" 1 "Bochum" 1)
   (make-game 15 "Wolfsburg" 2 "Freiburg" 2)
   (make-game 15 "Frankfurt" 2 "Mainz" 0)
   (make-game 15 "1. FC Köln" 0 "Bremen" 0)
   (make-game 15 "Schalke" 2 "Hertha" 0)
   
   (make-game 16 "Hertha" 2 "Bayer 04" 2)
   (make-game 16 "Nürnberg" 0 "Hamburg" 4)
   (make-game 16 "Freiburg" 0 "1. FC Köln" 0)
   (make-game 16 "Gladbach" 5 "Hannover" 3)
   (make-game 16 "Bochum" 1 "Bayern" 5)
   (make-game 16 "Hoffenheim" 1 "Frankfurt" 1)
   (make-game 16 "Bremen" 0 "Schalke" 2)
   (make-game 16 "Mainz" 1 "Stuttgart" 1)
   (make-game 16 "Wolfsburg" 1 "Dortmund" 3)
   
   (make-game 17 "Schalke" 1 "Mainz" 0)
   (make-game 17 "Frankfurt" 2 "Wolfsburg" 2)
   (make-game 17 "Hannover" 2 "Bochum" 3)
   (make-game 17 "Bayer 04" 3 "Gladbach" 2)
   (make-game 17 "Dortmund" 1 "Freiburg" 0)
   (make-game 17 "Bayern" 5 "Hertha" 2)
   (make-game 17 "Stuttgart" 3 "Hoffenheim" 1)
   (make-game 17 "Hamburg" 2 "Bremen" 1)
   (make-game 17 "1. FC Köln" 3 "Nürnberg" 0)
   
   (make-game 18 "Bayern" 2 "Hoffenheim" 0)
   (make-game 18 "Frankfurt" 1 "Bremen" 0)
   (make-game 18 "Hamburg" 2 "Freiburg" 0)
   (make-game 18 "Hannover" 0 "Hertha" 3)
   (make-game 18 "Bayer 04" 4 "Mainz" 2)
   (make-game 18 "Gladbach" 1 "Bochum" 2)
   (make-game 18 "Stuttgart" 3 "Wolfsburg" 1)
   (make-game 18 "Schalke" 1 "Nürnberg" 0)
   (make-game 18 "1. FC Köln" 2 "Dortmund" 3)
   
   (make-game 19 "Freiburg" 0 "Stuttgart" 1)
   (make-game 19 "Hertha" 0 "Gladbach" 0)
   (make-game 19 "Nürnberg" 1 "Frankfurt" 1)
   (make-game 19 "Mainz" 1 "Hannover" 0)
   (make-game 19 "Bochum" 2 "Schalke" 2)
   (make-game 19 "Bremen" 2 "Bayern" 3)
   (make-game 19 "Dortmund" 1 "Hamburg" 0)
   (make-game 19 "Wolfsburg" 2 "1. FC Köln" 3)
   (make-game 19 "Hoffenheim" 0 "Bayer 04" 3)
   
   (make-game 20 "Hamburg" 1 "Wolfsburg" 1)
   (make-game 20 "Hannover" 1 "Nürnberg" 3)
   (make-game 20 "Bayern" 3 "Mainz" 0)
   (make-game 20 "Hertha" 0 "Bochum" 0)
   (make-game 20 "Frankfurt" 1 "1. FC Köln" 2)
   (make-game 20 "Gladbach" 4 "Bremen" 3)
   (make-game 20 "Schalke" 2 "Hoffenheim" 0)
   (make-game 20 "Stuttgart" 4 "Dortmund" 1)
   (make-game 20 "Bayer 04" 3 "Freiburg" 1)
   
   (make-game 21 "Bremen" 2 "Hertha" 1)
   (make-game 21 "Hoffenheim" 2 "Hannover" 1)
   (make-game 21 "Bochum" 1 "Bayer 04" 1)
   (make-game 21 "Freiburg" 0 "Schalke" 0)
   (make-game 21 "1. FC Köln" 3 "Hamburg" 3)
   (make-game 21 "Wolfsburg" 1 "Bayern" 3)
   (make-game 21 "Nürnberg" 1 "Stuttgart" 2)
   (make-game 21 "Mainz" 1 "Gladbach" 0)
   (make-game 21 "Dortmund" 2 "Frankfurt" 3)
   
   (make-game 22 "Gladbach" 2 "Nürnberg" 1)
   (make-game 22 "Hannover" 1 "Bremen" 5)
   (make-game 22 "Bochum" 2 "Hoffenheim" 1)
   (make-game 22 "Stuttgart" 1 "Hamburg" 3)
   (make-game 22 "Bayer 04" 2 "Wolfsburg" 1)
   (make-game 22 "Hertha" 1 "Mainz" 1)
   (make-game 22 "Bayern" 3 "Dortmund" 1)
   (make-game 22 "Schalke" 2 "1. FC Köln" 0)
   (make-game 22 "Frankfurt" 2 "Freiburg" 1)
   
   (make-game 23 "Hoffenheim" 2 "Gladbach" 2)
   (make-game 23 "Mainz" 0 "Bochum" 0)
   (make-game 23 "Hamburg" 0 "Frankfurt" 0)
   (make-game 23 "Dortmund" 4 "Hannover" 1)
   (make-game 23 "1. FC Köln" 1 "Stuttgart" 5)
   (make-game 23 "Nürnberg" 1 "Bayern" 1)
   (make-game 23 "Freiburg" 0 "Hertha" 3)
   (make-game 23 "Bremen" 2 "Bayer 04" 2)
   (make-game 23 "Wolfsburg" 2 "Schalke" 1)
   
   (make-game 24 "Schalke" 2 "Dortmund" 1)
   (make-game 24 "Bochum" 0 "Nürnberg" 0)
   (make-game 24 "Gladbach" 1 "Freiburg" 1)
   (make-game 24 "Stuttgart" 2 "Frankfurt" 1)
   (make-game 24 "Mainz" 1 "Bremen" 2)
   (make-game 24 "Hertha" 0 "Hoffenheim" 2)
   (make-game 24 "Bayer 04" 0 "1. FC Köln" 0)
   (make-game 24 "Hannover" 0 "Wolfsburg" 1)
   (make-game 24 "Bayern" 1 "Hamburg" 0)
   
   (make-game 25 "Wolfsburg" 4 "Bochum" 1)
   (make-game 25 "Freiburg" 1 "Hannover" 2)
   (make-game 25 "Frankfurt" 1 "Schalke" 4)
   (make-game 25 "Hamburg" 1 "Hertha" 0)
   (make-game 25 "Bremen" 2 "Stuttgart" 2)
   (make-game 25 "1. FC Köln" 1 "Bayern" 1)
   (make-game 25 "Dortmund" 3 "Gladbach" 0)
   (make-game 25 "Nürnberg" 3 "Bayer 04" 2)
   (make-game 25 "Hoffenheim" 0 "Mainz" 1)
   
   (make-game 26 "Schalke" 2 "Stuttgart" 1)
   (make-game 26 "Mainz" 1 "1. FC Köln" 0)
   (make-game 26 "Bochum" 1 "Dortmund" 4)
   (make-game 26 "Hertha" 1 "Nürnberg" 2)
   (make-game 26 "Gladbach" 0 "Wolfsburg" 4)
   (make-game 26 "Hannover" 2 "Frankfurt" 1)
   (make-game 26 "Bayern" 2 "Freiburg" 1)
   (make-game 26 "Hoffenheim" 0 "Bremen" 1)
   (make-game 26 "Bayer 04" 4 "Hamburg" 2)
   
   (make-game 27 "1. FC Köln" 1 "Gladbach" 1)
   (make-game 27 "Freiburg" 1 "Mainz" 0)
   (make-game 27 "Bremen" 3 "Bochum" 2)
   (make-game 27 "Stuttgart" 2 "Hannover" 0)
   (make-game 27 "Nürnberg" 0 "Hoffenheim" 0)
   (make-game 27 "Frankfurt" 2 "Bayern" 1)
   (make-game 27 "Dortmund" 3 "Bayer 04" 0)
   (make-game 27 "Hamburg" 2 "Schalke" 2)
   (make-game 27 "Wolfsburg" 1 "Hertha" 5)
   
   (make-game 28 "Bochum" 1 "Frankfurt" 2)
   (make-game 28 "Bremen" 4 "Nürnberg" 2)
   (make-game 28 "Hannover" 1 "1. FC Köln" 4)
   (make-game 28 "Hertha" 0 "Dortmund" 0)
   (make-game 28 "Bayern" 1 "Stuttgart" 2)
   (make-game 28 "Mainz" 0 "Wolfsburg" 2)
   (make-game 28 "Bayer 04" 0 "Schalke" 2)
   (make-game 28 "Hoffenheim" 1 "Freiburg" 1)
   (make-game 28 "Gladbach" 1 "Hamburg" 0)
   
   (make-game 29 "Nürnberg" 2 "Mainz" 0)
   (make-game 29 "Stuttgart" 2 "Gladbach" 1)
   (make-game 29 "Freiburg" 1 "Bochum" 1)
   (make-game 29 "Dortmund" 2 "Bremen" 1)
   (make-game 29 "Frankfurt" 3 "Bayer 04" 2)
   (make-game 29 "Schalke" 1 "Bayern" 2)
   (make-game 29 "1. FC Köln" 0 "Hertha" 3)
   (make-game 29 "Wolfsburg" 4 "Hoffenheim" 0)
   (make-game 29 "Hamburg" 0 "Hannover" 0)
   
   (make-game 30 "Gladbach" 2 "Frankfurt" 0)
   (make-game 30 "Bremen" 4 "Freiburg" 0)
   (make-game 30 "Hoffenheim" 0 "1. FC Köln" 2)
   (make-game 30 "Hannover" 4 "Schalke" 2)
   (make-game 30 "Mainz" 1 "Dortmund" 0)
   (make-game 30 "Hertha" 0 "Stuttgart" 1)
   (make-game 30 "Bayer 04" 1 "Bayern" 1)
   (make-game 30 "Bochum" 1 "Hamburg" 2)
   (make-game 30 "Nürnberg" 0 "Wolfsburg" 2)
   
   (make-game 31 "1. FC Köln" 2 "Bochum" 0)
   (make-game 31 "Freiburg" 2 "Nürnberg" 1)
   (make-game 31 "Hamburg" 0 "Mainz" 1)
   (make-game 31 "Schalke" 3 "Gladbach" 1)
   (make-game 31 "Wolfsburg" 2 "Bremen" 4)
   (make-game 31 "Stuttgart" 2 "Bayer 04" 1)
   (make-game 31 "Bayern" 7 "Hannover" 0)
   (make-game 31 "Dortmund" 1 "Hoffenheim" 1)
   (make-game 31 "Frankfurt" 2 "Hertha" 2)
   
   (make-game 32 "Bochum" 0 "Stuttgart" 2)
   (make-game 32 "Hertha" 0 "Schalke" 1)
   (make-game 32 "Nürnberg" 2 "Dortmund" 3)
   (make-game 32 "Gladbach" 1 "Bayern" 1)
   (make-game 32 "Mainz" 1 "Frankfurt" 2)
   (make-game 32 "Bayer 04" 3 "Hannover" 0)
   (make-game 32 "Bremen" 1 "1. FC Köln" 0)
   (make-game 32 "Hoffenheim" 5 "Hamburg" 1)
   (make-game 32 "Freiburg" 1 "Wolfsburg" 0)
   
   (make-game 33 "Hamburg" 4 "Nürnberg" 0)
   (make-game 33 "Stuttgart" 2 "Mainz" 2)
   (make-game 33 "1. FC Köln" 2 "Freiburg" 2)
   (make-game 33 "Hannover" 6 "Gladbach" 1)
   (make-game 33 "Bayern" 3 "Bochum" 1)
   (make-game 33 "Schalke" 0 "Bremen" 2)
   (make-game 33 "Frankfurt" 1 "Hoffenheim" 2)
   (make-game 33 "Bayer 04" 1 "Hertha" 1)
   (make-game 33 "Dortmund" 1 "Wolfsburg" 1)
   
   (make-game 34 "Nürnberg" 1 "1. FC Köln" 0)
   (make-game 34 "Bochum" 0 "Hannover" 3)
   (make-game 34 "Gladbach" 1 "Bayer 04" 1)
   (make-game 34 "Mainz" 0 "Schalke" 0)
   (make-game 34 "Freiburg" 3 "Dortmund" 1)
   (make-game 34 "Bremen" 1 "Hamburg" 1)
   (make-game 34 "Hoffenheim" 1 "Stuttgart" 1)
   (make-game 34 "Hertha" 1 "Bayern" 3)
   (make-game 34 "Wolfsburg" 3 "Frankfurt" 1)))

; Punktzahl in Spiel
(define points
  (signature (enum 0 1 3)))

; Punktzahl für Heimmannschaft berechnen
(: home-points (game -> points))

(check-expect (home-points game1) 3)
(check-expect (home-points game2) 1)
(check-expect (home-points game3) 3)
(check-expect (home-points game4) 0)

; erste Version
(define home-points
  (lambda (game)
    (define goals1 (game-home-goals game))
    (define goals2 (game-guest-goals game))
    (cond
      ((> goals1 goals2) 3)
      ((< goals1 goals2) 0)
      ((= goals1 goals2) 1))))


; Punktzahl für Gastmannschaft berechnen
(: guest-points (game -> points))

(check-expect (guest-points game1) 0)
(check-expect (guest-points game2) 1)
(check-expect (guest-points game3) 0)
(check-expect (guest-points game4) 3)

(define guest-points
  (lambda (game)
    (define goals1 (game-guest-goals game))
    (define goals2 (game-home-goals game))
    (cond
      ((> goals1 goals2) 3)
      ((< goals1 goals2) 0)
      ((= goals1 goals2) 1))))

; Punktzahl bestimmen
(: compute-points ((game -> natural) (game -> natural) game -> points))

(define compute-points
  (lambda (get-goals-1 get-goals-2 game)
    (define goals1 (get-goals-1 game))
    (define goals2 (get-goals-2 game))
    (cond
      ((> goals1 goals2) 3)
      ((< goals1 goals2) 0)
      ((= goals1 goals2) 1))))

(define home-points2
  (lambda (game)
    (compute-points game-home-goals game-guest-goals game)))

(define guest-points2
  (lambda (game)
    (compute-points game-guest-goals game-home-goals game)))

; Punktzahl-Bestimmungsfunktion erzeugen
(: make-compute-points ((game -> natural) (game -> natural) -> (game -> points)))

(define make-compute-points
  (lambda (get-goals-1 get-goals-2)
    (lambda (game)
      (define goals1 (get-goals-1 game))
      (define goals2 (get-goals-2 game))
      (cond
        ((> goals1 goals2) 3)
        ((< goals1 goals2) 0)
        ((= goals1 goals2) 1)))))

(define home-points3
  (lambda (game)
    ((make-compute-points game-home-goals game-guest-goals) game)))

(define guest-points3
  (lambda (game)
    ((make-compute-points game-guest-goals game-home-goals) game)))

(define home-points4 (make-compute-points game-home-goals game-guest-goals))

(define guest-points4 (make-compute-points game-guest-goals game-home-goals))

; Ist Spiel unentschieden?
(: game-draw? (game -> boolean))

(check-expect (game-draw? game1) #f)
(check-expect (game-draw? game2) #t)
(check-expect (game-draw? game3) #f)

(define game-draw?
  (lambda (game)
    (= 1 (home-points game))))

; Unentschiedene Spiele rausfiltern
(: drawn-games ((list-of game) -> (list-of game)))

(check-expect (drawn-games day1) (list game2 game7 game8 game9))

(define drawn-games
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (game-draw? (first list))
           (cons (first list) (drawn-games (rest list)))
           (drawn-games (rest list)))))))

; Hat die Heimmannschaft gewonnen?
(: home-won? (game -> boolean))

(check-expect (home-won? game1) #t)
(check-expect (home-won? game2) #f)
(check-expect (home-won? game4) #f)

(define home-won?
  (lambda (game)
    (= 3 (home-points game))))

; Spiele herausfiltern, bei denen die Heimmannschaft gewann
(: home-won-games ((list-of game) -> (list-of game)))

(check-expect (home-won-games day1) (list game1 game3 game6))

(define home-won-games
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (home-won? (first list))
           (cons (first list)
                 (home-won-games (rest list)))
           (home-won-games (rest list)))))))

(: extract-games ((game -> boolean) (list-of game) -> (list-of game)))

(check-expect (extract-games game-draw? day1) (list game2 game7 game8 game9))
(check-expect (extract-games home-won? day1) (list game1 game3 game6))

(define extract-games
  (lambda (f? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (f? (first list))
           (cons (first list)
                 (extract-games f? (rest list)))
           (extract-games f? (rest list)))))))
    
(: extract-list ((%a -> boolean) (list-of %a) -> (list-of %a)))

(check-expect (extract-games game-draw? day1) (list game2 game7 game8 game9))
(check-expect (extract-games home-won? day1) (list game1 game3 game6))

(define extract-list
  (lambda (f? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (f? (first list))
           (cons (first list)
                 (extract-list f? (rest list)))
           (extract-list f? (rest list)))))))

                   
; Spielt Mannschaft bei Spiel?
(: plays-game? (team game -> boolean))

(check-expect (plays-game? "Wolfsburg" game1) #t)
(check-expect (plays-game? "Stuttgart" game1) #t)
(check-expect (plays-game? "Hannover" game1) #f)

(define plays-game?
  (lambda (team game)
    (or (string=? team (game-home-team game))
        (string=? team (game-guest-team game)))))

; Spielt Nürnberg mit?
(: plays-nürnberg? (game -> boolean))

(check-expect (plays-nürnberg? game1) #f)
(check-expect (plays-nürnberg? game5) #t)

(define plays-nürnberg?
  (lambda (game)
    (plays-game? "Nürnberg" game)))

; Eine hanebüchene Frage:
; Hat der SC Freiburg in der Saison 2009/10 gegen 1. FC Bayern München gewonnen?
(check-expect (cons?
               (filter (lambda (game)
                         (and (plays-game? "Freiburg" game)
                              (plays-game? "Bayern" game)
                              (= (team-points "Freiburg" game) 3)))
                       season-2009/2010))
              #f)
; und wie sieht es aus mit Dortmund gegen Bayer 04?
(check-expect (cons?
               (filter (lambda (g)
                         (and (plays-game? "Dortmund" g)
                              (plays-game? "Bayer 04" g)
                              (= (team-points "Dortmund" g) 3)))
                       season-2009/2010))
              #t)

(: plays-game?/team (team -> (game -> boolean)))

(define plays-game?/team
  (lambda (team)
    (lambda (game) (plays-game? team game))))

; Punkte einer Mannschaft aus Spiel berechnen
(: team-points (team game -> points))

(check-expect (team-points "Freiburg" game9) 1)
(check-expect (team-points "Dortmund" game6) 3)
(check-error (team-points "Hannover" game1))

(define team-points
  (lambda (team game)
    (cond
      ((string=? team (game-home-team game)) (home-points game))
      ((string=? team (game-guest-team game)) (guest-points game)))))

; Gesamttore eines Spiels berechnen
(: total-goals (game -> natural))

(check-expect (total-goals game1) 2)
(check-expect (total-goals game2) 4)

(define total-goals
  (lambda (game)
    (+ (game-home-goals game)
       (game-guest-goals game))))

; Gesamttore in einer Liste von Spielen berechnen
(: list-total-goals ((list-of game) -> (list-of natural)))

(check-expect (list-total-goals day1)
              (list 2 4 1 5 3 1 2 6 2))

(define list-total-goals
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (total-goals (first list))
             (list-total-goals (rest list)))))))

; Tore einer Mannschaft aus einem Spiel berechnen
(: team-goals (team game -> natural))

(check-expect (team-goals "Wolfsburg" game1) 2)
(check-expect (team-goals "Stuttgart" game1) 0)
(check-error (team-goals "Hannover" game1))

(define team-goals
  (lambda (team game)
    (cond
      ((string=? team (game-home-team game)) (game-home-goals game))
      ((string=? team (game-guest-team game)) (game-guest-goals game)))))

; Tore einer Mannschaft aus einer Liste von Spielen auflisten
(: list-team-goals (team (list-of game) -> (list-of natural)))

(define list-team-goals
  (lambda (team list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (team-goals team (first list))
             (list-team-goals team (rest list)))))))

; Auch das geht allgemeiner: Eine Funktion (%a -> %b) "fortsetzen" zu einer
; Funktion ((list-of %a) -> (list-of %b)):

(: list-apply ((%a -> %b) (list-of %a) -> (list-of %b)))

(check-expect (list-apply (lambda (game)
                            (team-goals "Hamburg" game))
                          (filter (lambda (game)
                                    (plays-game? "Hamburg" game))
                                  season-2009/2010))
              (list 1 4 4 3 3 1 1 3 0 3 2 2 0 1 0 4 2 2 0 1 3 3 0 0 1 2 2 0 0 2 0 1 4 1))

#;(define list-apply
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (total-goals (first list))
             (list-apply (rest list)))))))

#;(define list-apply
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (f (first list))
             (list-apply (rest list)))))))

(define list-apply
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (f (first list))
             (list-apply f (rest list)))))))

; jetzt können wir noch von "Hamburg" abstrahieren:

(: team-goals/team (team -> (game -> natural)))
(define team-goals/team
  (lambda (team)
    (lambda (game)
      (team-goals team game))))

(check-expect (list-apply (team-goals/team "Hamburg")
                          (filter (plays-game?/team "Hamburg")
                                  season-2009/2010))
              (list 1 4 4 3 3 1 1 3 0 3 2 2 0 1 0 4 2 2 0 1 3 3 0 0 1 2 2 0 0 2 0 1 4 1))

; list-map hat nicht genau die Signatur, die wir "versprochen" hatten:
; geplant war ((%a -> %b) -> ((list-of %a) -> (list-of %b)))
; geliefert haben wir ((%a -> %b) (list-of %a) -> (list-of %b))
; Da hilft der Schönfinkel-Isomorphismus, der im englischen Sprachraum Curry-Isomorphismus heißt:

#;(define xxx
  (lambda (team)
    (lambda (game)
      (f team game))))

#;(define xxx
    (lambda (f)
      (lambda (team)
        (lambda (game)
          (f team game)))))

(: xxx ((%a %b -> %c) -> (%a -> (%b -> %c))))

(define xxx
  (lambda (f)
    (lambda (a)
      (lambda (b)
        (f a b)))))

; Funktion schönfinkeln
(: curry ((%a %b -> %c) -> (%a -> (%b -> %c))))

(check-expect (list-apply ((curry team-goals) "Hamburg")
                          (filter ((curry plays-game?) "Hamburg")
                                  season-2009/2010))
              (list 1 4 4 3 3 1 1 3 0 3 2 2 0 1 0 4 2 2 0 1 3 3 0 0 1 2 2 0 0 2 0 1 4 1))

(define curry
  (lambda (f)
    (lambda (a)
      (lambda (b)
        (f a b)))))

; Funktion entschönfinkeln
(: uncurry ((%a -> (%b -> %c)) -> (%a %b -> %c)))

(check-expect ((uncurry (curry +)) 3 4) 7)

(define uncurry
  (lambda (f)
    (lambda (a b)
      ((f a) b))))
