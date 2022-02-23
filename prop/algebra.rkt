#lang deinprogramm/sdp
(require teachpack/deinprogramm/sdp/image)

(check-property
 (for-all ((a image)
           (b image)
           (c image))
   (expect (overlay (overlay a b) c)
           (overlay a (overlay b c)))))

#;(check-property
 (for-all ((a number)
           (b number)
           (c number))
   (= (+ (+ a b) c)
      (+ a (+ b c)))))
     

(: concatenate ((list-of %element) (list-of %element) -> (list-of %element)))

(check-expect (concatenate (list 1 2 3) (list 4 5 6))
              (list 1 2 3 4 5 6))

(check-property
 (for-all ((a (list-of integer))
           (b (list-of integer))
           (c (list-of integer)))
   (expect (concatenate (concatenate a b) c)
           (concatenate a (concatenate b c)))))

(check-property
 (for-all ((a (list-of integer)))
   (expect (concatenate a empty) a)))

(check-property
 (for-all ((a (list-of integer)))
   (expect (concatenate empty a) a)))

(define concatenate
  (lambda (list1 list2)
    (cond
      ((empty? list1) list2)
      ((cons? list1) 
       (cons (first list1)
             (concatenate (rest list1) list2))))))

; böse Überraschung

#;(check-property
 (for-all ((a number)
           (b number)
           (c number))
    (= (+ a (+ b c))
       (+ (+ a b) c))))


; Summe der Elemente einer Liste von Zahlen berechnen
(: list-sum ((list-of number) -> number))

(check-property
 (for-all ((a (list-of rational))
           (b (list-of rational)))
   (= (list-sum (concatenate a b))
      (+ (list-sum a) (list-sum b)))))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list) (list-sum (rest list)))))))

