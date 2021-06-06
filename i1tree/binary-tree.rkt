;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname binary-tree) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Binärbäume

; Ein leerer Baum 
;(: make-empty-tree empty-tree)
(define-record empty-tree
  make-empty-tree empty-tree?)

(define the-empty-tree (make-empty-tree))

; Ein Knoten besteht aus
; - einem Label
; - einem linken Baum
; - einem rechten Baum
(define-record (node-of label)
  make-node node?
  (node-label label)
  (node-left-branch (tree-of label))
  (node-right-branch (tree-of label)))

; Ein Binärbaum ist entweder ein leerer Baum oder ein Knoten
(define tree-of
  (lambda (label)
    (signature (mixed empty-tree (node-of label)))))

(define tree1 (make-node 3 (make-node 4 the-empty-tree (make-node 7 the-empty-tree the-empty-tree)) the-empty-tree))
(define tree2 (make-node 17 (make-node 3 the-empty-tree tree1) the-empty-tree))

; Tiefe eines Baums berechnen
(: depth ((tree-of %a) -> natural))
(check-expect (depth tree1) 3)
(check-expect (depth tree2) 5)

(define depth
  (lambda (tree)
    (cond
      ((empty-tree? tree)
       0)
      ((node? tree)
       (+ 1
          (max (depth (node-left-branch tree))
               (depth (node-right-branch tree))))))))

; Knoten in Baum zählen
(: node-count ((tree-of %a) -> natural))
(check-expect (node-count tree1) 3)
(check-expect (node-count tree2) 5)

(define node-count
  (lambda (t)
    (cond
      ((empty-tree? t)
       0)
      ((node? t)
       (+ 1
          (node-count (node-left-branch t))
          (node-count (node-right-branch t)))))))

; Suchbäume

; Ein Suchbaum besteht aus
; - einer Prozedur, die zwei Markierungen auf Gleichheit testet,
; - einer Prozedur, die vergleicht, ob eine Markierung kleiner als die andere ist
; - einem Binärbaum
(define-record (search-tree-of label)
  make-search-tree search-tree?
  (search-tree-label-equal-function (label label -> boolean))
  (search-tree-label-less-than-function (label label -> boolean))
  (search-tree-tree (tree-of label)))

; leeren Suchbaum konstruieren
(: make-empty-search-tree
   ((%a %a -> boolean)
    (%a %a -> boolean)
    -> (search-tree-of %a)))

(define make-empty-search-tree
  (lambda (label-equal-function label-less-than-function)
    (make-search-tree label-equal-function label-less-than-function
                      the-empty-tree)))

;Beispiele für einen Suchbäume
; s1 ist in Wirklichkeit gar kein Suchbaum:
(define s1
  (make-search-tree
   = <
   (make-node 5
              (make-node 17 the-empty-tree the-empty-tree)
              (make-node 3 the-empty-tree the-empty-tree))))

; s2 ist ein "echter" Suchbaum
(define s2
  (make-search-tree
   = <
   (make-node 5
              (make-node 3 the-empty-tree the-empty-tree)
              (make-node 17
                         (make-node 10 the-empty-tree (make-node 12 the-empty-tree the-empty-tree))
                         the-empty-tree))))



; festellen, ob Element in Suchbaum vorhanden ist
(: search-tree-member? (%a (search-tree-of %a) -> boolean))
; s1 ist kein Suchbaum, deswegen wird ausser der Wurzel kein Element gefunden
(check-expect (search-tree-member? 5 s1) #t)
(check-expect (search-tree-member? 17 s1) #f)
(check-expect (search-tree-member? 3 s1) #f)
; bei s2 sieht die Sache besser aus:
(check-expect (search-tree-member? 5 s2) #t)
(check-expect (search-tree-member? 17 s2) #t)
(check-expect (search-tree-member? 3 s2) #t)
(check-expect (search-tree-member? 10 s2) #t)

(define search-tree-member?
  (lambda (l s)
    (define label-equal? (search-tree-label-equal-function s))
    (define label-less-than? (search-tree-label-less-than-function s))
    (define member?
      ;; (: member? (tree -> boolean))
      (lambda (t)
        (cond
          ((empty-tree? t) #f)
          ((node? t)
           (cond                 
             ((label-equal? (node-label t) l)
              #t)
             ((label-less-than? l (node-label t))
              (member? (node-left-branch t)))
             (else
              (member? (node-right-branch t))))))))
    (member? (search-tree-tree s))))


; neues Element in Suchbaum einfügen
(: search-tree-insert (%a (search-tree-of %a) -> (search-tree-of %a)))
(check-expect (search-tree-member? 5  s3) #t)
(check-expect (search-tree-member? 17  s3) #t)
(check-expect (search-tree-member? 3  s3) #t)
(check-expect (search-tree-member? 13  s3) #f)
(check-expect (search-tree-member? -1  s3) #f)


(define search-tree-insert
  (lambda (l s)
    (define label-equal? (search-tree-label-equal-function s))
    (define label-less-than? (search-tree-label-less-than-function s))
    (define insert
      ; (: insert ((tree-of %a) -> (tree-of %a)))
      (lambda (t)
        (cond
          ((empty-tree? t)
           (make-node l the-empty-tree the-empty-tree))
          ((node? t)
           (cond
             ((label-equal? l (node-label t))
              t)
             ((label-less-than? l (node-label t))
              (make-node (node-label t)
                         (insert (node-left-branch t))
                         (node-right-branch t)))
             (else
              (make-node (node-label t)
                         (node-left-branch t)
                         (insert (node-right-branch t)))))))))
    (make-search-tree
     label-equal? label-less-than?
     (insert (search-tree-tree s)))))
  
; aus allen Elementen einer Liste einen Suchbaum machen
(: list->search-tree ((%a %a -> boolean)
                      (%a %a -> boolean) (list-of %a) -> (search-tree-of %a)))

(check-property
 (for-all ((els (list-of natural)))
   (let ((st (list->search-tree = < els)))
     (every? (lambda (el)
               (search-tree-member? el st))
             els))))

(check-property
 (for-all ((els (list-of natural))
           (el natural))
   (==> (not (member? = el els))
        (not (search-tree-member? el (list->search-tree = < els)))))) 

(define list->search-tree
  (lambda (= < els)
    (fold (make-empty-search-tree = <)
          search-tree-insert
          els)))

(define every?
  (lambda (p? lis)
    (fold #t
          (lambda (first result)
            (and result
                 (p? first)))
          lis)))

; ist Wert Element einer Liste?
(: member? ((%a %a -> boolean) %a (list-of %a) -> boolean))

(check-expect (member? = 5 empty) #f)
(check-expect (member? = 5 (list 1 2 3)) #f)
(check-expect (member? = 1 (list 1 2 3)) #t)
(check-expect (member? = 2 (list 1 2 3)) #t)
(check-expect (member? = 3 (list 1 2 3)) #t)

(define member?
  (lambda (= el lis)
    (cond
      ((empty? lis) #f)
      ((cons? lis)
       (if (= el (first lis))
           #t
           (member? = el (rest lis)))))))

;Baum mit search-tree-insert
(define s3
  (search-tree-insert
   5
   (search-tree-insert
    17
    (search-tree-insert
     3
     (make-empty-search-tree = <)))))
