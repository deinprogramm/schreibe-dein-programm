;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname binary-tree) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Binärbäume

; Ein Knoten besteht aus
; - Markierung
; - linken Ast
; - rechter Ast
(define-record (node-of leaf label)
  make-node node?
  (node-label label)
  (node-left-branch (tree-of leaf label))
  (node-right-branch (tree-of leaf label)))

; Ein Binärbaum ist entweder ein Blatt oder ein Knoten
(define tree-of
  (lambda (leaf label)
    (signature (mixed leaf (node-of leaf label)))))

(: tree1 (tree-of false number))
(define tree1 (make-node 3 (make-node 4 (make-node 5 #f #f) (make-node 7 #f #f)) #f))
(: tree2 (tree-of false number))
(define tree2 (make-node 17 (make-node 3 #f tree1) (make-node 20 #f #f)))
(: tree3 (tree-of number string))
(define tree3 (make-node "Axl"
                         (make-node "Slash" 17 (make-node "Duff" 14 23))
                         12))

; Beispiele
(define-record creek
  make-creek
  creek?
  (creek-origin string))
(define river (tree-of creek string))
(define river? node?)
(define make-confluence make-node)
(define confluence-location node-label)
(define confluence-main-stem node-left-branch)
(define confluence-tributary node-right-branch)

(define-record unknown-parent
  make-unknown-parent
  unknown-parent?)
(define parent (tree-of unknown-parent string))

(define person (node-of unknown-parent string))
(define person? node?)
(define an-unknown-parent (make-unknown-parent))
(define make-person make-node)

(define slash
  (make-person "Slash"
               (make-person "Ola Hudson"
                            an-unknown-parent
                            an-unknown-parent)
               (make-person "Anthony Hudson"
                            an-unknown-parent
                            an-unknown-parent)))
(define london-hudson
  (make-person "London Hudson"
               slash
               (make-person "Perla Ferrar"
                            an-unknown-parent
                            an-unknown-parent)))


(define person-name node-label)
(define person-parent-1 node-left-branch)
(define person-parent-2 node-right-branch)


; Ist jemand Vorfahr:in einer Person?
(: ancestor? (string person -> boolean))

(check-expect (ancestor? "Slash" london-hudson) #t)
(check-expect (ancestor? "Axl" london-hudson) #f)

(define ancestor?
  (lambda (name person)
    (or (string=? name (person-name person))
        (parent-ancestor? name (person-parent-1 person))
        (parent-ancestor? name (person-parent-2 person)))))

; Ist jemand Vorfahr:in eines Elternteils?
(: parent-ancestor? (string parent -> boolean))

(check-expect (parent-ancestor? "Slash" london-hudson) #t)
(check-expect (parent-ancestor? "Axl" london-hudson) #f)
(check-expect (parent-ancestor? "Slash" an-unknown-parent) #f)

(define parent-ancestor?
  (lambda (name parent)
    (cond
      ((person? parent)
       (ancestor? name parent))
      ((unknown-parent? parent)
       #f))))

; Tiefe eines Baums berechnen
(: depth ((tree-of %leaf %label) -> natural))

(check-expect (depth tree1) 3)
(check-expect (depth tree2) 5)

(define depth
  (lambda (tree)
    (cond
      ((node? tree)
       (+ 1
          (max (depth (node-left-branch tree))
               (depth (node-right-branch tree)))))
      (else 0))))

; Knoten in Baum zählen
(: node-count ((tree-of %leaf %label) -> natural))
(check-expect (node-count tree1) 4)
(check-expect (node-count tree2) 7)

(define node-count
  (lambda (t)
    (if (node? t)
        (+ 1
           (node-count (node-left-branch t))
           (node-count (node-right-branch t)))
        0)))

; ist Wert Element einer Liste?
(: member? ((%a %a -> boolean) %a (list-of %a) -> boolean))

(check-expect (member? = 5 empty) #f)
(check-expect (member? = 5 (list 1 2 3)) #f)
(check-expect (member? = 1 (list 1 2 3)) #t)
(check-expect (member? = 2 (list 1 2 3)) #t)
(check-expect (member? = 3 (list 1 2 3)) #t)
(check-expect (member? string=? "Slash" (list "Axl" "Slash")) #t)
(check-expect (member? string=? "Buckethead" (list "Axl" "Slash")) #f)

(define member?
  (lambda (equals? element list)
    (cond
      ((empty? list) #f)
      ((cons? list)
       (if (equals? element (first list))
           #t
           (member? equals? element (rest list)))))))



; Ist eine Zahl in einem sortierten Baum vorhanden? 
(: tree-member? (real (tree-of false real) -> boolean))

(define tree4
  (make-node 5
             (make-node 3 #f #f)
             (make-node 17
                        (make-node 10 #f (make-node 12 #f #f))
                        #f)))

(check-expect (tree-member? 5 tree4) #t)
(check-expect (tree-member? 17 tree4) #t)
(check-expect (tree-member? 3 tree4) #t)
(check-expect (tree-member? 10 tree4) #t)
(check-expect (tree-member? 2 tree4) #f)
      
(define tree-member?
  (lambda (value tree)
    (cond
      ((node? tree)
       (define label (node-label tree))
       (cond
         ((= value label) #t)
         ((< value label)
          (tree-member? value (node-left-branch tree)))
         (else
          (tree-member? value (node-right-branch tree)))))
      (else #f))))

; Zahl in sortierten Baum einfügen 
(: tree-insert (real (tree-of false real) -> (tree-of false real)))

(check-expect (tree-member? 5 (tree-insert 5 tree4)) #t)
(check-expect (tree-member? 11 (tree-insert 11 tree4)) #t)

(define tree-insert
  (lambda (value tree)
    (cond
      ((node? tree)
       (cond
         ((= value (node-label tree))
          tree)
         ((< value (node-label tree))
          (make-node (node-label tree)
                     (tree-insert value (node-left-branch tree))
                     (node-right-branch tree)))
         (else
          (make-node (node-label tree)
                     (node-left-branch tree)
                     (tree-insert value (node-right-branch tree))))))
      (else
       (make-node value #f #f)))))

; Suchbäume

; Ein Suchbaum besteht aus
; - Funktion für =
; - Funktion für <
; - Binärbaum
(define-record (search-tree-of element)
  make-search-tree search-tree?
  (search-tree-label-=?-function (element element -> boolean))
  (search-tree-label-<?-function (element element -> boolean))
  (search-tree-tree (tree-of false element)))

(define search-tree1
  (make-search-tree
   string=? string<?
   (make-node "M"
              (make-node "B"
                         (make-node "A" #f #f)
                         (make-node "D" #f #f))
              (make-node "O"
                         (make-node "N" #f #f)
                         (make-node "R" #f #f)))))

; festellen, ob Element in Suchbaum vorhanden ist
(: search-tree-member? (%a (search-tree-of %a) -> boolean))
(check-expect (search-tree-member? "M" search-tree1) #t)
(check-expect (search-tree-member? "D" search-tree1) #t)
(check-expect (search-tree-member? "N" search-tree1) #t)
(check-expect (search-tree-member? "R" search-tree1) #t)
(check-expect (search-tree-member? "Z" search-tree1) #f)

(define search-tree-member?
  (lambda (value search-tree)
    (define label=? (search-tree-label-=?-function search-tree))
    (define label<? (search-tree-label-<?-function search-tree))
    (define tree-member?
      (lambda (value tree)
        (cond
          ((node? tree)
           (define label (node-label tree))
           (cond
             ((label=? value label) #t)
             ((label<? value label)
              (tree-member? value (node-left-branch tree)))
             (else
              (tree-member? value (node-right-branch tree)))))
          (else #f))))
    (tree-member? value (search-tree-tree search-tree))))


; leeren Suchbaum konstruieren
(: make-empty-search-tree
   ((%a %a -> boolean) (%a %a -> boolean)
                       -> (search-tree-of %a)))

(define make-empty-search-tree
  (lambda (label-=?-function label-<?-function)
    (make-search-tree label-=?-function label-<?-function
                      #f)))


; neues Element in Suchbaum einfügen
(: search-tree-insert (%a (search-tree-of %a) -> (search-tree-of %a)))
(check-expect (search-tree-member?  5 search-tree2) #t)
(check-expect (search-tree-member? 17 search-tree2) #t)
(check-expect (search-tree-member?  3 search-tree2) #t)
(check-expect (search-tree-member? 13 search-tree2) #f)
(check-expect (search-tree-member? -1 search-tree2) #f)

(define search-tree-insert
  (lambda (value search-tree)
    (define label=? (search-tree-label-=?-function search-tree))
    (define label<? (search-tree-label-<?-function search-tree))
    (define tree-insert
      (lambda (value tree)
        (cond
          ((node? tree)
           (cond
             ((label=? value (node-label tree))
              tree)
             ((label<? value (node-label tree))
              (make-node (node-label tree)
                         (tree-insert value (node-left-branch tree))
                         (node-right-branch tree)))
             (else
              (make-node (node-label tree)
                         (node-left-branch tree)
                         (tree-insert value (node-right-branch tree))))))
          (else
           (make-node value #f #f)))))
    (make-search-tree
     label=? label<?
     (tree-insert value (search-tree-tree search-tree)))))

(define search-tree2
  (search-tree-insert
   5
   (search-tree-insert
    17
    (search-tree-insert
     3
     (make-empty-search-tree = <)))))


; aus allen Elementen einer Liste einen Suchbaum machen
(: list->search-tree ((%a %a -> boolean) (%a %a -> boolean)
                                         (list-of %a) -> (search-tree-of %a)))

(check-property
 (for-all ((elements (list-of natural)))
   (let ((search-tree (list->search-tree = < elements)))
     (every? (lambda (element)
               (search-tree-member? element search-tree))
             elements))))

(check-property
 (for-all ((elements (list-of natural))
           (element natural))
   (==> (not (member? = element elements))
        (not (search-tree-member? element (list->search-tree = < elements)))))) 

(define list->search-tree
  (lambda (= < elements)
    (fold (make-empty-search-tree = <)
          search-tree-insert
          elements)))

(define every?
  (lambda (p? list)
    (fold #t
          (lambda (first result)
            (and result
                 (p? first)))
          list)))

; Die Markierung an einem größenannotierten Baum besteht aus:
; - Anzahl der Knoten
; - "eigentliche" Markierung
(define-record (sized-label-of label)
  make-sized-label
  sized-label
  (sized-label-size natural)
  (sized-label-label label))

(define sized-tree1
  (make-node (make-sized-label 7 "M")
             (make-node (make-sized-label 3 "B")
                        (make-node (make-sized-label 1 "A") #f #f)
                        (make-node (make-sized-label 1 "D") #f #f))
             (make-node (make-sized-label 3 "O")
                        (make-node (make-sized-label 1 "N") #f #f)
                        (make-node (make-sized-label 1 "R") #f #f))))

; Größenannotierten Knoten konstruieren
(: make-sized-node (%label (tree-of %leaf (sized-label-of %label))
                           (tree-of %leaf (sized-label-of %label))
                     -> (node-of %leaf (sized-label-of %label))))

(check-expect (make-sized-node
               "M"
               (make-sized-node "B"
                                (make-sized-node "A" #f #f)
                                (make-sized-node "D" #f #f))
               (make-sized-node "O"
                                (make-sized-node "N" #f #f)
                                (make-sized-node "R" #f #f)))
              sized-tree1)

(define make-sized-node
  (lambda (label left-branch right-branch)
    (make-node
     (make-sized-label (+ 1
                          (sized-tree-size left-branch)
                          (sized-tree-size right-branch))
                       label)
     left-branch right-branch)))                                    

; Signatur für größenannotierte Knoten
(define sized-node-of
  (lambda (label leaf)
    (node-of leaf (sized-label-of label))))

; Signatur für größenannotierte Bäume
(define sized-tree-of
  (lambda (leaf label)
    (tree-of leaf (sized-label-of label))))

; Größe eines größenannotierten Baums liefern
(: sized-tree-size ((tree-of %leaf (sized-label-of %label)) -> natural))

(check-expect (sized-tree-size "A") 0)
(check-expect (sized-tree-size sized-tree1) 7)

(define sized-tree-size
  (lambda (tree)
    (cond
      ((node? tree)
       (sized-label-size (node-label tree)))
      (else 0))))

; einfache Rotation nach links
(: single-left
   (%label (sized-tree-of %leaf %label) (sized-node-of %leaf %label)
           -> (sized-node-of %leaf %label)))


(check-expect
 (single-left "a" "X" (make-sized-node "b" "Y" "Z"))
 (make-sized-node "b"
                  (make-sized-node "a" "X" "Y")
                  "Z"))

(define single-left
  (lambda (a X node)
    (define b (sized-node-label node))
    (define Y (node-left-branch node))
    (define Z (node-right-branch node))
    (make-sized-node b
                     (make-sized-node a X Y)
                     Z)))

(define sized-node-label
  (lambda (node)
    (sized-label-label (node-label node))))

; doppelte Rotation nach links
(: double-left
   (%label (sized-tree-of %leaf %label) (sized-node-of %leaf %label)
           -> (sized-node-of %leaf %label)))

(check-expect
 (double-left "a" "X"
              (make-sized-node "c"
                               (make-sized-node "b" "Y1" "Y2")
                               "Z"))
 (make-sized-node "b"
                  (make-sized-node "a" "X" "Y1")
                  (make-sized-node "c" "Y2" "Z"))) 

(define double-left
  (lambda (a X node)
    (define node2 (node-left-branch node))
    (define b (sized-node-label node2))
    (define c (sized-node-label node))
    (define Y1 (node-left-branch node2))
    (define Y2 (node-right-branch node2))
    (define Z (node-right-branch node))
    (make-sized-node b
                     (make-sized-node a X Y1)
                     (make-sized-node c Y2 Z))))


; einfache Rotation nach rechts
(: single-right
   (%label (sized-node-of %leaf %label) (sized-tree-of %leaf %label)
           -> (sized-node-of %leaf %label)))

(check-expect
 (single-right "b" (make-sized-node "a" "X" "Y") "Z")
 (make-sized-node "a"
                  "X"
                  (make-sized-node "b" "Y" "Z")))

(define single-right
  (lambda (b node Z)
    (define a (sized-node-label node))
    (define X (node-left-branch node))
    (define Y (node-right-branch node))
    (make-sized-node a
                     X (make-sized-node b Y Z))))

; doppelte Rotation nach rechts
(: double-right
   (%label (sized-node-of %leaf %label) (sized-tree-of %leaf %label)
           -> (sized-node-of %leaf %label)))

(check-expect
 (double-right "c"
               (make-sized-node "a"
                                "X"
                                (make-sized-node "b" "Y1" "Y2"))
               "Z")
 (make-sized-node "b"
                  (make-sized-node "a" "X" "Y1")
                  (make-sized-node "c" "Y2" "Z")))
                                                 

(define double-right
  (lambda (c node Z)
    (define node2 (node-right-branch node))
    (define a (sized-node-label node))
    (define b (sized-node-label node2))
    (define X (node-left-branch node))
    (define Y1 (node-left-branch node2))
    (define Y2 (node-right-branch node2))
    (make-sized-node b
                     (make-sized-node a X Y1)
                     (make-sized-node c Y2 Z))))

(define ratio 5)

(define make-balanced-node
  (lambda (label left-branch right-branch)
    (define ln (sized-tree-size left-branch))
    (define rn (sized-tree-size right-branch))
    (cond
      ((< (+ ln rn) 2)
       (make-node (make-sized-label (+ 1 ln rn) label)
                  left-branch right-branch))
      ((> rn (* ratio ln)) ; right is too big
       (define rl (node-left-branch right-branch))
       (define rr (node-right-branch right-branch))
       (define rln (sized-tree-size rl))
       (define rrn (sized-tree-size rr))
       (if (< rln rrn)
           (single-left label left-branch right-branch)
           (double-left label left-branch right-branch)))
      ((> ln (* ratio rn)) ; left is too big
       (define ll (node-left-branch left-branch))
       (define lr (node-right-branch left-branch))
       (define lln (sized-tree-size ll))
       (define lrn (sized-tree-size lr))
       (if (< lrn lln)
           (single-right label left-branch right-branch)
           (double-right label left-branch right-branch)))
      (else
       (make-sized-node label left-branch right-branch)))))

; Ein balancierter Suchbaum besteht aus
; - Funktion für =
; - Funktion für <
; - Binärbaum mit Größe im Label
(define-record (sized-search-tree-of element)
  make-sized-search-tree sized-search-tree?
  (sized-search-tree-label-=?-function (element element -> boolean))
  (sized-search-tree-label-<?-function (element element -> boolean))
  (sized-search-tree-tree (sized-tree-of false element)))

(define make-empty-sized-search-tree
  (lambda (label-=?-function label-<?-function)
    (make-sized-search-tree label-=?-function label-<?-function
                               #f)))

(define balanced-search-tree-insert
  (lambda (value search-tree)
    (define label=? (sized-search-tree-label-=?-function search-tree))
    (define label<? (sized-search-tree-label-<?-function search-tree))
    (define tree-insert
      (lambda (value tree)
        (cond
          ((node? tree)
           (cond
             ((label=? value (sized-node-label tree))
              tree)
             ((label<? value (sized-node-label tree))
              (make-balanced-node (sized-node-label tree)
                                  (tree-insert value (node-left-branch tree))
                                  (node-right-branch tree)))
             (else
              (make-balanced-node (sized-node-label tree)
                                  (node-left-branch tree)
                                  (tree-insert value (node-right-branch tree))))))
          (else
           (make-sized-node value #f #f)))))
    (make-sized-search-tree
     label=? label<?
     (tree-insert value (sized-search-tree-tree search-tree)))))




(define balanced-search-tree2
  (balanced-search-tree-insert
   5
   (balanced-search-tree-insert
    17
    (balanced-search-tree-insert
     3
     (make-empty-sized-search-tree = <)))))


(define sized-search-tree1
  (make-sized-search-tree
   string=? string<?
   (make-sized-node "M"
              (make-sized-node "B"
                               (make-sized-node "A" #f #f)
                               (make-sized-node "D" #f #f))
              (make-sized-node "O"
                               (make-sized-node "N" #f #f)
                               (make-sized-node "R" #f #f)))))

; festellen, ob Element in Suchbaum vorhanden ist
(: sized-search-tree-member? (%a (sized-search-tree-of %a) -> boolean))
(check-expect (sized-search-tree-member? "M" sized-search-tree1) #t)
(check-expect (sized-search-tree-member? "D" sized-search-tree1) #t)
(check-expect (sized-search-tree-member? "N" sized-search-tree1) #t)
(check-expect (sized-search-tree-member? "R" sized-search-tree1) #t)
(check-expect (sized-search-tree-member? "Z" sized-search-tree1) #f)

(define sized-search-tree-member?
  (lambda (value search-tree)
    (define label=? (sized-search-tree-label-=?-function search-tree))
    (define label<? (sized-search-tree-label-<?-function search-tree))
    (define tree-member?
      (lambda (value tree)
        (cond
          ((node? tree)
           (define label (sized-node-label tree))
           (cond
             ((label=? value label) #t)
             ((label<? value label)
              (tree-member? value (node-left-branch tree)))
             (else
              (tree-member? value (node-right-branch tree)))))
          (else #f))))
    (tree-member? value (sized-search-tree-tree search-tree))))

; aus allen Elementen einer Liste einen Suchbaum machen
(: list->balanced-search-tree ((%a %a -> boolean) (%a %a -> boolean)
                                                  (list-of %a) -> (sized-search-tree-of %a)))

(check-property
 (for-all ((elements (list-of natural)))
   (let ((search-tree (list->balanced-search-tree = < elements)))
     (every? (lambda (element)
               (sized-search-tree-member? element search-tree))
             elements))))

(check-property
 (for-all ((elements (list-of natural))
           (element natural))
   (==> (not (member? = element elements))
        (not (sized-search-tree-member? element (list->balanced-search-tree = < elements)))))) 

(define list->balanced-search-tree
  (lambda (= < elements)
    (fold (make-empty-sized-search-tree = <)
          balanced-search-tree-insert
          elements)))
