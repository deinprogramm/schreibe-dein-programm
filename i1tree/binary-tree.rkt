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
  (lambda (leaf node-label)
    (signature (mixed leaf (node-of leaf node-label)))))

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
(define-record (search-tree-of a)
  make-search-tree search-tree?
  (search-tree-label-=?-function (a a -> boolean))
  (search-tree-label-<?-function (a a -> boolean))
  (search-tree-tree (tree-of false a)))

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

