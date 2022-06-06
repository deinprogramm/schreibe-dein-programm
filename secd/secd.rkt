#lang deinprogramm/sdp/advanced

; Kapitel "Die SECD-Maschine"

; Ein Lambda-Term ist eins der folgenden:
; - ein Symbol (für eine Variable)
; - eine zweielementige Liste (für eine reguläre Applikation)
; - eine Liste der Form (lambda (x) e) (für eine Abstraktion)
; - ein Basiswert
; - eine Liste mit einem Primitivum als erstem Element
;      (für eine primitive Applikation)

(define term
  (signature
   (mixed symbol
	  application
	  abstraction
	  base
	  primitive-application
	  assignment)))

; Ein Basiswert ist ein boolescher Wert oder eine Zahl
; Prädikat für Basiswerte
(: base? (any -> boolean))
(define base?
  (lambda (v)
    (or (boolean? v) (number? v))))

(define base (signature (predicate base?)))

; Ein Primitivum ist eins der Symbole +, -, *, /, =
; Prädikat für Primitive
(: primitive? (any -> boolean))

(check-expect (primitive? '+) #t)
(check-expect (primitive? 'foo) #f)

(define primitive?
  (lambda (s)
    (or (equal? '+ s)
        (equal? '- s)
        (equal? '* s)
        (equal? '/ s)
        (equal? '= s))))

(define primitive (signature (predicate primitive?)))

; Prädikat für reguläre Applikationen
(: application? (any -> boolean))
(define application?
  (lambda (t)
    (and (cons? t)
         (not (equal? 'set! (first t)))
         (not (equal? 'lambda (first t)))
         (not (primitive? (first t))))))

(define application (signature (predicate application?)))

; Prädikat für Abstraktionen
(: abstraction? (any -> boolean))
(define abstraction?
  (lambda (t)
    (and (cons? t)
         (equal? 'lambda (first t)))))

(define abstraction (signature (predicate abstraction?)))

; Prädikat für primitive Applikationen
(: primitive-application? (any -> boolean))
(define primitive-application?
  (lambda (t)
    (and (cons? t)
         (primitive? (first t)))))

(define primitive-application (signature (predicate primitive-application?)))

; Prädikat für Zuweisungen
(: assignment? (any -> boolean))
(define assignment?
  (lambda (t)
    (and (cons? t)
         (equal? 'set! (first t)))))

(define assignment (signature (predicate assignment?)))

; Eine Instruktion ist eins der folgenden:
; - ein Basiswert
; - eine Variable
; - eine Applikations-Instruktion
; - eine endrekursive Applikations-Instruktion
; - eine Instruktion für eine primitive Applikation
; - eine Abstraktion

(define instruction
  (signature
   (mixed base
	  symbol
	  ap
	  tailap
	  prim
	  abst
	  :=)))

; Eine Maschinencode-Programm ist eine Liste von Instruktionen.

(define machine-code (signature (list-of instruction)))

; Eine Applikations-Instruktion ist ein Wert
;   (make-ap)
(define-record ap
  make-ap ap?)
(: make-ap (-> ap))

; Eine endrekursive Applikations-Instruktion ist ein Wert
;   (make-tailap)
(define-record tailap
  make-tailap tailap?)

; Eine Zuweisungs-Instruktion ist ein Wert
;  (make-:=)
(define-record :=
  make-:= :=?)

; Die Instruktion für eine primitive Applikation
; ist ein Wert
;   (real-make-prim op arity)
; wobei op ein Symbol und arity die Stelligkeit
; ist
(define-record prim
  real-make-prim prim?
  (prim-operator symbol)
  (prim-arity natural))

; Primitiv erzeugen
(: make-prim (symbol -> prim))
(define make-prim
  (lambda (s)
    (real-make-prim s 2))) ; alle haben derzeit Stelligkeit 2

; Eine Abstraktions-Instruktion ist ein Wert
;  (make-abs v c)
; wobei v ein Symbol (für eine Variable) und c
; Maschinencode ist
(define-record abst
  make-abs abs?
  (abs-variable symbol)
  (abs-code machine-code))

; Term in Maschinencode übersetzen
(: term->machine-code (term -> machine-code))

(check-expect (term->machine-code '((lambda (x) (x x)) (lambda (x) (x x))))
              (list (make-abs 'x (list 'x 'x (make-ap))) (make-abs 'x (list 'x 'x (make-ap))) (make-ap)))
(check-expect (term->machine-code '(+ 1 2))
              (list 1 2 (make-prim '+)))

(define term->machine-code
  (lambda (e)
    (cond
      ((symbol? e) (list e))
      ((application? e)
       (append (term->machine-code (first e))
               (append (term->machine-code (first (rest e)))
                       (list (make-ap)))))
      ((abstraction? e)
       (list
        (make-abs (first (first (rest e)))
                  (term->machine-code
                   (first (rest (rest e)))))))
      ((base? e) (list e))
      ((primitive-application? e)
       (append
        (append-lists
         (map term->machine-code (rest e)))
        (list (make-prim (first e)))))
      ((assignment? e)
       (cons (first (rest e))
             (append (term->machine-code (first (rest (rest e))))
                     (list (make-:=))))))))

; die Elemente einer Liste von Listen aneinanderhängen
(: append-lists ((list-of (list-of %a)) -> (list-of %a)))
(define append-lists
  (lambda (l)
    (fold '() append l)))

; Term in Maschinencode übersetzen
;  in nicht-endrekursivem Kontext
(: term->machine-code/t (term -> machine-code))
(define term->machine-code/t
  (lambda (e)
    (cond
      ((symbol? e) (list e))
      ((application? e)
       (append (term->machine-code/t (first e))
               (append (term->machine-code/t (first (rest e)))
                       (list (make-ap)))))
      ((abstraction? e)
       (list
        (make-abs (first (first (rest e)))
                  (term->machine-code/t-t
                   (first (rest (rest e)))))))
      ((base? e) (list e))
      ((primitive-application? e)
       (append
        (append-lists
         (map term->machine-code/t (rest e)))
        (list (make-prim (first e)))))
      ((assignment? e)
       (cons (first (rest e))
             (append (term->machine-code/t (first (rest (rest e))))
                     (list (make-:=))))))))

; Term in Maschinencode übersetzen
;  in endrekursivem Kontext
(: term->machine-code/t-t (term -> machine-code))

(check-expect (term->machine-code/t '((lambda (x) (x x)) (lambda (x) (x x))))
              (list (make-abs 'x (list 'x 'x (make-tailap))) (make-abs 'x (list 'x 'x (make-tailap))) (make-ap)))
(check-expect (term->machine-code/t '(+ 1 2))
              (list 1 2 (make-prim '+)))

(define term->machine-code/t-t
  (lambda (e)
    (cond
      ((symbol? e) (list e))
      ((application? e)
       (append (term->machine-code/t (first e))
               (append (term->machine-code/t (first (rest e)))
                       (list (make-tailap)))))
      ((abstraction? e)
       (list
        (make-abs (first (first (rest e)))
                  (term->machine-code/t-t
                   (first (rest (rest e)))))))
      ((base? e) (list e))
      ((primitive-application? e)
       (append
        (append-lists
         (map term->machine-code/t (rest e)))
        (list (make-prim (first e)))))
      ((assignment? e)
       (cons (first (rest e))
             (append (term->machine-code/t (first (rest (rest e))))
                     (list (make-:=))))))))

; Ein Stack ist eine Liste von Werten
(define stack (signature (list-of value)))

; Eine Umgebung ist eine Liste von Bindungen.
; Dabei gibt es für jede Variable nur eine Bindung.
(define environment (signature (list-of binding)))

; Eine Bindung (Name: binding) ist ein Wert
;  (make-binding v x)
; wobei v der Name einer Variablen und x der dazugehörige Wert ist.

(define-record binding
  make-binding binding?
  (binding-variable symbol)
  (binding-value value))

; die leere Umgebung
(: the-empty-environment environment)
(define the-empty-environment empty)

; eine Umgebung um eine Bindung erweitern
(: extend-environment (environment symbol value -> environment))
(define extend-environment
  (lambda (e v w)
    (cons (make-binding v w)
          (remove-environment-binding e v))))

; die Bindung für eine Variable aus einer Umgebung entfernen
(: remove-environment-binding (environment symbol -> environment))
(define remove-environment-binding
  (lambda (e v)
    (cond
      ((empty? e) empty)
      ((cons? e)
       (if (equal? v (binding-variable (first e)))
           (rest e)
           (cons (first e)
                 (remove-environment-binding (rest e) v)))))))
  
; die Bindung für eine Variable in einer Umgebung finden
(: lookup-environment (environment symbol -> value))
(define lookup-environment
  (lambda (e v)
    (cond
      ((empty? e) (violation "unbound variable"))
      ((cons? e)
       (if (equal? v (binding-variable (first e)))
           (binding-value (first e))
           (lookup-environment (rest e) v))))))

; Ein Dump ist eine Liste von Frames.
(define dump (signature (list-of frame)))

; Ein Frame ist ein Wert
;  (make-frame s e c)
; wobei s ein Stack, e eine Umgebung und c Maschinencode ist.
(define-record frame
  make-frame frame?
  (frame-stack stack)
  (frame-environment environment)
  (frame-code machine-code))

; Ein SECD-Wert ist ein Basiswert oder eine Closure
(define value (signature (mixed base closure void)))

; Eine Closure ist ein Wert
;  (make-closure v c e)
; wobei v die Variable der Lambda-Abstraktion, c der Code der Lambda-Abstraktion
; und e ein Environment ist.
(define-record closure
  make-closure closure?
  (closure-variable symbol)
  (closure-code machine-code)
  (closure-environment environment))

; Ein SECD-Zustand ist ein Wert
;  (make-secd s e c d)
; wobei s ein Stack, e eine Umgebung, c Maschinencode
; und d ein Dump ist
(define-record secd
  make-secd secd?
  (secd-stack stack)
  (secd-environment environment)
  (secd-code machine-code)
  (secd-dump dump))

; Zustandsübergang berechnen
(: secd-step (secd -> secd))
(define secd-step
  (lambda (state)
    (let ((stack (secd-stack state))
          (environment (secd-environment state))
          (code (secd-code state))
          (dump (secd-dump state)))
      (cond
        ((cons? code)
         (cond
           ((base? (first code))
            (make-secd (cons (first code) stack)
                       environment
                       (rest code)
                       dump))
           ((symbol? (first code))
            (make-secd (cons (lookup-environment environment (first code)) stack)
                       environment
                       (rest code)
                       dump))
           ((prim? (first code))
            (make-secd (cons
                        (apply-primitive (prim-operator (first code))
                                         (take-reverse (prim-arity (first code)) stack))
                        (drop (prim-arity (first code)) stack))
                       environment
                       (rest code)
                       dump))
           ((abs? (first code))
            (make-secd (cons (make-closure (abs-variable (first code))
                                           (abs-code (first code))
                                           environment)
                                  stack)
                       environment
                       (rest code)
                       dump))
           ((ap? (first code))
            (let ((closure (first (rest stack))))
              (make-secd empty  
                         (extend-environment
                          (closure-environment closure)
                          (closure-variable closure)
                          (first stack))
                         (closure-code closure)
                         (cons
                          (make-frame (rest (rest stack)) environment (rest code))
                          dump))))
           ((tailap? (first code))
            (let ((closure (first (rest stack))))
              (make-secd (rest (rest stack))
                         (extend-environment
                          (closure-environment closure)
                          (closure-variable closure)
                          (first stack))
                         (closure-code closure)
                         dump)))))
        ((empty? code)
         (let ((f (first dump)))
           (make-secd
            (cons (first stack)
                  (frame-stack f))
            (frame-environment f)
            (frame-code f)
            (rest dump))))))))


; Delta-Transition berechnen
(: apply-primitive (primitive (list-of value) -> value))

(check-expect (apply-primitive '+ '(1 2)) 3)
(check-expect (apply-primitive '- '(2 1)) 1)

(define apply-primitive
  (lambda (p args)
    (cond
      ((equal? p '+)
       (+ (first args) (first (rest args))))
      ((equal? p '-)
       (- (first args) (first (rest args))))
      ((equal? p '=)
       (= (first args) (first (rest args))))
      ((equal? p '*)
       (* (first args) (first (rest args))))
      ((equal? p '/)
       (/ (first args) (first (rest args)))))))

; die ersten Elemente einer Liste in umgekehrter Reihenfolge berechnen
(: take-reverse (natural (list-of %a) -> (list-of %a)))

(check-expect (take-reverse 2 '(1 2 3 4 5)) '(2 1))
(check-expect (take-reverse 0 '(1 2 3 4 5)) '())
(check-expect (take-reverse 5 '(1 2 3 4 5)) '(5 4 3 2 1))

(define take-reverse
  (lambda (n l)
    ;; (: loop (natural (list-of a) (list-of a) -> (list-of a)))
    (letrec ((loop (lambda (n l r)
                     (if (= n 0)
                         r
                         (loop (- n 1) (rest l) (cons (first l) r))))))
      (loop n l '()))))

; die ersten Elemente einer Liste weglassen
(: drop (natural (list-of %a) -> (list-of %a)))

(check-expect (drop 2 '(1 2 3 4 5)) '(3 4 5))
(check-expect (drop 0 '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (drop 5 '(1 2 3 4 5)) '())

(define drop
  (lambda (n l)
    (if (= n 0)
        l
        (drop (- n 1) (rest l)))))

; Aus Term SECD-Anfangszustand machen
(: inject-secd (term -> secd))
(define inject-secd
  (lambda (e)
    (make-secd empty
               the-empty-environment
               (term->machine-code/t e)
               empty)))

; bis zum Ende Zustandsübergänge berechnen
(: secd-step* (secd -> secd))
(define secd-step*
  (lambda (state)
    (if (and (empty? (secd-code state))
             (empty? (secd-dump state)))
        state
        (secd-step* (secd-step state)))))


; Evaluationsfunktion zur SECD-Maschine berechnen
(: eval-secd (term -> (mixed value (one-of 'function))))

(check-expect (eval-secd '(+ 1 2)) 3)
(check-expect (eval-secd '(((lambda (x) (lambda (y) (+ x y))) 1) 2)) 3)

(define eval-secd
  (lambda (e)
    (let ((val (first
                (secd-stack
                 (secd-step* 
                  (inject-secd e))))))
      (if (base? val)
          val
          'function))))

(define write-secd/tex
  (lambda (state)
    (begin
      (write-string "(")
      (write-stack/tex (secd-stack state))
      (write-string ", &")
      (write-environment/tex (secd-environment state))
      (write-string ", &")
      (write-code/tex (secd-code state))
      (write-string ", &")
      (write-dump/tex (secd-dump state))
      (write-string ")"))))

(define write-stack/tex
  (lambda (stack)
    (cond
      ((empty? stack) (write-string "\\epsilon"))
      ((cons? stack)
       (begin
         (write-value/tex (first stack))
         (for-each (lambda (val)
                     (begin
                       (write-string "~")
                       (write-value/tex val)))
                   (rest stack)))))))

(define write-value/tex
  (lambda (value)
    (cond
      ((base? value)
       (cond
         ((number? value) (write-string (number->string value)))
         ((boolean? value)
          (if value
              (write-string "\\textit{true}")
              (write-string "\\textit{false}")))))
      ((closure? value)
       (begin
         (write-string "(")
         (write-string (symbol->string (closure-variable value)))
         (write-string ", ")
         (write-code/tex (closure-code value))
         (write-string ", ")
         (write-environment/tex (closure-environment value))
         (write-string ")"))))))

(define write-environment/tex
  (lambda (e)
    (cond
      ((empty? e) (write-string "\\varnothing"))
      ((cons? e)
       (begin
         (write-string "\\{")
         (write-binding/tex (first e))
         (for-each (lambda (b)
                     (begin
                       (write-string ", ")
                       (write-binding/tex b)))
                   (rest e))
         (write-string "\\}"))))))

(define write-binding/tex
  (lambda (b)
    (begin
      (write-string "(")
      (write-string (symbol->string (binding-variable b)))
      (write-string ", ")
      (write-value/tex (binding-value b))
      (write-string ")"))))

(define write-code/tex
  (lambda (c)
    (cond
      ((empty? c) (write-string "\\epsilon"))
      ((cons? c)
       (begin
         (write-instruction/tex (first c))
         (for-each (lambda (i)
                     (begin
                       (write-string "~")
                       (write-instruction/tex i)))
                   (rest c)))))))

(define write-instruction/tex
  (lambda (i)
    (cond
      ((base? i) (write-value/tex i))
      ((symbol? i) (write-string (symbol->string i)))
      ((prim? i)
       (begin
         (write-string "\\mathtt{prim}_")
         (write-string (symbol->string (prim-operator i)))))
      ((abs? i)
       (begin
         (write-string "(")
         (write-string (symbol->string (abs-variable i)))
         (write-string ", ")
         (write-code/tex (abs-code i))
         (write-string ")")))
      ((ap? i)
       (write-string "\\mathtt{ap}"))
      ((tailap? i)
       (write-string "\\mathtt{tailap}")))))

(define write-dump/tex
  (lambda (d)
    (cond
      ((empty? d) (write-string "\\epsilon"))
      ((cons? d)
       (begin
         (write-frame/tex (first d))
         (for-each (lambda (f)
                     (begin
                       (write-string "~")
                       (write-frame/tex f)))
                   (rest d)))))))

(define write-frame/tex
  (lambda (f)
    (begin
      (write-string "(")
      (write-stack/tex (frame-stack f))
      (write-string ", ")
      (write-environment/tex (frame-environment f))
      (write-string ", ")
      (write-code/tex (frame-code f))
      (write-string ")"))))

(define secd-step*/tex
  (lambda (state)
    (begin
      (write-string "\\hookrightarrow{}&")
      (write-secd/tex state)
      (write-string "\\\\") (write-newline)
      (if (and (empty? (secd-code state))
               (empty? (secd-dump state)))
          state
          (secd-step*/tex (secd-step state))))))

; Ein Stack ist eine Liste aus Adressen.
(define stackh (signature (list-of address)))

; Eine Umgebung bildet Variablen auf Adressen ab.

; Eine Adresse (Name: address) ist eine ganze Zahl.
(define address (signature natural))

; Ein Heap ist ein Wert
;   (make-heap s n)
; wobei n die nächste freie Adresse ist und s eine Liste
; von Zellen.
; Name: heap
(define-record heap
  make-heap heap?
  (heap-cells (list-of cell))
  (heap-next natural))

(define the-empty-heap (make-heap empty 0))

; Eine Zelle ist ein Wert
;   (make-cell a w)
; wobei a eine Adresse und w ein Wert ist
(define-record cell
  make-cell cell?
  (cell-address address)
  (cell-value value))

; Wert im Speicher ablegen
(: heap-store (heap address value -> heap))
(define heap-store
  (lambda (h a w)
    (make-heap (cons (make-cell a w)
                     (remove-cell a (heap-cells h)))
               (let ((next (heap-next h)))
                 (if (= a next)
                     (+ next 1)
                     next)))))

; Zelle zu einer Adresse entfernen
(: remove-cell (address (list-of cell) -> (list-of cell)))
(define remove-cell
  (lambda (a c)
    (cond
      ((empty? c) empty)
      ((cons? c)
       (if (= a (cell-address (first c)))
           (rest c)
           (cons (first c)
                 (remove-cell a (rest c))))))))

; den Wert an einer Adresse im Heap nachschauen
(: heap-lookup (heap address -> value))
(define heap-lookup
  (lambda (h a)
    (cells-lookup (heap-cells h) a)))

; den Wert an einer Adresse in einer Liste von Zellen nachschauen
(: cells-lookup ((list-of cell) address -> value))
(define cells-lookup
  (lambda (c a)
    (cond
      ((empty? c) (violation "unassigned address"))
      ((cons? c)
       (if (= a (cell-address (first c)))
           (cell-value (first c))
           (cells-lookup (rest c) a))))))

; Ein void-Wert ist ein Wert
;  (make-void)
(define-record void
  make-void void?)

(define the-void (make-void))

; Ein SECDH-Zustand ist ein Wert
;   (make-secd s e c d h)
; wobei s ein Stack, e eine Umgebung, c Maschinencode,
; d ein Dump und h ein Speicher ist.
; Name: secdh
(define-record secdh
  make-secdh secdh?
  (secdh-stack stackh)
  (secdh-environment environment)
  (secdh-code machine-code)
  (secdh-dump dump)
  (secdh-heap heap))

; eine Zustandstransition berechnen
(: secdh-step (secdh -> secdh))
(define secdh-step
  (lambda (state)
    (let ((stack (secdh-stack state))
          (environment (secdh-environment state))
          (code (secdh-code state))
          (dump (secdh-dump state))
          (heap (secdh-heap state)))
      (cond
        ((cons? code)
         (cond
           ((base? (first code))
            (let ((a (heap-next heap)))
              (make-secdh
               (cons a stack)
               environment
               (rest code)
               dump
               (heap-store heap a (first code)))))
           ((symbol? (first code))
            (make-secdh
             (cons (lookup-environment environment (first code)) stack)
             environment
             (rest code)
             dump
             heap))
           ((prim? (first code))
            (let ((a (heap-next heap)))
              (make-secdh
               (cons a
                     (drop (prim-arity (first code)) stack))
               environment
               (rest code)
               dump
               (heap-store heap a
                           (apply-primitive
                            (prim-operator (first code))
                            (map (lambda (address)
                                   (heap-lookup heap address))
                                 (take-reverse (prim-arity (first code)) stack)))))))
           ((:=? (first code))
            (let ((a (heap-next heap)))
              (make-secdh
               (cons a (rest (rest stack)))
               environment
               (rest code)
               dump
               (heap-store
                (heap-store heap
                            (first (rest stack)) 
                            (heap-lookup heap (first stack)))
                a the-void))))
           ((abs? (first code))
            (let ((a (heap-next heap)))
              (make-secdh
               (cons a stack)
               environment
               (rest code)
               dump
               (heap-store heap a
                           (make-closure (abs-variable (first code))
                                         (abs-code (first code))
                                         environment)))))
           ((ap? (first code))
            (let ((closure (heap-lookup heap (first (rest stack))))
                  (a (heap-next heap)))
              (make-secdh empty
                          (extend-environment
                           (closure-environment closure)
                           (closure-variable closure)
                           a)
                          (closure-code closure)
                          (cons
                           (make-frame (rest (rest stack)) environment (rest code))
                           dump)
                          (heap-store heap a (heap-lookup heap (first stack))))))
           ((tailap? (first code))
            (let ((closure (heap-lookup heap (first (rest stack))))
                  (a (heap-next heap)))
              (make-secdh (rest (rest stack))
                          (extend-environment
                           (closure-environment closure)
                           (closure-variable closure)
                           a)
                          (closure-code closure)
                          dump
                          (heap-store heap a (heap-lookup heap (first stack))))))))
        ((empty? code)
         (let ((f (first dump)))
           (make-secdh
            (cons (first stack)
                  (frame-stack f))
            (frame-environment f)
            (frame-code f)
            (rest dump)
            heap)))))))
    
; bis zum Ende Zustandsübergänge berechnen
(: secdh-step* (secdh -> secdh))
(define secdh-step*
  (lambda (state)
    (if (and (empty? (secdh-code state))
             (empty? (secdh-dump state)))
        state
        (secdh-step* (secdh-step state)))))

; aus Term SECDH-Anfangszustand machen
(: inject-secdh (term -> secdh))
(define inject-secdh
  (lambda (e)
    (make-secdh empty
                the-empty-environment
                (term->machine-code e)
                empty
               the-empty-heap)))

; Evaluationsfunktion zur SECD-Maschine berechnen
(: eval-secdh (term -> (mixed value (one-of 'function))))

(check-expect (eval-secdh '(+ 1 2)) 3)
(check-expect (eval-secdh '(((lambda (x) (lambda (y) (+ x y))) 1) 2)) 3)
(check-expect (eval-secdh '((lambda (x) ((lambda (y) x) (set! x (+ x 1)))) 12)) 13)

(define eval-secdh
  (lambda (e)
    (let ((final (secdh-step* (inject-secdh e))))
      (let ((val (heap-lookup (secdh-heap final)
                              (first (secdh-stack final)))))
        (if (base? val)
            val
            'function)))))

(define write-secdh/tex
  (lambda (state)
    (begin
      (write-string "(")
      (write-stackh/tex (secdh-stack state))
      (write-string ", &")
      (write-environmenth/tex (secdh-environment state))
      (write-string ", &")
      (write-codeh/tex (secdh-code state))
      (write-string ", &")
      (write-dumph/tex (secdh-dump state))
      (write-string ", &")
      (write-heap/tex (secdh-heap state))
      (write-string ")"))))

(define write-stackh/tex
  (lambda (stack)
    (cond
      ((empty? stack) (write-string "\\epsilon"))
      ((cons? stack)
       (begin
         (write-address/tex (first stack))
         (for-each (lambda (val)
                     (begin
                       (write-string "~")
                       (write-address/tex val)))
                   (rest stack)))))))

(define write-address/tex
  (lambda (addr)
    (begin
      (write-string "\\langle{}")
      (write-string (number->string addr))
      (write-string "\\rangle{}"))))

(define write-environmenth/tex
  (lambda (e)
    (cond
      ((empty? e) (write-string "\\varnothing"))
      ((cons? e)
       (begin
         (write-string "\\{")
         (write-bindingh/tex (first e))
         (for-each (lambda (b)
                     (begin
                       (write-string ", ")
                       (write-bindingh/tex b)))
                   (rest e))
         (write-string "\\}"))))))

(define write-bindingh/tex
  (lambda (b)
    (begin
      (write-string "(")
      (write-string (symbol->string (binding-variable b)))
      (write-string ", ")
      (write-address/tex (binding-value b))
      (write-string ")"))))

(define write-codeh/tex
  (lambda (c)
    (cond
      ((empty? c) (write-string "\\epsilon"))
      ((cons? c)
       (begin
         (write-instructionh/tex (first c))
         (for-each (lambda (i)
                     (begin
                       (write-string "~")
                       (write-instructionh/tex i)))
                   (rest c)))))))

(define write-instructionh/tex
  (lambda (i)
    (cond
      ((:=? i)
       (write-string "\\mathtt{:=}"))
      ((abs? i)
       (begin
         (write-string "(")
         (write-string (symbol->string (abs-variable i)))
         (write-string ", ")
         (write-codeh/tex (abs-code i))
         (write-string ")")))
      (else
       (write-instruction/tex i)))))

(define write-dumph/tex
  (lambda (d)
    (cond
      ((empty? d) (write-string "\\epsilon"))
      ((cons? d)
       (begin
         (write-frameh/tex (first d))
         (for-each (lambda (f)
                     (begin
                       (write-string "~")
                       (write-frameh/tex f)))
                   (rest d)))))))

(define write-frameh/tex
  (lambda (f)
    (begin
      (write-string "(")
      (write-stackh/tex (frame-stack f))
      (write-string ", ")
      (write-environmenth/tex (frame-environment f))
      (write-string ", ")
      (write-codeh/tex (frame-code f))
      (write-string ")"))))

(define write-heap/tex
  (lambda (h)
    (let ((cells (heap-cells h)))
      (if (empty? cells)
	  (write-string "\\varnothing")
	  (begin
	    (write-cell/tex (first cells))
	    (for-each (lambda (cell)
                        (begin
                          (write-string ", ")
                          (write-cell/tex cell)))
		      (rest cells)))))))

(define write-cell/tex
  (lambda (c)
    (begin
      (write-string "(")
      (write-address/tex (cell-address c))
      (write-string ", ")
      (write-valueh/tex (cell-value c)))))

(define write-valueh/tex
  (lambda (value)
    (cond
     ((void? value)
      (write-string "\\mathtt{void}"))
      ((closure? value)
       (begin
         (write-string "(")
         (write-string (symbol->string (closure-variable value)))
         (write-string ", ")
         (write-codeh/tex (closure-code value))
         (write-string ", ")
         (write-environmenth/tex (closure-environment value))
         (write-string ")")))
      (else
       (write-value/tex value)))))

(define secdh-step*/tex
  (lambda (state)
    (begin
      (write-string "\\hookrightarrow{}&")
      (write-secdh/tex state)
      (write-string "\\\\") (write-newline)
      (if (and (empty? (secdh-code state))
               (empty? (secdh-dump state)))
          state
          (secdh-step*/tex (secdh-step state))))))

;(secdh-step*/tex (inject-secdh '((lambda (x) ((lambda (y) x) (set! x (+ x 1)))) 12)))
;(secdh-step*/tex (inject-secdh '((lambda (x) (((lambda (f) (lambda (g) (f (g x)))) (lambda (n) (set! n (* n n)))) (lambda (m) (+ m 1)))) 2)))
(secdh-step*/tex (inject-secdh '((lambda (x) ((lambda (f) (f x)) (lambda (n) (set! n (* n n))))) 3)))
