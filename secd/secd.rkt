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
	  primitive-application
          base
	  assignment)))

; Ein Basiswert ist ein boolescher Wert oder eine Zahl
; Prädikat für Basiswerte
(: base? (any -> boolean))
(define base?
  (lambda (term)
    (or (boolean? term) (number? term))))

(define base (signature (predicate base?)))

; Ein Primitivum ist eins der Symbole +, -, *, /, =
; Prädikat für Primitive
(: primitive? (any -> boolean))

(check-expect (primitive? '+) #t)
(check-expect (primitive? 'foo) #f)

(define primitive?
  (lambda (term)
    (or (equal? '+ term)
        (equal? '- term)
        (equal? '* term)
        (equal? '/ term)
        (equal? '= term))))

(define primitive (signature (predicate primitive?)))

; Prädikat für reguläre Applikationen
(: application? (any -> boolean))
(define application?
  (lambda (term)
    (and (cons? term)
         (not (equal? 'set! (first term)))
         (not (equal? 'lambda (first term)))
         (not (primitive? (first term))))))

(define application (signature (predicate application?)))

; Prädikat für Abstraktionen
(: abstraction? (any -> boolean))
(define abstraction?
  (lambda (term)
    (and (cons? term)
         (equal? 'lambda (first term)))))

(define abstraction (signature (predicate abstraction?)))

; Prädikat für primitive Applikationen
(: primitive-application? (any -> boolean))
(define primitive-application?
  (lambda (term)
    (and (cons? term)
         (primitive? (first term)))))

(define primitive-application (signature (predicate primitive-application?)))

; Prädikat für Zuweisungen
(: assignment? (any -> boolean))
(define assignment?
  (lambda (term)
    (and (cons? term)
         (equal? 'set! (first term)))))

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

; Applikations-Instruktion
(define-record ap
  make-ap ap?)

; Eine endrekursive Applikations-Instruktion ist ein Wert
;   (make-tailap)
(define-record tailap
  make-tailap tailap?)

; Eine Zuweisungs-Instruktion ist ein Wert
;  (make-:=)
(define-record :=
  make-:= :=?)

; Eine Instruktion für eine primitive Applikation hat folgende
; Eigenschaften:
; - Operator
; - Stelligkeit
(define-record prim
  make-prim prim?
  (prim-operator symbol)
  (prim-arity natural))

; Eine Abstraktions-Instruktion hat folgende Eigenschaften:
; - Parameter (eine Variable)
; - Code für den Rumpf
(define-record abst
  make-abs abs?
  (abs-variable symbol)
  (abs-code machine-code))

; Term in Maschinencode übersetzen
(: term->machine-code (term -> machine-code))

(check-expect (term->machine-code '(+ 1 2))
              (list 1 2 (make-prim '+ 2)))
(check-expect (term->machine-code '((lambda (x) (x x)) (lambda (x) (x x))))
              (list (make-abs 'x (list 'x 'x (make-ap))) (make-abs 'x (list 'x 'x (make-ap))) (make-ap)))

(define term->machine-code
  (lambda (term)
    (cond
      ((symbol? term) (list term))
      ((base? term) (list term))
      ((application? term)
       (append (term->machine-code (first term))
               (append (term->machine-code (first (rest term)))
                       (list (make-ap)))))
      ((abstraction? term)
       (list
        (make-abs (first (first (rest term)))
                  (term->machine-code
                   (first (rest (rest term)))))))
      ((primitive-application? term)
       (append
        (append-lists
         (map term->machine-code (rest term)))
        (list (make-prim (first term) (length (rest term))))))
      ((assignment? term)
       (cons (first (rest term))
             (append (term->machine-code (first (rest (rest term))))
                     (list (make-:=))))))))

; die Elemente einer Liste von Listen aneinanderhängen
(: append-lists ((list-of (list-of %a)) -> (list-of %a)))
(define append-lists
  (lambda (list)
    (fold '() append list)))

; Term in Maschinencode übersetzen
;  in nicht-endrekursivem Kontext
(: term->machine-code/t (term -> machine-code))
(define term->machine-code/t
  (lambda (term)
    (cond
      ((symbol? term) (list term))
      ((application? term)
       (append (term->machine-code/t (first term))
               (append (term->machine-code/t (first (rest term)))
                       (list (make-ap)))))
      ((abstraction? term)
       (list
        (make-abs (first (first (rest term)))
                  (term->machine-code/t-t
                   (first (rest (rest term)))))))
      ((base? term) (list term))
      ((primitive-application? term)
       (append
        (append-lists
         (map term->machine-code/t (rest term)))
        (list (make-prim (first term) (length (rest term))))))
      ((assignment? term)
       (cons (first (rest term))
             (append (term->machine-code/t (first (rest (rest term))))
                     (list (make-:=))))))))

; Term in Maschinencode übersetzen
;  in endrekursivem Kontext
(: term->machine-code/t-t (term -> machine-code))

(check-expect (term->machine-code/t '((lambda (x) (x x)) (lambda (x) (x x))))
              (list (make-abs 'x (list 'x 'x (make-tailap))) (make-abs 'x (list 'x 'x (make-tailap))) (make-ap)))
(check-expect (term->machine-code/t '(+ 1 2))
              (list 1 2 (make-prim '+ 2)))

(define term->machine-code/t-t
  (lambda (term)
    (cond
      ((symbol? term) (list term))
      ((application? term)
       (append (term->machine-code/t (first term))
               (append (term->machine-code/t (first (rest term)))
                       (list (make-tailap)))))
      ((abstraction? term)
       (list
        (make-abs (first (first (rest term)))
                  (term->machine-code/t-t
                   (first (rest (rest term)))))))
      ((base? term) (list term))
      ((primitive-application? term)
       (append
        (append-lists
         (map term->machine-code/t (rest term)))
        (list (make-prim (first term) (length (rest term))))))
      ((assignment? term)
       (cons (first (rest term))
             (append (term->machine-code/t (first (rest (rest term))))
                     (list (make-:=))))))))

; Ein Stack ist eine Liste von Werten
(define stack (signature (list-of value)))

; Eine Umgebung ist eine Liste von Bindungen.
; Dabei gibt es für jede Variable nur eine Bindung.
(define environment (signature (list-of binding)))

; Eine Bindung besteht aus:
; - Variable
; - Wert
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
  (lambda (environment variable value)
    (cons (make-binding variable value)
          (remove-environment-binding environment variable))))

; die Bindung für eine Variable aus einer Umgebung entfernen
(: remove-environment-binding (environment symbol -> environment))
(define remove-environment-binding
  (lambda (environment variable)
    (cond
      ((empty? environment) empty)
      ((cons? environment)
       (if (equal? variable (binding-variable (first environment)))
           (rest environment)
           (cons (first environment)
                 (remove-environment-binding (rest environment) variable)))))))
  
; die Bindung für eine Variable in einer Umgebung finden
(: lookup-environment (environment symbol -> value))
(define lookup-environment
  (lambda (environment variable)
    (cond
      ((empty? environment) (violation "unbound variable"))
      ((cons? environment)
       (if (equal? variable (binding-variable (first environment)))
           (binding-value (first environment))
           (lookup-environment (rest environment) variable))))))

; Ein Dump ist eine Liste von Frames.
(define dump (signature (list-of frame)))

; Ein Frame besteht aus:
; - Stack
; - Umgebung
; - Code
(define-record frame
  make-frame frame?
  (frame-stack stack)
  (frame-environment environment)
  (frame-code machine-code))

; Ein SECD-Wert ist ein Basiswert oder eine Closure
(define value (signature (mixed base closure void)))

; Eine Closure besteht aus:
; - Variable
; - Code
; - Umgebung
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
    (define stack (secd-stack state))
    (define environment (secd-environment state))
    (define code (secd-code state))
    (define dump (secd-dump state))
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
          (define closure (first (rest stack)))
          (make-secd empty  
                     (extend-environment
                      (closure-environment closure)
                      (closure-variable closure)
                      (first stack))
                     (closure-code closure)
                     (cons
                      (make-frame (rest (rest stack)) environment (rest code))
                      dump)))
         ((tailap? (first code))
          (define closure (first (rest stack)))
          (make-secd (rest (rest stack))
                     (extend-environment
                      (closure-environment closure)
                      (closure-variable closure)
                      (first stack))
                     (closure-code closure)
                     dump))))
      ((empty? code)
       (define frame (first dump))
       (make-secd
        (cons (first stack)
              (frame-stack frame))
        (frame-environment frame)
        (frame-code frame)
        (rest dump))))))


; Delta-Transition berechnen
(: apply-primitive (primitive (list-of value) -> value))

(check-expect (apply-primitive '+ '(1 2)) 3)
(check-expect (apply-primitive '- '(2 1)) 1)

(define apply-primitive
  (lambda (primitive args)
    (cond
      ((equal? primitive '+)
       (+ (first args) (first (rest args))))
      ((equal? primitive '-)
       (- (first args) (first (rest args))))
      ((equal? primitive '=)
       (= (first args) (first (rest args))))
      ((equal? primitive '*)
       (* (first args) (first (rest args))))
      ((equal? primitive '/)
       (/ (first args) (first (rest args)))))))

; die ersten Elemente einer Liste in umgekehrter Reihenfolge berechnen
(: take-reverse (natural (list-of %a) -> (list-of %a)))

(check-expect (take-reverse 2 '(1 2 3 4 5)) '(2 1))
(check-expect (take-reverse 0 '(1 2 3 4 5)) '())
(check-expect (take-reverse 5 '(1 2 3 4 5)) '(5 4 3 2 1))

(define take-reverse
  (lambda (n list0)
    ;; (: loop (natural (list-of a) (list-of a) -> (list-of a)))
    (define accumulate
      (lambda (n list acc)
        (cond
          ((zero? n) acc)
          ((positive? n)
           (accumulate (- n 1) (rest list) (cons (first list) acc))))))
    (accumulate n list0 '())))

; die ersten Elemente einer Liste weglassen
(: drop (natural (list-of %a) -> (list-of %a)))

(check-expect (drop 2 '(1 2 3 4 5)) '(3 4 5))
(check-expect (drop 0 '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (drop 5 '(1 2 3 4 5)) '())

(define drop
  (lambda (n list)
    (cond
      ((zero? n) list)
      ((positive? n)
       (drop (- n 1) (rest list))))))

; Aus Term SECD-Anfangszustand machen
(: inject-secd (term -> secd))
(define inject-secd
  (lambda (term)
    (make-secd empty
               the-empty-environment
               (term->machine-code/t term)
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
  (lambda (term)
    (define value (first
                   (secd-stack
                    (secd-step* 
                     (inject-secd term)))))
    (if (base? value)
        value
        'function)))

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
  (lambda (environment)
    (cond
      ((empty? environment) (write-string "\\varnothing"))
      ((cons? environment)
       (begin
         (write-string "\\{")
         (write-binding/tex (first environment))
         (for-each (lambda (binding)
                     (begin
                       (write-string ", ")
                       (write-binding/tex binding)))
                   (rest environment))
         (write-string "\\}"))))))

(define write-binding/tex
  (lambda (binding)
    (begin
      (write-string "(")
      (write-string (symbol->string (binding-variable binding)))
      (write-string ", ")
      (write-value/tex (binding-value binding))
      (write-string ")"))))

(define write-code/tex
  (lambda (code)
    (cond
      ((empty? code) (write-string "\\epsilon"))
      ((cons? code)
       (begin
         (write-instruction/tex (first code))
         (for-each (lambda (instruction)
                     (begin
                       (write-string "~")
                       (write-instruction/tex instruction)))
                   (rest code)))))))

(define write-instruction/tex
  (lambda (instruction)
    (cond
      ((base? instruction) (write-value/tex instruction))
      ((symbol? instruction) (write-string (symbol->string instruction)))
      ((prim? instruction)
       (begin
         (write-string "\\mathtt{prim}_")
         (write-string (symbol->string (prim-operator instruction)))))
      ((abs? instruction)
       (begin
         (write-string "(")
         (write-string (symbol->string (abs-variable instruction)))
         (write-string ", ")
         (write-code/tex (abs-code instruction))
         (write-string ")")))
      ((ap? instruction)
       (write-string "\\mathtt{ap}"))
      ((tailap? instruction)
       (write-string "\\mathtt{tailap}")))))

(define write-dump/tex
  (lambda (dump)
    (cond
      ((empty? dump) (write-string "\\epsilon"))
      ((cons? dump)
       (begin
         (write-frame/tex (first dump))
         (for-each (lambda (frame)
                     (begin
                       (write-string "~")
                       (write-frame/tex frame)))
                   (rest dump)))))))

(define write-frame/tex
  (lambda (frame)
    (begin
      (write-string "(")
      (write-stack/tex (frame-stack frame))
      (write-string ", ")
      (write-environment/tex (frame-environment frame))
      (write-string ", ")
      (write-code/tex (frame-code frame))
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
  (lambda (heap address value)
    (define next (heap-next heap))
    (make-heap (cons (make-cell address value)
                     (remove-cell address (heap-cells heap)))
               (if (= address next)
                   (+ next 1)
                   next))))

; Zelle zu einer Adresse entfernen
(: remove-cell (address (list-of cell) -> (list-of cell)))
(define remove-cell
  (lambda (address cell)
    (cond
      ((empty? cell) empty)
      ((cons? cell)
       (if (= address (cell-address (first cell)))
           (rest cell)
           (cons (first cell)
                 (remove-cell address (rest cell))))))))

; den Wert an einer Adresse im Heap nachschauen
(: heap-lookup (heap address -> value))
(define heap-lookup
  (lambda (heap address)
    (cells-lookup (heap-cells heap) address)))

; den Wert an einer Adresse in einer Liste von Zellen nachschauen
(: cells-lookup ((list-of cell) address -> value))
(define cells-lookup
  (lambda (cells address)
    (cond
      ((empty? cells) (violation "unassigned address"))
      ((cons? cells)
       (if (= address (cell-address (first cells)))
           (cell-value (first cells))
           (cells-lookup (rest cells) address))))))

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
    (define stack (secdh-stack state))
    (define environment (secdh-environment state))
    (define code (secdh-code state))
    (define dump (secdh-dump state))
    (define heap (secdh-heap state))
    (cond
      ((cons? code)
       (cond
         ((base? (first code))
          (define address (heap-next heap))
          (make-secdh
           (cons address stack)
           environment
           (rest code)
           dump
           (heap-store heap address (first code))))
         ((symbol? (first code))
          (make-secdh
           (cons (lookup-environment environment (first code)) stack)
           environment
           (rest code)
           dump
           heap))
         ((prim? (first code))
          (define address (heap-next heap))
          (make-secdh
           (cons address
                 (drop (prim-arity (first code)) stack))
           environment
           (rest code)
           dump
           (heap-store heap address
                       (apply-primitive
                        (prim-operator (first code))
                        (map (lambda (address)
                               (heap-lookup heap address))
                             (take-reverse (prim-arity (first code)) stack))))))
         ((:=? (first code))
          (define address (heap-next heap))
          (make-secdh
           (cons address (rest (rest stack)))
           environment
           (rest code)
           dump
           (heap-store
            (heap-store heap
                        (first (rest stack)) 
                        (heap-lookup heap (first stack)))
            address the-void)))
         ((abs? (first code))
          (define address (heap-next heap))
          (make-secdh
           (cons address stack)
           environment
           (rest code)
           dump
           (heap-store heap address
                       (make-closure (abs-variable (first code))
                                     (abs-code (first code))
                                     environment))))
         ((ap? (first code))
          (define closure (heap-lookup heap (first (rest stack))))
          (define address (heap-next heap))
          (make-secdh empty
                      (extend-environment
                       (closure-environment closure)
                       (closure-variable closure)
                       address)
                      (closure-code closure)
                      (cons
                       (make-frame (rest (rest stack)) environment (rest code))
                       dump)
                      (heap-store heap address (heap-lookup heap (first stack)))))
         ((tailap? (first code))
          (define closure (heap-lookup heap (first (rest stack))))
          (define address (heap-next heap))
          (make-secdh (rest (rest stack))
                      (extend-environment
                       (closure-environment closure)
                       (closure-variable closure)
                       address)
                      (closure-code closure)
                      dump
                      (heap-store heap address (heap-lookup heap (first stack)))))))
      ((empty? code)
       (define frame (first dump))
       (make-secdh
        (cons (first stack)
              (frame-stack frame))
        (frame-environment frame)
        (frame-code frame)
        (rest dump)
        heap)))))
    
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
  (lambda (term)
    (make-secdh empty
                the-empty-environment
                (term->machine-code term)
                empty
               the-empty-heap)))

; Evaluationsfunktion zur SECD-Maschine berechnen
(: eval-secdh (term -> (mixed value (one-of 'function))))

(check-expect (eval-secdh '(+ 1 2)) 3)
(check-expect (eval-secdh '(((lambda (x) (lambda (y) (+ x y))) 1) 2)) 3)
(check-expect (eval-secdh '((lambda (x) ((lambda (y) x) (set! x (+ x 1)))) 12)) 13)

(define eval-secdh
  (lambda (term)
    (define final (secdh-step* (inject-secdh term)))
    (define value (heap-lookup (secdh-heap final)
                               (first (secdh-stack final))))
    (if (base? value)
        value
        'function)))

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
         (for-each (lambda (value)
                     (begin
                       (write-string "~")
                       (write-address/tex value)))
                   (rest stack)))))))

(define write-address/tex
  (lambda (address)
    (begin
      (write-string "\\langle{}")
      (write-string (number->string address))
      (write-string "\\rangle{}"))))

(define write-environmenth/tex
  (lambda (environment)
    (cond
      ((empty? environment) (write-string "\\varnothing"))
      ((cons? environment)
       (begin
         (write-string "\\{")
         (write-bindingh/tex (first environment))
         (for-each (lambda (binding)
                     (begin
                       (write-string ", ")
                       (write-bindingh/tex binding)))
                   (rest environment))
         (write-string "\\}"))))))

(define write-bindingh/tex
  (lambda (binding)
    (begin
      (write-string "(")
      (write-string (symbol->string (binding-variable binding)))
      (write-string ", ")
      (write-address/tex (binding-value binding))
      (write-string ")"))))

(define write-codeh/tex
  (lambda (code)
    (cond
      ((empty? code) (write-string "\\epsilon"))
      ((cons? code)
       (begin
         (write-instructionh/tex (first code))
         (for-each (lambda (instruction)
                     (begin
                       (write-string "~")
                       (write-instructionh/tex instruction)))
                   (rest code)))))))

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
    (define cells (heap-cells h))
    (if (empty? cells)
        (write-string "\\varnothing")
        (begin
          (write-cell/tex (first cells))
          (for-each (lambda (cell)
                      (begin
                        (write-string ", ")
                        (write-cell/tex cell)))
                    (rest cells))))))

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
