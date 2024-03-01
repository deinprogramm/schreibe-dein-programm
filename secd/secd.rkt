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
          base)))

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
	  abst)))

; Eine Maschinencode-Programm ist eine Liste von Instruktionen.

(define machine-code (signature (list-of instruction)))

; Applikations-Instruktion
(define-record ap
  make-ap ap?)

; Eine endrekursive Applikations-Instruktion ist ein Wert
;   (make-tailap)
(define-record tailap
  make-tailap tailap?)

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
  make-abst abs?
  (abst-variable symbol)
  (abst-code machine-code))

; Term in Maschinencode übersetzen
(: term->machine-code (term -> machine-code))

(check-expect (term->machine-code '(+ 1 2))
              (list 1 2 (make-prim '+ 2)))
(check-expect (term->machine-code '((lambda (x) (x x)) (lambda (x) (x x))))
              (list (make-abst 'x (list 'x 'x (make-ap))) (make-abst 'x (list 'x 'x (make-ap))) (make-ap)))

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
        (make-abst (first (first (rest term)))
                  (term->machine-code
                   (first (rest (rest term)))))))
      ((primitive-application? term)
       (append
        (append-lists
         (map term->machine-code (rest term)))
        (list (make-prim (first term) (length (rest term)))))))))

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
        (make-abst (first (first (rest term)))
                  (term->machine-code/t-t
                   (first (rest (rest term)))))))
      ((base? term) (list term))
      ((primitive-application? term)
       (append
        (append-lists
         (map term->machine-code/t (rest term)))
        (list (make-prim (first term) (length (rest term)))))))))

; Term in Maschinencode übersetzen
;  in endrekursivem Kontext
(: term->machine-code/t-t (term -> machine-code))

(check-expect (term->machine-code/t '((lambda (x) (x x)) (lambda (x) (x x))))
              (list (make-abst 'x (list 'x 'x (make-tailap))) (make-abst 'x (list 'x 'x (make-tailap))) (make-ap)))
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
        (make-abst (first (first (rest term)))
                  (term->machine-code/t-t
                   (first (rest (rest term)))))))
      ((base? term) (list term))
      ((primitive-application? term)
       (append
        (append-lists
         (map term->machine-code/t (rest term)))
        (list (make-prim (first term) (length (rest term)))))))))

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

(check-expect (extend-environment the-empty-environment 'axl 59)
              (list (make-binding 'axl 59)))
(check-expect (extend-environment
               (extend-environment
                the-empty-environment
                'axl 60)
               'slash 57)
              (list (make-binding 'slash 57) (make-binding 'axl 60)))
(check-expect (extend-environment
               (extend-environment
                the-empty-environment
                'axl 59)
               'axl 60)
              (list (make-binding 'axl 60)))

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

(check-expect
 (lookup-environment (list (make-binding 'slash 57) (make-binding 'axl 60))
                     'axl)
 60)
(check-expect
 (lookup-environment (list (make-binding 'slash 57) (make-binding 'axl 60))
                     'slash)
 57)

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
(define value (signature (mixed base closure)))

; Eine Closure besteht aus:
; - Variable
; - Code
; - Umgebung
(define-record closure
  make-closure closure?
  (closure-variable symbol)
  (closure-code machine-code)
  (closure-environment environment))

; Ein SECD-Zustand besteht aus:
; - Stack
; - Umgebung
; - Code
; - Dump
(define-record secd
  make-secd secd?
  (secd-stack stack)
  (secd-environment environment)
  (secd-code machine-code)
  (secd-dump dump))

; Zustandsübergang berechnen
(: secd-step (secd -> secd))

(check-expect
 (secd-step
  (make-secd empty the-empty-environment (list 5) empty))
 (make-secd (list 5) the-empty-environment empty empty))
(check-expect
 (secd-step
  (make-secd empty (list (make-binding 'axl 60)) (list 'axl) empty))
 (make-secd (list 60) (list (make-binding 'axl 60)) empty empty))
(check-expect
 (secd-step
  (make-secd (list 23 42) the-empty-environment
                          (list (make-prim '+ 2))
                          empty))
 (make-secd (list 65) the-empty-environment empty empty))
(check-expect
 (secd-step
  (make-secd empty (list (make-binding 'slash 57))
             (list (make-abst 'axl (list 'axl 'slash (make-prim '+ 2))))
             empty))
 (make-secd (list (make-closure 'axl
                                (list 'axl 'slash (make-prim '+ 2))
                                (list (make-binding 'slash 57))))
            (list (make-binding 'slash 57))
            empty empty))
(check-expect
 (secd-step
  (make-secd (list 60
                   (make-closure 'axl
                                 (list 'axl 'slash (make-prim '+ 2))
                                 (list (make-binding 'slash 57))))
             the-empty-environment
             (list (make-ap)) empty))
 (make-secd empty
            (list (make-binding 'axl 60) (make-binding 'slash 57))
            (list 'axl 'slash (make-prim '+ 2))
            (list (make-frame empty the-empty-environment empty))))

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
          (make-secd (cons (make-closure (abst-variable (first code))
                                         (abst-code (first code))
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

