#lang racket

;; Live Variables Analysis
;; A variable is live at a program point if its
;; current value may be read during the remaining
;; execution of program.
(provide (all-defined-out))

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define live-variables-analysis
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg n) (set))
   ; entry fact
   (λ (fun cfg entry) (set))
   ; exit fact
   (λ (fun cfg exit) (set))
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (get-vars e)]
       [(Node (Equal l r) _)
        (set-union (get-vars l) (get-vars r))]
       [(Node (Greater l r) _)
        (set-union (get-vars l) (get-vars r))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (set id)]
       [else (set)]))
   ; meet
   set-union))


(define (live-variables-analysis-star final-var-set)
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg n) (set))
   ; entry fact
   (λ (fun cfg entry) (set))
   ; exit fact
   (lambda (fun cfg exit) final-var-set)
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (get-vars e)]
       [(Node (Equal l r) _)
        (set-union (get-vars l) (get-vars r))]
       [(Node (Greater l r) _)
        (set-union (get-vars l) (get-vars r))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (set id)]
       [else (set)]))
   ; meet
   set-union))


(define live-variables
  (chaotic-iteration live-variables-analysis))


(define live-variables-worklist
  (worklist-iteration live-variables-analysis))

(define (live-variables-star-worklist final-var-set)
  (worklist-iteration (live-variables-analysis-star final-var-set)))

(define (live-variables-star final-var-set)
 (chaotic-iteration (live-variables-analysis-star final-var-set))
)

#|
(module+ test
  (define test-stmt
    (parse-stmt
     '{{:= x 2}
       {:= y 4}
       {:= x 1}
       {if {> y x}
           {:= z y}
           {:= z {* y y}}}
       {:= x z}}))
          

  (define result (live-variables test-stmt))
  (define result-OUT (cdr result))

  (define result-star ((live-variables-star (set 'x 'y 'z)) test-stmt))
  (define result-OUT-star (cdr result-star))

  
  (define result-worklist (live-variables test-stmt))
  (define result-OUT-worklist (cdr result-worklist))

  (check-equal? (make-immutable-hash (hash->list result-OUT-star))
                (hash
                 (Node (Skip) 7) (set 'z)
                 (Node (Greater 'y 'x) 6) (set 'y)
                 (Node (Assign 'x 1) 3) (set 'x 'y)
                 (Node (Assign 'y 4) 2) (set 'y)
                 (Node (Assign 'z 'y) 4) (set 'z)
                 (Node (Assign 'x 2) 1) (set)
                 (Node (Assign 'x 'z) 8) (set)
                 (Node (Assign 'z (Mult 'y 'y)) 5) (set 'z)))


 (check-equal? (make-immutable-hash (hash->list result-OUT-worklist))
                (hash
                 (Node (Skip) 7) (set 'z)
                 (Node (Greater 'y 'x) 6) (set 'y)
                 (Node (Assign 'x 1) 3) (set 'x 'y)
                 (Node (Assign 'y 4) 2) (set 'y)
                 (Node (Assign 'z 'y) 4) (set 'z)
                 (Node (Assign 'x 2) 1) (set)
                 (Node (Assign 'x 'z) 8) (set)
                 (Node (Assign 'z (Mult 'y 'y)) 5) (set 'z)))

  )
|#
(module+ test
  (define test-stmt
    (parse-stmt
     '{{:= x 2}
       {:= y 4}
       {:= x 1}
       {if {> y x}
           {:= z y}
           {:= z {* y y}}}
       {:= x z}}))

  (define result (live-variables test-stmt))
  (define result-OUT (cdr result))

  (check-equal? (make-immutable-hash (hash->list result-OUT))
                (hash
                 (Node (Skip) 7) (set 'z)
                 (Node (Greater 'y 'x) 6) (set 'y)
                 (Node (Assign 'x 1) 3) (set 'x 'y)
                 (Node (Assign 'y 4) 2) (set 'y)
                 (Node (Assign 'z 'y) 4) (set 'z)
                 (Node (Assign 'x 2) 1) (set)
                 (Node (Assign 'x 'z) 8) (set)
                 (Node (Assign 'z (Mult 'y 'y)) 5) (set 'z)))

  (define result-star ((live-variables-star (set 'x 'y 'z)) test-stmt))
  (define result-OUT-star (cdr result-star))

  (check-equal? (make-immutable-hash (hash->list result-OUT-star))
                (hash
                 (Node (Skip) 7) (set 'y 'z)
                 (Node (Greater 'y 'x) 6) (set 'y)
                 (Node (Assign 'x 1) 3) (set 'x 'y)
                 (Node (Assign 'y 4) 2) (set 'y)
                 (Node (Assign 'z 'y) 4) (set 'y 'z)
                 (Node (Assign 'x 2) 1) (set)
                 (Node (Assign 'x 'z) 8) (set 'x 'y 'z)
                 (Node (Assign 'z (Mult 'y 'y)) 5) (set 'y 'z)))

  (define result-worklist (live-variables-worklist test-stmt))
  (define result-OUT-worklist (cdr result-worklist))

  (check-equal? (make-immutable-hash (hash->list result-OUT-worklist))
                (hash
                 (Node (Skip) 7) (set 'z)
                 (Node (Greater 'y 'x) 6) (set 'y)
                 (Node (Assign 'x 1) 3) (set 'x 'y)
                 (Node (Assign 'y 4) 2) (set 'y)
                 (Node (Assign 'z 'y) 4) (set 'z)
                 (Node (Assign 'x 2) 1) (set)
                 (Node (Assign 'x 'z) 8) (set)
                 (Node (Assign 'z (Mult 'y 'y)) 5) (set 'z)))

  (define result-star-worklist ((live-variables-star-worklist (set 'x 'y 'z)) test-stmt))
  (define result-OUT-star-worklist (cdr result-star-worklist))

  (check-equal? (make-immutable-hash (hash->list result-OUT-star-worklist))
                (hash
                 (Node (Skip) 7) (set 'y 'z)
                 (Node (Greater 'y 'x) 6) (set 'y)
                 (Node (Assign 'x 1) 3) (set 'x 'y)
                 (Node (Assign 'y 4) 2) (set 'y)
                 (Node (Assign 'z 'y) 4) (set 'y 'z)
                 (Node (Assign 'x 2) 1) (set)
                 (Node (Assign 'x 'z) 8) (set 'x 'y 'z)
                 (Node (Assign 'z (Mult 'y 'y)) 5) (set 'y 'z)))

)
