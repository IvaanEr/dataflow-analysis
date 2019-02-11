#lang racket

;; Live Variables Analysis
;; A variable is live at a program point if its
;; current value may be read during the remaining
;; execution of program.

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
      ; [(Node (App f args) _)
      ;  (apply set-union (map get-vars args))]
      ; [(Node (Output e) _)
      ;  (get-vars e)]
      ; [(Node (Return e) _)
      ;  (get-vars e)]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (set id)]
       [else (set)]))
   ; meet
   set-union))

(define live-variables-star
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
      ; [(Node (App f args) _)
      ;  (apply set-union (map get-vars args))]
      ; [(Node (Output e) _)
      ;  (get-vars e)]
      ; [(Node (Return e) _)
      ;  (get-vars e)]
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
)