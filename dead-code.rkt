#lang racket
(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")
(require "live-var.rkt")

(require dyoo-while-loop)
(require rackunit)

(define (live prog o)
  (define live-IN (car ((live-variables-star o) prog)))
  (print live-IN)
)


(define (bury expr o)
  (match expr
    [(Skip) (Skip)]
    [(Assign x a) (cond [(set-member? (set o) x) (Assign x a)] [else (Skip)])]
    [(Seq s) (Seq (list (bury (first s) (live (second s) o)) (bury (second s) o)))]
    [(If cnd thn els) (If cnd (bury thn o) (bury els o))]
    [(While cnd body) (While cnd (bury body (live (While cnd body) o)))]
  )
)

(module+ test
  (define test-stmt
    (parse-stmt '{{:= x {+ a b}}
                  {:= y {* a b}}
                  {while {> y {+ a b}}
                         {{:= a {+ a 1}}
                          {:= x {+ a b}}}}
                  })
   )

  (define test-stmt-2
    (parse-stmt '{{:= x 1}
                  {:= x 5}
                  {if {> x 3} {:= y x} {:= y 10}}
                 }))

 ;(define out-var (cdr (live-variables test-stmt)))
 ;(print out-var)
 ;(define values (hash-values out-var)) 
 ;(define ultimo (last values)) 
 ;(print ultimo)


 (define out-var (cdr (live-variables test-stmt-2)))
 ;(print out-var)
 (define values (hash-values out-var)) 
 (define ultimo (last values)) 
 ;(print ultimo)

 (define test1 (bury test-stmt-2 ultimo))
 ;(print test1)
)

