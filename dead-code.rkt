#lang racket
(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")
(require "live-var.rkt")

(require dyoo-while-loop)
(require rackunit)


(define (is-first node)
  (match node
    [(Node _ 1) #t]
    [else #f])
  )

(define (live prog o)
  (define live-IN (car ((live-variables-star o) prog)))
  (define ll (hash->list live-IN))
  (filter (lambda (elem) (is-first (car elem))) ll)
  (define conjunto (cdr (first ll)))
  ;(print live-IN)
  ;(print conjunto)
  conjunto
)


(define (bury expr o)
  (match expr
    [(Skip) (Skip)]
    [(Assign x a) (cond [(set-member? o x) (Assign x a)] [else (Skip)])]
    [(Seq s) (Seq (list (bury (first s) (live (second s) o)) (bury (second s) o)))]
    [(If cnd thn els) (If cnd (bury thn o) (bury els o))]
    [(While cnd body) (While cnd (bury body (live (While cnd body) o)))]
  )
)

(module+ test
  
  (define test-stmt-2
    (parse-stmt '{{:= x 1}
                  {:= x 5}
                  {if {> x 6} {:= y x} {:= y 10}}
                 }))

  (print test-stmt-2)
  (define test-stmt-3
     (parse-stmt '{:= x 1}))

  (define test-stmt-4
    (parse-stmt '{while {> x 1}
                        {{:= x {+ x 1}}
                         {:= y 0}
                        }
                 }))

 (define test2 (bury test-stmt-2 (set 'x 'y)))
  (print test2)
  
 (define test3 (bury test-stmt-3 (set 'x)))
 (check-equal? test3 (Assign 'x 1))

 (define test4 (bury test-stmt-4 (set 'x 'y)))
 (check-equal? test4 (While (Greater 'x 1) (Seq (list (Assign 'x (Plus 'x 1)) (Skip)))))

)

