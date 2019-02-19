#lang racket
(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")
(require "live-var.rkt")

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
  (printf "Conjunto: ~v \n" conjunto)
  conjunto
)


(define (bury expr o)
  (match expr
    [(Skip) (Skip)]
    [(Assign x a) (cond [(set-member? o x) (Assign x a)] [else (Skip)])]
    [(Seq s) #:when (> (length s) 1) (Seq (list (bury (first s) (live (Seq (rest s)) o)) (bury (Seq (rest s)) o)))]
    [(Seq s) #:when (= (length s) 1) (bury (first s) o)]
    [(If cnd thn els) (If cnd (bury thn o) (bury els o))]
    [(While cnd body) (While cnd (bury body (live (While cnd body) o)))]
  )
)

(module+ test

  (define test-stmt-1
    (parse-stmt '{while {> x 5}
                        {if {> y 10} {:= x {+ x 1}} {{:= y {- y 1}}{:= y 10}}}
                 }))
  
  (define test-stmt-2
    (parse-stmt '{if {> x 4} {{:= y 2}{:= y 10}} {{:= x 1}{:= x 10}}}))

  (define test-stmt-3
     (parse-stmt '{{:= x 3}{:= x 2}{:= x 1}}))

  (define test-stmt-4
    (parse-stmt '{while {> x 1}
                        {{:= x {+ x 1}}
                         {:= y 0}
                        }
                 }))
 (printf "Test 1\n")
 (define test1 (bury test-stmt-1 (set 'x 'y)))
 (check-equal? test1 (While (Greater 'x 5) (If (Greater 'y 10) (Assign 'x (Plus 'x 1)) (Seq (list (Skip) (Assign 'y 10))))))

 (printf "Test 2\n")
 (define test2 (bury test-stmt-2 (set 'x 'y)))
 (check-equal? test2 (If (Greater 'x 4) (Seq (list (Skip) (Assign 'y 10))) (Seq (list (Skip) (Assign 'x 10)))))

 (printf "Test 3\n")
 (define test3 (bury test-stmt-3 (set 'x)))
 (check-equal? test3 (Seq (list (Skip) (Assign 'x 1))))

 (printf "Test 4\n")
 (define test4 (bury test-stmt-4 (set 'x 'y)))
 (check-equal? test4 (While (Greater 'x 1) (Seq (list (Assign 'x (Plus 'x 1)) (Skip)))))
 
)

