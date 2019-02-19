 #lang racket

;; The framework of dataflow analysis
;; Chaotic Iteration Algorithm
;(require br/cond)

;(require control)
(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")

(provide (all-defined-out))

(struct Analysis (direction
                  init
                  entry-fact
                  exit-fact
                  gen
                  kill
                  meet))

(define (chaotic-iteration analysis)
  (define init (Analysis-init analysis))
  (define direction (Analysis-direction analysis))
  (define entry-fact (Analysis-entry-fact analysis))
  (define exit-fact (Analysis-exit-fact analysis))
  (define gen (Analysis-gen analysis))
  (define kill (Analysis-kill analysis))
  (define meet (Analysis-meet analysis))

  (lambda (fun)
    (define cfg (stmt->cfg fun))
    (define IN (make-hash))
    (define OUT (make-hash))
    
    (for ([n (CFG-nodes cfg)])
        (hash-set! IN n (init cfg n))
        (hash-set! OUT n (init cfg n)))

    (hash-set! OUT (CFG-exit cfg) (set))
    (hash-set! IN (CFG-entry cfg) (set))
    
    (when (not (empty? entry-fact))
      (hash-set! IN (CFG-entry cfg) (entry-fact fun cfg (CFG-entry cfg))))
    (when (not (empty? exit-fact))
      (hash-set! OUT (CFG-exit cfg) (exit-fact fun cfg (CFG-exit cfg))))

    
    (define (loop IN OUT old-IN old-OUT)
      (for ([n (CFG-nodes cfg)])
        (cond [(eq? 'forward direction)
               (let ([preds (map (curry hash-ref OUT) (get-preds n cfg))])
                 (when (not (empty? preds)) (hash-set! IN n (apply meet preds))))
               (hash-set! OUT n (set-union (set-subtract (hash-ref IN n) (kill cfg n)) (gen cfg n)))]
              [(eq? 'backward direction)
               (let ([succs (map (curry hash-ref IN) (get-succs n cfg))])
                 (when (not (empty? succs)) (hash-set! OUT n (apply meet succs))))
               (hash-set! IN n (set-union (set-subtract (hash-ref OUT n) (kill cfg n)) (gen cfg n)))]
              [else (error "not a direction")]))
      
      (if (and (equal? IN old-IN)
               (equal? OUT old-OUT))
          (cons IN OUT)
          (loop IN OUT (hash-copy IN) (hash-copy OUT))))
    (loop IN OUT (hash-copy IN) (hash-copy OUT))))


(define (worklist-iteration analysis)
  (define init (Analysis-init analysis))
  (define direction (Analysis-direction analysis))
  (define entry-fact (Analysis-entry-fact analysis))
  (define exit-fact (Analysis-exit-fact analysis))
  (define gen (Analysis-gen analysis))
  (define kill (Analysis-kill analysis))
  (define meet (Analysis-meet analysis))

  (lambda (fun)
    (define cfg (stmt->cfg fun))
    (define IN (make-hash))
    (define OUT (make-hash))
    (define worklist empty)
    
    (for ([n (CFG-nodes cfg)])
        (set! worklist (append worklist (list n)))
        (hash-set! IN n (init cfg n))
        (hash-set! OUT n (init cfg n)))

    (hash-set! OUT (CFG-exit cfg) (set))
    (hash-set! IN (CFG-entry cfg) (set))
    
    (when (not (empty? entry-fact))
      (hash-set! IN (CFG-entry cfg) (entry-fact fun cfg (CFG-entry cfg))))
    (when (not (empty? exit-fact))
      (hash-set! OUT (CFG-exit cfg) (exit-fact fun cfg (CFG-exit cfg))))

    (define (loop IN OUT old-IN old-OUT worklist)
      (let ([v_i (first worklist)])
        (set! worklist (rest worklist))
        (cond [(eq? 'forward direction)
               (let ([preds (map (curry hash-ref OUT) (get-preds v_i cfg))])
                  (when (not (empty? preds)) (hash-set! IN v_i (apply meet preds))))
               (hash-set! OUT v_i (set-union (set-subtract (hash-ref IN v_i) (kill cfg v_i)) (gen cfg v_i)))
               (when (not (and (equal? IN old-IN) (equal? OUT old-OUT)))
                   (let ([succs (get-succs v_i cfg)])
                     (let ([new_list (append worklist succs)])
                        (set! worklist (remove-duplicates new_list)))))]
              [(eq? 'backward direction)
               (let ([succs (map (curry hash-ref IN) (get-succs v_i cfg))])
                 (when (not (empty? succs)) (hash-set! OUT v_i (apply meet succs)))
               (hash-set! IN v_i (set-union (set-subtract (hash-ref OUT v_i) (kill cfg v_i)) (gen cfg v_i)))
               (when (not (and (equal? IN old-IN) (equal? OUT old-OUT)))
                 (let ([preds (get-preds v_i cfg)])
                   (let ([new_list (append worklist preds)])
                     (set! worklist (remove-duplicates new_list))))))]
             [else (error "not a direction")]))
      (if (empty? worklist)
          (cons IN OUT)
          (loop IN OUT (hash-copy IN) (hash-copy OUT) worklist)))

    (loop IN OUT (hash-copy IN) (hash-copy OUT) worklist))
  )