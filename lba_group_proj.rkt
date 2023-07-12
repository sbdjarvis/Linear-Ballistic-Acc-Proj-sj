#lang racket
(require rml-neural/activation)

;; data hash table from article, table 1; experiment 5
(define model-param-expt5 (hash 't0 0.438
                                'a 460
                                'b 460
                                's 0.422
                                'vp 1.07
                                'vr 1.0
                                'vh 1.36
                                'vl 1.00
                                'vv 0.802))

(define (lba-decision-time param)
  (let* ((t0 (hash-ref param 't0))
         (a (hash-ref param 'a))
         (b (hash-ref param 'b))
         (s (hash-ref param 's))
         (vr (if (hash-has-key? param 'vr) (hash-ref param 'vr) 1.0)) ;; 'vr is missing from the table. tedious but it searches if the parameter is there but fi not, it uses a ddefault drift rate value of 1.0 that I hard-coded 
         (drift (gaussian vr s))
         (rand-k (- b a)))
    (if (>= drift 0)
        (/ (- b t0) drift)
        (/ (- rand-k t0) (- drift)))))
                  
(define decision-time (lba-decision-time model-param-expt5))
(displayln decision-time)