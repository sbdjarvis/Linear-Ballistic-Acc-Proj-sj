#lang racket

(require rml-neural/activation)

(require plot)
;; data hash table from article, table 1; experiment 5
(define model-param-expt5 (hash 't0 0.438 ;; non-decision response time
                                'a 460 ;; upper end of start point distribution
                                'b 460 ;; response threshold
                                's 0.422 ;; standard deviation
                                'vp 1.07 ;; mean drift rate for pseudowords
                                'vr 1.0 ;; basic parameter. starting small but mean drift rate for random letter strings
                                'vh 1.36 ;; mean drift rate for high-frequency words
                                'vl 1.00 ;; mean drift rate for low-frequency words
                                'vv 0.802)) ;; mean drift rate for very low-frequency words

(define (lba-decision-time param)
  (let* ((t0 (hash-ref param 't0))
         (a (hash-ref param 'a))
         (b (hash-ref param 'b))
         (s (hash-ref param 's))
         (vr (hash-ref param 'vr)) 
         (drift (gaussian vr s)) ;; supposed to be a nice visual of a gaussian using the s as an activator 
         (rand-k (- b a))) ;; response caution? 
    (if (>= drift 0)
        (/ (- b t0) drift) ;; time for accumulator to reach the threshold
        (/ (- rand-k t0) (- drift)))))
                  
(define decision-time (lba-decision-time model-param-expt5))
(displayln decision-time)

(plot decision-time)
