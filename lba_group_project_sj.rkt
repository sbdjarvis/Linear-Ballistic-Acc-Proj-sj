#lang racket

(require math/distributions)
;(require "Matrix_List_Helpers.rkt") ; I thought that "Numerical_Integration.rkt" needed this, but apparently not?
(require (only-in "Numerical_Integration.rkt" fnInt)) ; from https://github.com/mkierzenka/Racket_NumericalMethods.



(define model-param-expt5 (hash 't0 0.438 ;; non-decision response time
                                'a 460 ;; upper end of start point distribution
                                'b 460 ;; response threshold
                                's 0.422 ;; standard deviation
                                'v1 1.07 ; PLACEHOLDER HERE - need to set up for individual values?
                                'vp 1.07 ;; mean drift rate for pseudowords
                                'vr 1.0 ;; basic parameter. starting small but mean drift rate for random letter strings
                                'vh 1.36 ;; mean drift rate for high-frequency words
                                'vl 1.00 ;; mean drift rate for low-frequency words
                                'vv 0.802)) ;; mean drift rate for very low-frequency words

(define model-param-expt3 (hash 't0 0.425 ;; non-decision response time
                                'a 380 ;; upper end of start point distribution
                                'b 380 ;; response threshold
                                's 0.428 ;; standard deviation
                                'v1 1.04 ; PLACEHOLDER HERE - need to set up for individual values?
                                'vp 1.07 ;; mean drift rate for pseudowords
                                'vr 1.0 ;; basic parameter. starting small but mean drift rate for random letter strings
                                'vh 1.36 ;; mean drift rate for high-frequency words
                                'vl 1.00 ;; mean drift rate for low-frequency words
                                'vv 0.802)) ;; mean drift rate for very low-frequency words

(define model-param-Counter-expt3 (hash 't0 0.425 ;; non-decision response time
                                'a 380 ;; upper end of start point distribution
                                'b 380 ;; response threshold
                                's 0.428 ;; standard deviation
                                'v1 -0.04 ; PLACEHOLDER HERE - need to set up for individual values?
                                'vp 1.07 ;; mean drift rate for pseudowords
                                'vr 1.0 ;; basic parameter. starting small but mean drift rate for random letter strings
                                'vh 1.36 ;; mean drift rate for high-frequency words
                                'vl 1.00 ;; mean drift rate for low-frequency words
                                'vv 0.802)) ;; mean drift rate for very low-frequency words



(define dist (normal-dist)) ; In case we ever want to change the distribution used. Other papers used ones with tails, etc.

; Helper functions for getting our phis:

(define (ϕ x) ; Lowercase phi for probability distribution func
  (pdf dist x))
(define (Φ x) ; Uppercase phi for cumulative probability distribution func
  (cdf dist x))

; Here's the PDF and CDF eq'ns, respectively:

(define (f-at-t t expt-params)
  (let ([A (hash-ref expt-params 'a)]
        [v (hash-ref expt-params 'v1)]
        [b (hash-ref expt-params 'b)]
        [s (hash-ref expt-params 's)])
   (* (/ 1 A)
      (+ (* -1 v (Φ (/ (- b A (* t v)) (* t s))))
          (* s (ϕ (/ (- b A (* t v)) (* t s))))
          (* v (Φ (/ (- b (* t v)) (* t s))))
          (* -1 s (ϕ (/ (- b (* t v)) (* t s))))))))

(define (F-at-t t expt-params)
  (let ([A (hash-ref expt-params 'a)]
        [v (- 1 (hash-ref expt-params 'v1))] ; Okay, so this line is a bit of a kludge. Since we're only running 2-decision models, we're assuming vi+1 = 1 - vi.
        [b (hash-ref expt-params 'b)]
        [s (hash-ref expt-params 's)])
   (+ 1 (* (/ (- b A (* t v)) A) (Φ (/ (- b A (* t v)) (* t s))))
        (* -1 (/ (- b (* t v)) A) (Φ (/ (- b (* t v)) (* t s ))))
        (* (/ (* t s) A) (ϕ (/ (- b A (* t v)) (* t s))))
        (* -1 (/ (* t s) A) (ϕ (/ (- b (* t v)) (* t s )))))))

(define (PDF-func ft FT) ; Here's the PDF for vi
  (* ft (- 1 FT)))

(define (CDF-func var expt-params)
  (fnInt (λ (t) (PDF-func (f-at-t var expt-params) (F-at-t var expt-params))) 0 var
                '(-1 0 1)
                '(1/6 4/6 1/6)))

(define (intTEST func var)
  (fnInt (λ (t) (f-at-t t model-param-expt5)) 0 1
                '(-1 0 1)
                '(1/6 4/6 1/6)))

; (F-at-t 0.00000001 model-param-expt3)
; (PDF-func (f-at-t 0.000001 model-param-expt3) (F-at-t  0.000001 model-param-expt3))
; (CDF-func 0.000001 model-param-expt3)
; (fnInt (PDF1 (f-at-t 1 1 1 1 1) (F-at-t 1 1 1 1 1)) 0 1 (list -1 0 1) (list (/ 1 6) (/ 4 6) (/ 1 6) ))