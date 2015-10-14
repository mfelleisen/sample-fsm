#lang racket

(provide
 ;; X X -> X
 one-of

 ;; [Listof X] Y [X -> Y] [X -> Z] -> '() or Z
 apply-to-first

 ;; [Listof Number] -> Number 
 sum

 ;; [Listof Number] Number -> Number 
 relative-average
 )


(define (sum l)
  (apply + l))

(define (relative-average l w)
  (exact->inexact
   (/ (sum l)
      w (length l))))

;; X *-> X
#;
(define (one-of . x)
  (list-ref x (random (length x))))

(define (one-of x y)
  (if (= (random 2) 0) x y))

(define (apply-to-first l x sel f)
  (define result (for/first ([a (in-list l)] #:when (equal? x (sel a))) a))
  (if result (f result) '()))
