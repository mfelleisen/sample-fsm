#lang racket

(provide
 ;; Population N N N -> [Listof Payoff]
 ;; (evolve p c s r) computes the list of average payoffs over the evolution of population p
 ;; for c cycles of of match-ups with r rounds per match and at death-birth rate of s
 evolve)

;; ---------------------------------------------------------------------------------------------------
(require "automata.rkt" "population.rkt")

(define (evolve population cycles rate rounds)
  (define-values (result _)
    (for/fold ([result '()][population population]) ([_ (in-range cycles)])
      [define payoffs (match-ups population rounds interact)]
      (values (cons (relative-average payoffs rounds) result)
              (death-birth population (payoff-percentages payoffs) rate))))
  (reverse result))

;; [Listof Payoff] Payoff -> [Listof [0,1]]
;; constraint: (= (sum payoff) payoff-sum)
;; from the matching result, calculate the accumulated fitness
(define (payoff-percentages payoffs)
  (define payoff-sum (sum payoffs))
  (define-values (accumulated _)
    (for/fold ([accumulated (list 0)] [init 0]) ([y (in-list payoffs)])
      (define next-init (+ init (/ y payoff-sum)))
      (values (cons next-init accumulated) next-init)))
  (reverse accumulated))

;; [Listof Number] -> Number 
(define (sum l) (apply + l))

;; [Listof Number] Number -> Number
(define (relative-average l w)
  (exact->inexact
   (/ (sum l)
      w (length l))))