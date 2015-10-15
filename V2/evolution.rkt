#lang racket

(provide
 ;; Population N N N -> [Listof Payoff]
 ;; (evolve p c s r) computes the list of average payoffs over the evolution of population p
 ;; for c cycles of of match-ups with r rounds per match and at death-birth rate of s
 evolve)

;; ---------------------------------------------------------------------------------------------------
(require "automata.rkt" "population.rkt" "utilities.rkt")

(define (evolve population cycles rate rounds)
  (cond
    [(zero? cycles) '()]
    [else (define p2 (match-up* population rounds))
          (define p* (population-payoffs p2))
          (cons (relative-average p* rounds)
                (evolve (death-birth p2 rate) (- cycles 1) rate rounds))]))
