#lang racket

(provide
 ;; Population N N N -> [Listof Payoff]
 ;; (evolve p c s r) computes the list of average payoffs over the evolution of population p
 ;; for c cycles of of match-ups with r rounds per match and at death-birth rate of s
 evolve)

;; ---------------------------------------------------------------------------------------------------
(require "automata.rkt")

(define (evolve population cycles speed rounds-per-match)
  (define-values (result _)
    (for/fold ([result '()][population population]) ([_ (in-range cycles)])
      [define payoffs (match-ups population rounds-per-match)]
      [define sum (apply + payoffs)]
      [define average-payoff (exact->inexact (/ sum (* rounds-per-match (length population))))]
      [define fitness (payoff-percentages payoffs sum)]
      [define survivors (drop population speed)]
      [define substitutes (randomise-over-fitness fitness population speed)]
      [define new-population (shuffle (append survivors substitutes))]
      (values (cons average-payoff result) new-population)))
  (reverse result))

;; Population N -> [Listof Number]
;; (match-ups p r) matches up neighboring pairs of
;; automata in population p for r rounds per match 
(define (match-ups population rounds-per-match)
  ;; Automata Automata ->* Number Number Any Any Any Any 
  ;; the sum of pay-offs for the two respective automata over all rounds 
  (define (match-up auto1 auto2)
    (for/fold ([sum1 0] [sum2 0] [auto1 auto1] [auto2 auto2]) ([_ (in-range rounds-per-match)])
      (define-values (d1 d2 next1 next2) (interact auto1 auto2))
      (values (+ d1 sum1) (+ d2 sum2) next1 next2)))
  ;; -- IN --
  (let pair-up-loop ([population population])
    (cond
      [(empty? population) '()] ; a population is a list of even length 
      [else (define-values (sum1 sum2 _1 _2) (match-up (first population) (second population)))
            (list* sum1 sum2 (pair-up-loop (rest (rest population))))])))

;; [Listof Payoff] Payoff -> [Listof [0,1]]
;; constraint: (= (sum payoff) payoff-sum)
;; from the matching result, calculate the accumulated fitness
(define (payoff-percentages payoff payoff-sum)
  (define-values (accumulated _)
    (for/fold ([accumulated (list 0)] [init 0]) ([y (in-list payoff)])
      (define next-init (+ init (/ y payoff-sum)))
      (values (cons next-init accumulated) next-init)))
  (reverse accumulated))

;; [Listof [0,1]] Population N -> Population 
;; spawn another set of fitt automata
;; at the end of the cycle, kill N%; then spawn child-copies of "fittest"
(define (randomise-over-fitness payoff-percentage population speed)
  (for/list ([n (in-range speed)])
    [define r (random)] ;; SHOULDN"T THIS LINE BE OUTSIDE OF THE for/list COMPREHENSION?
    (for/and ([p (in-list population)]
              [a (in-list payoff-percentage)]
              #:break (< r a))
      p)))
