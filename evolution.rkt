#lang racket

(provide
 evolve)

;; ---------------------------------------------------------------------------------------------------
(require "automata.rkt")

;; N N N N -> [Listof Real]
;; EVOLVE THE POPULATION OVER CYCLES
(define (evolve population cycles speed rounds-per-match)
  (define-values (result _)
    (for/fold ([result '()][population population]) ([_ (in-range cycles)])
      [define round-results (match-population population rounds-per-match)]
      [define sum (apply + round-results)]
      [define average-payoff (/ sum (* rounds-per-match #i100.))]
      [define accum-fitness (payoff-percentages-accumulated round-results sum)]
      [define survivors (drop population speed)]
      [define substitutes (randomise-over-fitness accum-fitness population speed)]
      [define new-population (shuffle (append survivors substitutes))]
      (values (cons average-payoff result) new-population)))
  (reverse result))

;; FITNESS CALCULATION
;; from the matching result, calculate the fitness

(define (payoff-percentages-accumulated payoff payoff-sum)
  (define-values (accumulated _)
    (for/fold ([accumulated (list 0)] [init 0]) ([y (in-list payoff)])
      (define next-init (+ init (/ y payoff-sum)))
      (values (cons next-init accumulated) next-init)))
  (reverse accumulated))

;; spawn another set of fitt automata
;; at the end of the cycle, kill N%; then spawn child-copies of "fittest"
;;
;; This procedure uses an independent Bernoulli draw. We independently
;; draw a random number (associated with an automaton) for 10 times. How
;; likely an automaton is chosen depends on its own fitness (its interval
;; in the unit scale of the accumulated percentages.) 
(define (randomise-over-fitness accumulated-payoff-percentage population speed)
  (for/list ([n (in-range speed)])
    [define r (random)]
    (for/and ([p (in-list population)]
	      [a (in-list accumulated-payoff-percentage)]
	      #:break (< r a))
      p)))

;; Population N -> [Listof Number]
;; MATCH POPULATION
(define (match-population population rounds-per-match)
  ;; Automata Automata ->* Number Number Any Any Any Any 
  (define (match-up auto1 auto2)
    (for/fold ([sum1 0]
               [sum2 0]
               [auto1 auto1]
               [auto2 auto2]
               [strat1 (automaton-current-state auto1)]
               [strat2 (automaton-current-state auto2)])
              ([_ (in-range rounds-per-match)])
      [define next1 (react strat2 auto1)]
      [define next2 (react strat1 auto2)]
      [define-values (d1 d2) (match-strategies strat1 strat2)]
      (values (+ d1 sum1) (+ d2 sum2) (update auto1 next1) (update auto2 next2) next1 next2)))
  ;; -- IN --
  (let pair-up-loop ([population population])
    (cond
      [(or (empty? population) (empty? (rest population))) '()]
      [else (define-values (sum1 sum2 _1 _2 _3 _4)
              (match-up (first population) (second population)))
            (list* sum1 sum2 (pair-up-loop (rest (rest population))))])))

;; Strategy Strategy ->* N N 
(define (match-strategies strat1 strat2)
  (cond 
    [(and (equal? COOPERATE strat1) (equal? COOPERATE strat2)) (values 3 3)]
    [(and (equal? COOPERATE strat1) (equal? DEFECT strat2))    (values 0 4)]
    [(and (equal? DEFECT strat1) (equal? COOPERATE strat2))    (values 4 0)]
    [else                                                      (values 1 1)]))
