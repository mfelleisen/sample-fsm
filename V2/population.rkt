#lang racket

; evolution: pair of neighbors against each other, collect payoffs per round
; main: plot payoffs

;; -----------------------------------------------------------------------------
;; Populations of Automata where Each Carries its Life Time Payoff  

(provide
 ;; type Population
 
 ;; N -> Population
 ;; (build-population n c) for even n, build a population of size n 
 ;; with c constraint: (even? n)
 build-random-population
 
 ;; Population N -> Population
 ;; (match-ups p r) matches up neighboring pairs of
 ;; automata in population p for r rounds 
 match-ups
 
 ;; Population N -> Population 
 ;; (death-birth p r) replaces r elements of p with r "children" of 
 ;; randomly picked fittest elements of p
 ;; constraint (< r (length p))
 death-birth)

;; ---------------------------------------------------------------------------------------------------

(require "automata.rkt" "utilities.rkt")

(module+ test
  (require rackunit))

;; type [Population X] = (Cons [Vectorof X] [Vectorof X])
;; the first vector carries the current "citizens", death-birth switches the two 

(define DEF-COO 2)

;; -----------------------------------------------------------------------------
(define (build-random-population n)
  (cons (build-vector n (lambda (_) (make-random-automaton DEF-COO DEF-COO)))
        (make-vector n #false)))

;; -----------------------------------------------------------------------------
(define (match-ups population0 rounds-per-match)
  (define population (car population0))
  ;; comment out this line if you want cummulative payoff histories
  (set! population (for/vector ([x population]) (reset x)))
  ;; -- IN --
  (for ([i (in-range (- (vector-length population) 1))])
    (define p1 (vector-ref population i))
    (define p2 (vector-ref population (+ i 1)))
    (define-values (a1 a2) (match-up p1 p2))
    (vector-set! population i a1)
    (vector-set! population (+ i 1) a2))
  population0)

;; Automata Automata N ->* Automata Automata
;; the sum of pay-offs for the two respective automata over all rounds 
(define (match-up auto1 auto2 rounds-per-match)
  (for/fold ([auto1 auto1] [auto2 auto2]) ([_ (in-range rounds-per-match)])
    (interact auto1 auto2)))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal?
   (death-birth (cons (vector (automaton 0 1 't1)) (vector #false)) 1)
   (cons (vector (automaton 0 1 't1)) (vector (automaton 0 1 't1))))
  
  (check-equal?
   (death-birth
    (cons (vector (automaton 0 1 't1)  (automaton 0 9 't1))
          (vector #false #false)) 1)
   (cons (vector (automaton 0 9 't1) (automaton 0 9 't1))
         (vector (automaton 0 9 't1) (automaton 0 9 't1)))))

(define (death-birth population0 rate)
  (define population (car population0))
  (define fitness (payoff-percentages population))
  ;; MF: why are we dropping of the first 'speed'?
  [define substitutes (randomise-over-fitness population fitness rate)]
  (for ([i (in-range rate)][p substitutes])
    (vector-set! population i p))
  (shuffle-vector population (cdr population0)))

;; [Vectorof Automata] -> [Listof [0,1]]
;; from the matching result, calculate the accumulated fitness

(module+ test
  (check-equal? (payoff-percentages (vector (automaton 0 1 't1)))
                '(1.0))
  (check-equal? (payoff-percentages
                 (vector (automaton 0 2 't1) (automaton 0 2 't2)))
                '(.5 1.0))
  (check-equal? (payoff-percentages
                 (vector (automaton 0 2 't1) (automaton 0 8 't2)))
                '(.2 1.0)))

(define (payoff-percentages population)
  (define payoffs (for/list ([x population]) (automaton-payoff x)))
  (define payoff-sum (sum payoffs))
  (define-values (accumulated _)
    (for/fold ([accumulated (list)] [init 0]) ([y (in-list payoffs)])
      (define next-init (exact->inexact (+ init (/ y payoff-sum))))
      (values (cons next-init accumulated) next-init)))
  (reverse accumulated))

;; Population [Listof [0,1]] N -> [Listof Automaton]
;; spawn another set of fittest automata
;; at the end of the cycle, kill N%; then spawn child-copies of "fittest"

;; This procedure uses an independent Bernoulli draw. We independently
;; draw a random number (associated with an automaton) for 10 times. How
;; likely an automaton is chosen depends on its own fitness (its interval
;; in the unit scale of the accumulated percentages.)

(module+ test
  (define p0 (vector (automaton 0 1 't1)  (automaton 0 9 't1)))
  (define p1 (list (automaton 0 9 't1)))
  ;; this test case fails if (random) picks a number < .10
  (check-equal?
   (randomise-over-fitness p0 (payoff-percentages p0) 1)
   (list (automaton 0 9 't1))))

(define (randomise-over-fitness population fitness speed)
  (define population* (vector->list population))
  (for/list ([n (in-range speed)])
    [define r (random)]
    ; (displayln r)
    (let choose ([population* (rest population*)]
                 [fitness fitness]
                 [previous (first population*)])
      (cond
        [(empty? population*) previous]
        [else
         (define f (first fitness))
         (define p (first population*))
         (if (< r f)
             p
             (choose (rest population*) (rest fitness) p))]))))

;; [Vectorof X] {Vectorof X] -> (cons [Vectorof X] [Vectorof X])
;; effect: shuffle vector b into vector a
;; constraint: (= (vector-length a) (vector-length b))
;; Fisher-Yates Shuffle

(module+ test
  (check-pred
   (lambda (r)
     (member r
             (list (cons (vector 1 2) (vector 1 2))
                   (cons (vector 2 1) (vector 1 2)))))
   (shuffle-vector (vector 1 2) (vector #f #f))))

(define (shuffle-vector b a)
  ;; copy b into a
  (for ([x (in-vector b)][i (in-naturals)])
    (vector-set! a i x))
  ;; now shuffle a 
  (for ([x (in-vector b)] [i (in-naturals)])
    (define j (random (add1 i)))
    (unless (= j i) (vector-set! a i (vector-ref a j)))
    (vector-set! a j x))
  (cons a b))
