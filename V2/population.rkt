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

 ;; Population -> [Listof Payoff]
 population-payoffs
 
 ;; Population N -> Population
 ;; (match-ups p r) matches up neighboring pairs of
 ;; automata in population p for r rounds 
 match-up*
 
 ;; Population N -> Population 
 ;; (death-birth p r) replaces r elements of p with r "children" of 
 ;; randomly picked fittest elements of p
 ;; constraint (< r (length p))
 death-birth)

;; ---------------------------------------------------------------------------------------------------

(require "automata.rkt" "utilities.rkt")

(module+ test
  (require rackunit))

;; Population = (Cons Automaton* Automaton*)
;; Automaton* = [Vectorof Automaton]
;; the first vector carries the current "citizens", death-birth switches the two 

(define DEF-COO 2)

;; -----------------------------------------------------------------------------
(define (build-random-population n)
  (define v (build-vector n (lambda (_) (make-random-automaton DEF-COO DEF-COO))))
  (cons v v))

;; -----------------------------------------------------------------------------
(define (population-payoffs population0)
  (define population (car population0))
  (for/list ([a population]) (automaton-payoff a)))

;; -----------------------------------------------------------------------------
(define (match-up* population0 rounds-per-match)
  (define population (car population0))
  ;; comment out this line if you want cummulative payoff histories:
  (for ([x population][i (in-naturals)]) (vector-set! population i (reset x)))
  ;; -- IN --
  (for ([i (in-range 0 (- (vector-length population) 1) 2)])
    (define p1 (vector-ref population i))
    (define p2 (vector-ref population (+ i 1)))
    (define-values (a1 a2) (match-up p1 p2 rounds-per-match))
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
  (define a* (vector (automaton 0 0 1 't1)))
  (check-equal?
   (death-birth (cons a* a*) 1)
   (cons (vector (automaton 0 0 0 't1)) (vector (automaton 0 0 0 't1))))

  (define a2 (vector (automaton 0 0 1 't1)  (automaton 0 0 9 't1))) 
  (define p2 (cons a2 a2))
  (define ea (vector (automaton 0 0 0 't1) (automaton 0 0 9 't1)))
  (define ep (cons ea ea))
  
  (check-equal? (death-birth p2 1 #:random .2) ep))

(define (death-birth population0 rate #:random (q #false))
  (define population (car population0))
  ;; MF: why are we dropping the first 'speed'?
  [define substitutes (randomise-over-fitness population rate #:random q)]
  (for ([i (in-range rate)][p (in-list substitutes)])
    ;; clone must change if we wanted to accumulate payoffs across cycles
    ;; see above in match-ups
    (vector-set! population i (clone p)))
  (shuffle-vector population (cdr population0)))

;; -----------------------------------------------------------------------------
;; Automaton* N -> [Listof Automaton]
;; (randomise-over-fitness v n) spawn a list of n automata
;; the probabiliy of choosing automaton i is directly proportionally
;; to its payoff relative to the overall payoff for the entire population

;; Nguyen Linh Chi says: 
;; This procedure uses an independent Bernoulli draw. We independently
;; draw a random number (associated with an automaton) for 10 times. How
;; likely an automaton is chosen depends on its own fitness (its interval
;; in the unit scale of the accumulated percentages.)

(module+ test
  (define p0 (vector (automaton 0 0 1 't1)  (automaton 0 0 90 't1)))
  (define p1 (list (automaton 0 0 90 't1)))
  (check-equal? (randomise-over-fitness p0 1 #:random .2)
                (list (automaton 0 0 90 't1))))

(define (randomise-over-fitness a* speed #:random (q #false))
  (define %s (payoff-%s a*))
  ;; (= (length fitness) (length population))
  (for/list ([n (in-range speed)])
    [define r (or q (random))]
    ;; population is non-empty so there will be some p such that ... 
    (for/last ([p (in-vector a*)] [% (in-list %s)] #:final (< r %)) p)))

;; -----------------------------------------------------------------------------
;; Automata* -> [Listof [0,1]]
;; from the matching result, calculate the accumulated fitness

(module+ test
  (check-equal? (payoff-%s (vector (automaton 0 0 1 't1)))
                '(1.0))
  (check-equal? (payoff-%s
                 (vector (automaton 0 0 2 't1) (automaton 0 0 2 't2)))
                '(.5 1.0))
  (check-equal? (payoff-%s
                 (vector (automaton 0 0 2 't1) (automaton 0 0 8 't2)))
                '(.2 1.0)))

(define (payoff-%s a*)
  (define payoffs (for/list ([x (in-vector a*)]) (automaton-payoff x)))
  (define total (sum payoffs))
  (let relative->absolute ([payoffs payoffs][so-far #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            (cons (/ nxt total) (relative->absolute (rest payoffs) nxt))])))

;; -----------------------------------------------------------------------------
;; Automata* -> (cons Automata* Automata*)
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
