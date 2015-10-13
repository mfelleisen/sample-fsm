#lang racket

(provide
 ;; type [Population X]
 
 ;; N [N -> X] -> Population
 ;; (build-population n c) for even n, build a population of size n 
 ;; with c constraint: (even? n)
 build-population
 
 ;; Population N [X X -> Y] -> [Listof Y]
 ;; (match-ups p r) matches up neighboring pairs of
 ;; automata in population p for r rounds per match
 match-ups
 
 ;; Population [Listof [0,1]] N -> Population 
 ;; (death-birth p f r) replaces r elements of p with r "children" of 
 ;; randomly picked fittest elements of p
 ;; constraint: (= (vector-length p) (length f))
 ;; constraint: the i-th fitness describes the i-th population 
 death-birth)

;; ---------------------------------------------------------------------------------------------------

;; type [Population X] = (Cons [Vectorof X] [Vectorof X])
;; the first vector carries the current "citizens", death-birth switches the two 

(define (build-population n f)
  (cons (build-vector n f) (make-vector n #false)))

(define (match-ups population0 rounds-per-match interact)
  (define population (car population0))
  (define n (- (vector-length population) 1))
  ;; Automata Automata ->* Number Number Any Any Any Any 
  ;; the sum of pay-offs for the two respective automata over all rounds 
  (define (match-up auto1 auto2)
    (for/fold ([sum1 0] [sum2 0] [auto1 auto1] [auto2 auto2]) ([_ (in-range rounds-per-match)])
      (define-values (d1 d2 next1 next2) (interact auto1 auto2))
      (values (+ d1 sum1) (+ d2 sum2) next1 next2)))
  ;; -- IN --
  (reverse
   (for/fold ([l '()]) ([i (in-range (- (vector-length population) 1))])
     (define p1 (vector-ref population i))
     (define p2 (vector-ref population (+ i 1)))
     (define-values (sum1 sum2 a1 a2) (match-up p1 p2))
     (vector-set! population i a1)
     (vector-set! population (+ i 1) a2)
     (list* sum1 sum2 l))))
     
(define (death-birth population0 fitness rate)
  (define population (car population0))
  ;; MF: why are we dropping of the first 'speed'?
  [define substitutes (randomise-over-fitness population fitness rate)]
  (when (ormap not substitutes)
    (pretty-print substitutes)
    (error 'shuffle-vector "subst!"))
  (for ([i (in-range rate)][p substitutes])
    (vector-set! population i p))
  (shuffle-vector population (cdr population0)))

;; [Population X] [Listof [0,1]] N -> [Listof X]
;; spawn another set of fitt automata
;; at the end of the cycle, kill N%; then spawn child-copies of "fittest"

;; This procedure uses an independent Bernoulli draw. We independently
;; draw a random number (associated with an automaton) for 10 times. How
;; likely an automaton is chosen depends on its own fitness (its interval
;; in the unit scale of the accumulated percentages.) 
(define (randomise-over-fitness population fitness speed)
  (for/list ([n (in-range speed)])
    [define r (random)]
    (define candidate
      (for/last ([p (in-vector population)]
                 [f (in-list fitness)]
                 #:break (< r f))
        p))
    (or candidate (vector-ref population 0))))

;; [Vectorof X] {Vectorof X] -> (cons [Vectorof X] [Vectorof X])
;; effect: shuffle vector b into vector a
;; constraint: (= (vector-length a) (vector-length b))
;; Fisher-Yates Shuffle
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

; (shuffle-vector (build-vector 100 values) (make-vector 100 #false))
