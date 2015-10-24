#lang racket

;; Populations of Automata

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
 ;; (regenerate p r) replaces r elements of p with r "children" of 
 ;; randomly chosen fittest elements of p, also shuffle 
 ;; constraint (< r (length p))
 regenerate)

;; =============================================================================
(require "automata.rkt" "utilities.rkt")

(module+ test
  (require rackunit)
  (require  (submod "automata.rkt" test)))

;; Population = (Cons Automaton* Automaton*)
;; Automaton* = [Vectorof Automaton]

(define DEF-COO 2)

;; -----------------------------------------------------------------------------
(define (build-random-population n)
  (define v (build-vector n (lambda (_) (make-random-automaton DEF-COO))))
  (cons v v))

;; -----------------------------------------------------------------------------
(define (population-payoffs population0)
  (define population (car population0))
  (for/list ([a population]) (automaton-payoff a)))

;; -----------------------------------------------------------------------------
(module+ test
  (define a1 (vector (defects 0) (cooperates 40)))
  (define p1 (cons a1 a1))
  (define e1 (vector (defects 40) (cooperates 0)))
  (define p1-expected (cons e1 a1))
  
  (define a2 (vector (defects 0) (tit-for-tat 0)))
  (define p2 (cons a2 a2))
  (define e2 (vector (defects 13) (tit-for-tat 9)))
  (define p2-expected (cons e2 a2))
  
  (define a3 (vector (tit-for-tat 0) (defects 0)))
  (define p3 (cons a3 a3))
  (define e3 (vector (tit-for-tat 9) (defects 13)))
  (define p3-expected (cons e3 a3))

  ;; these don't work because the population changes 
  ; (check-euqal? (match-up* p2 10) p2-expected)
  ; (check-equal? (match-up* p3 10) p3-expected)  
  (check-equal? (match-up* p1 10) p1-expected))

(define (match-up* population0 rounds-per-match)
  (define population (car population0))
  ;; comment out this line if you want cummulative payoff histories:
  ;; see below in birth-death
  (population-reset population)
  ;; -- IN --
  (for ([i (in-range 0 (- (vector-length population) 1) 2)])
    (define p1 (vector-ref population i))
    (define p2 (vector-ref population (+ i 1)))
    (define-values (a1 a2) (interact p1 p2 rounds-per-match))
    (vector-set! population i a1)
    (vector-set! population (+ i 1) a2))
  population0)

;; Automaton* -> Void
;; effec: reset all automata in a*
(define (population-reset a*)
  (for ([x a*][i (in-naturals)]) (vector-set! a* i (automaton-reset x))))

;; -----------------------------------------------------------------------------
(module+ test
  (define a* (vector (cooperates 1)))
  (define p* (cons a* a*))
  (check-equal? (regenerate p* 1) p*)

  (define a20 (vector (cooperates 1)  (cooperates 9)))
  (define p20 (cons a20 a20))
  
  (check-pred
   (match-lambda 
     [(cons a* b*)
      (member a* (list (vector (cooperates 0) (cooperates 9))
                       (vector (cooperates 9) (cooperates 0))))])
   (regenerate p20 1 #:random .2)))

(define (regenerate population0 rate #:random (q #false))
  (match-define (cons a* b*) population0)
  (define probabilities (payoff->probabilities a*))
  [define substitutes   (choose-randomly probabilities rate #:random q)]
  (for ([i (in-range rate)][p (in-list substitutes)])
    (vector-set! a* i (clone (vector-ref b* p))))
  (shuffle-vector a* b*))

;; Automata* -> [Listof Probability]
;; turn the list of automata into a list of probabilities based on payoffs
;; CONSTRAINT (list-ref @result i) corresponds to probability oe (list-ref a* i)
(define (payoff->probabilities a*)
  (define payoffs (for/list ([x (in-vector a*)]) (automaton-payoff x)))
  (define total   (sum payoffs))
  (for/list ([p (in-list payoffs)]) (/ p total)))

;; Automata* Automata* -> (cons Automata* Automata*)
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
