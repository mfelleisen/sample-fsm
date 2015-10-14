#lang racket

(provide
 ;; type Automaton
 automaton

 ;; type Payoff = N 
 
 ;; Automaton -> Payoff 
 automaton-payoff 

 ;; N N -> Automaton
 ;; (make-random-automaton n k) builds an n states x k inputs automaton
 ;; with a random transition table 
 make-random-automaton

;; Automaton Automaton -> Automaton Automaton
 ;; give each automaton the reaction of the other in the current state
 ;; determine payoff for each and transition the automaton
 interact
 
 ;; Automaton -> Automaton 
 ;; wipe out the historic payoff
 reset)

;; -----------------------------------------------------------------------------
;; Representing Automata with n States that can React to k Inputs

(module+ test
  (require rackunit))

(define COOPERATE 0)
(define DEFECT    1)

(struct automaton (current payoff table) #:transparent)
;; Automaton  = (automaton Payoff State Table)
;; Table      = [Vectorof n Transition])
;; Transition = [Vectorof k State]
;; State      = [0,n)
;; Input      = [0,k)
;; Payoff      = N

(define (make-random-automaton n k)
  ;; [Any -> Transition]
  (define (make-transition _i)
    (build-vector k (lambda (_) (random n))))
  (automaton (random n) 0 (build-vector n make-transition)))

;; -----------------------------------------------------------------------------
;; State Table -> Automaton
(module+ test
  (define t1
    (vector
     (vector 0 0)
     (vector 1 1)))
  (define t2
    (vector
     (vector 0 1)
     (vector 0 1)))
  (define a1 (make-automaton DEFECT t1))
  (define a2 (make-automaton COOPERATE t2))
  
  (check-pred automaton? (make-automaton 0 t1)))

(define (make-automaton current table)
  (automaton current 0 table))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal? (reset (automaton 1 4 t2)) (automaton 1 0 t2)))

(define (reset a)
  (match-define (automaton current payoff table) a)
  (automaton current 0 table))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal? (let-values ([(b1 b2) (interact a1 a2)]) (list b1 b2))
                (list
                 (automaton 1 0 t1)
                 (automaton 1 4 t2))))

(define (interact a1 a2)
  (match-define (automaton current1 payoff1 table1) a1)
  (match-define (automaton current2 payoff2 table2) a2)
  (match-define (cons p1 p2) (payoff current1 current2))
  (define n1 (vector-ref (vector-ref table1 current1) current2))
  (define n2 (vector-ref (vector-ref table1 current1) current2))
  (define next1 (automaton n1 (+ payoff1 p1) table1))
  (define next2 (automaton n2 (+ payoff2 p2) table2))
  (values next1 next2))

;; -----------------------------------------------------------------------------
;; PayoffTable = [Vectorof k [Vectorof k (cons Payoff Payoff)]]
(define PAYOFF-TABLE
  (vector (vector (cons 3 3) (cons 4 0))
          (vector (cons 0 4) (cons 1 1))))

;; State State -> [Cons Payoff Payoff]
(define (payoff current1 current2)
  (vector-ref (vector-ref PAYOFF-TABLE current1) current2))