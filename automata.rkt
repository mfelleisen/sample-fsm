#lang racket

;; An N-states/2-Actions Automaton

(provide
 ;; type Automaton
 ;; type Payoff = N 
 
 ;; Payoff -> Automaton 
 defects
 cooperates
 tit-for-tat
 grim-trigger
 
 ;; Automaton -> Payoff 
 automaton-payoff 
 
 ;; N -> Automaton
 ;; (make-random-automaton n) builds an n states/2 actions automaton
 ;; with a random transition table 
 make-random-automaton
 
 ;; Automaton Automaton N -> Automaton Automaton
 interact
 
 ;; Automaton -> Automaton 
 ;; create new automaton from given one (same initial state)
 clone 
 
 ;; Automaton -> Automaton 
 ;; wipe out the historic payoff
 automaton-reset)

;; =============================================================================
(module+ test
  (provide
   ;; syntax: (check-payoffs? actual expected1 expected2)
   ;; runs actual, expects two automata, compares their payoffs with expected{i}
   check-payoffs?)
  
  (require rackunit)
  
  (define-syntax-rule
    (check-payoffs? actual expected1 expected2)
    (check-equal? (let-values ([(auto1 auto2) actual])
                    (list (automaton-payoff auto1) (automaton-payoff auto2)))
                  (list expected1 expected2))))

;; -----------------------------------------------------------------------------
(define COOPERATE 0)
(define DEFECT    1)
;; -----------------
(define actions#  2)

(struct automaton (current initial payoff table) #:transparent)
(struct state (action dispatch) #:transparent)

;; Automaton   = (automaton Index Index Payoff State*)
;; State*      = [Index ->f State]
;; State       = (state Action Transitions)
;; Transitions = [Action ->f Index]
;; Action      = COOPERATE | DEFECT 
;; Index       = [0,n)
;; [X ->f Y]   = [Vectorof Y | range: X]

;; TODO: Chi suggests keeping around a list of payoffs so that the model
;; can incooperate discounting of future income at some inflation rate.
;; This would also affect relative-average and other functions. 

;; (automaton c c0 p t) :
;;   means the automaton is in state c and its initial state is c0
;;   when an interaction takes place,
;;   -- let n = t(c) 
;;   -- n = [a,i] :
;;   means it takes action a and uses i(o) as the next state
;;   if o is the action taken by the other agent 

(define (make-random-automaton states#)
  (define initial-current (random states#))
  (define (states*) (build-vector states# make-state))
  (define (make-state _) (state (random actions#) (transitions)))
  (define (transitions) (build-vector actions# make-transition))
  (define (make-transition _) (random states#))
  (automaton initial-current initial-current 0 (states*)))

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
  (define observably-equivalent-to-all-defects (make-automaton DEFECT t1))
  (define observably-equivalent-to-tit-for-tat (make-automaton COOPERATE t2))
  
  (check-pred automaton? (make-automaton 0 t1)))

(define (make-automaton current table)
  (automaton current current 0 table))

;; CLASSIC Automata
(define (defects.v0 p0)
  (define defect-transitions
    (transitions DEFECT
                 DEFECT
                 #:i-cooperate/it-cooperates DEFECT 
                 #:i-cooperate/it-defects    DEFECT
                 #:i-defect/it-cooperates    DEFECT
                 #:i-defect/it-defects       DEFECT))
  (automaton DEFECT DEFECT p0 defect-transitions))

;; Chi observes that this definition is smaller than the above,
;; but in my mind less easy to read than .v0
(define (defects p0)
  (automaton 0 0 p0 (vector (state DEFECT (vector 0 0)))))

(define (cooperates.v0 p0)
  (define cooperates-transitions
    (transitions COOPERATE
                 COOPERATE
                 #:i-cooperate/it-cooperates COOPERATE 
                 #:i-cooperate/it-defects    COOPERATE
                 #:i-defect/it-cooperates    COOPERATE
                 #:i-defect/it-defects       COOPERATE))
  (automaton COOPERATE COOPERATE p0 cooperates-transitions))

(define (cooperates p0)
  (define t (vector (state COOPERATE (vector COOPERATE COOPERATE))))
  (automaton COOPERATE COOPERATE p0 t))

(define (tit-for-tat p0)
  (define tit-for-tat-transitions
    (transitions COOPERATE
                 DEFECT
                 #:i-cooperate/it-cooperates COOPERATE 
                 #:i-cooperate/it-defects    DEFECT
                 #:i-defect/it-cooperates    COOPERATE
                 #:i-defect/it-defects       DEFECT))
  (automaton COOPERATE COOPERATE p0 tit-for-tat-transitions))

(define (grim-trigger p0)
  (define grim-transitions
    (transitions COOPERATE
                 DEFECT
                 #:i-cooperate/it-cooperates COOPERATE 
                 #:i-cooperate/it-defects    DEFECT
                 #:i-defect/it-cooperates    DEFECT
                 #:i-defect/it-defects       DEFECT))
  (automaton COOPERATE COOPERATE p0 grim-transitions))

;; Action Action Index Index Index Index -> State*
(define (transitions a1
                     a2
                     #:i-cooperate/it-cooperates cc
                     #:i-cooperate/it-defects    cd
                     #:i-defect/it-cooperates    dc
                     #:i-defect/it-defects       dd)
  (vector (state a1 (vector cc cd)) (state a2 (vector dc dd))))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal? (automaton-reset (automaton 1 1 4 t2)) (automaton 1 1 0 t2)))

(define (automaton-reset a)
  (match-define (automaton current c0 payoff table) a)
  (automaton c0 c0 0 table))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal? (clone (automaton 1 0 4 t2)) (automaton 0 0 0 t2)))

(define (clone a)
  (match-define (automaton current c0 payoff table) a)
  (automaton c0 c0 0 table))

;; -----------------------------------------------------------------------------
(module+ test
  (check-payoffs? (interact (defects 0) (cooperates 0) 10) 40 0)
  (check-payoffs? (interact (defects 0) (tit-for-tat 0) 10) 13 9)
  (check-payoffs? (interact (tit-for-tat 0) (defects 0) 10) 9 13))

(define (interact auto1 auto2 rounds-per-match)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2])
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (values n1 (+ payoff1 p1) n2 (+ payoff2 p2))))
  (values (automaton new1 c1 p1 table1) (automaton new2 c2 p2 table2)))

;; -----------------------------------------------------------------------------
;; PayoffTable = [Vectorof k [Vectorof k (cons Payoff Payoff)]]
(define PAYOFF-TABLE
  (vector (vector (cons 3 3) (cons 0 4))
          (vector (cons 4 0) (cons 1 1))))

;; State State -> [Cons Payoff Payoff]
(define (payoff current1 current2)
  (vector-ref (vector-ref PAYOFF-TABLE current1) current2))
