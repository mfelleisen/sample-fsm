#lang typed/racket

;; An N-states, N-inputs Automaton

(require "type-utility.rkt")

(define-type Automaton automaton)
(define-type Payoff Nonnegative-Real)

(provide/type
 (defects (-> Payoff Automaton))
 (cooperates (-> Payoff Automaton))
 (tit-for-tat (-> Payoff Automaton))
 (grim-trigger (-> Payoff Automaton))
 (make-random-automaton
  ;; (make-random-automaton n) builds an n states x n inputs automaton
  ;; with a random transition table 
  (-> Natural Automaton))
 
 (match-pair
   ;; give each automaton the reaction of the other in the current state
   ;; determine payoff for each and transition the automaton
   (-> Automaton Automaton Natural (values Automaton Automaton)))

 (automaton-reset
  ;; wipe out the historic payoff, set back to original state
  (-> Automaton Automaton))
 (clone
  ;; create new automaton from given one (same original state)
  (-> Automaton Automaton)))

(provide
 automaton-payoff
 Automaton
 Payoff)

;; =============================================================================
(module+ test
  (provide
   ;; syntax: (check-payoffs? actual expected1 expected2)
   ;; runs actual, expects two automata, compares their payoffs with expected{i}
   check-payoffs?)
  
  (require typed/rackunit)
  
  (define-syntax-rule
    (check-payoffs? actual expected1 expected2)
    (check-equal? (let-values ([(auto1 auto2) actual])
                    (list (automaton-payoff auto1) (automaton-payoff auto2)))
                  (list expected1 expected2))))

;; -----------------------------------------------------------------------------
(: COOPERATE State)
(define COOPERATE 0)
(: DEFECT State)
(define DEFECT    1)

(define-type State Natural)
(define-type Transition* [Vectorof Transition])
(define-type Transition [Vectorof State])

(struct automaton ({current : State}
                   {original : State}
                   {payoff : Payoff}
                   {table : Transition*}) #:transparent)

(define (make-random-automaton n)
  (: transitions [-> Any Transition])
  (define (transitions _i) (build-vector n (lambda (_) (random n))))
  (define original-current (random n))
  (automaton original-current original-current 0 (build-vector n transitions)))

;; -----------------------------------------------------------------------------
;; State Table -> Automaton
(module+ test
  (: t1 Transition*)
  (define t1
    (vector
     (vector 0 0)
     (vector 1 1)))
  (: t2 Transition*)
  (define t2
    (vector
     (vector 0 1)
     (vector 0 1)))
  (define observably-equivalent-to-all-defects (make-automaton DEFECT t1))
  (define observably-equivalent-to-tit-for-tat (make-automaton COOPERATE t2))
  
  (check-pred automaton? (make-automaton 0 t1)))

(: make-automaton (-> State Transition* Automaton))
(define (make-automaton current table)
  (automaton current current 0 table))

(: transitions (-> #:i-cooperate/it-cooperates State
                   #:i-cooperate/it-defects    State
                   #:i-defect/it-cooperates    State
                   #:i-defect/it-defects  State
                   Transition*))
(define (transitions #:i-cooperate/it-cooperates cc
                     #:i-cooperate/it-defects    cd
                     #:i-defect/it-cooperates    dc
                     #:i-defect/it-defects       dd)
  (vector (vector cc cd)
          (vector dc dd)))

;; CLASSIC AUTOMATA
;; the all defector has 2 states of cooperate and defect
;; but it always defects, no matter what
;; the opponent may play cooperate or defect
;; it doesnt care, it always stay in the state defect

(define defect-transitions
  (transitions #:i-cooperate/it-cooperates DEFECT 
               #:i-cooperate/it-defects    DEFECT
               #:i-defect/it-cooperates    DEFECT
               #:i-defect/it-defects       DEFECT))

(define (defects p0)
  (automaton DEFECT DEFECT p0 defect-transitions))

(define cooperates-transitions
  (transitions #:i-cooperate/it-cooperates COOPERATE 
               #:i-cooperate/it-defects    COOPERATE
               #:i-defect/it-cooperates    COOPERATE
               #:i-defect/it-defects       COOPERATE))

(define (cooperates p0)
  (automaton COOPERATE COOPERATE p0 cooperates-transitions))

;; the tit for tat starts out optimistic, it cooperates initially
;; however, if the opponent defects, it punishes by switching to defecting
;; if the opponent cooperates, it returns to play cooperate again

(define tit-for-tat-transitions
  (transitions #:i-cooperate/it-cooperates COOPERATE 
               #:i-cooperate/it-defects    DEFECT
               #:i-defect/it-cooperates    COOPERATE
               #:i-defect/it-defects       DEFECT))


(define (tit-for-tat p0)
  (automaton COOPERATE COOPERATE p0 tit-for-tat-transitions))

;; the grim trigger also starts out optimistic,
;; but the opponent defects for just once then
;; it jumps to defect forever
;; it doesnt forgive, and doesnt forget

(define grim-transitions
  (transitions #:i-cooperate/it-cooperates COOPERATE 
               #:i-cooperate/it-defects    DEFECT
               #:i-defect/it-cooperates    DEFECT
               #:i-defect/it-defects       DEFECT))

(: grim-trigger (-> Payoff Automaton))
(define (grim-trigger p0)
  (automaton COOPERATE COOPERATE p0 grim-transitions))

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
;; the sum of pay-offs for the two respective automata over all rounds

(module+ test
  (check-payoffs? (match-pair (defects 0) (cooperates 0) 10) 40 0)
  (check-payoffs? (match-pair (defects 0) (tit-for-tat 0) 10) 13 9)
  (check-payoffs? (match-pair (tit-for-tat 0) (defects 0) 10) 9 13))

(define (match-pair auto1 auto2 rounds-per-match)
  (for/fold ([auto1 : Automaton auto1] [auto2 : Automaton auto2])
            ([_ (in-range rounds-per-match)])
    (interact auto1 auto2)))

;; -----------------------------------------------------------------------------
(: interact (-> Automaton Automaton (values Automaton Automaton)))
(module+ test
  (check-equal? (let-values ([(b1 b2) (interact
                                       observably-equivalent-to-all-defects
                                       observably-equivalent-to-tit-for-tat)])
                  (list b1 b2))
                (list
                 (automaton DEFECT DEFECT    4 t1)
                 (automaton DEFECT COOPERATE 0 t2)))
  
  (check-payoffs? (interact (defects 0) (cooperates 0)) 4 0)
  (check-payoffs?
   (for/fold ([auto1 : Automaton (defects 0)]
              [auto2 : Automaton (cooperates 0)])
             ([_ (in-range 2)])
     (interact auto1 auto2))
   8
   0)
  (check-payoffs? (interact (defects 0) (tit-for-tat 0)) 4 0)
  (check-payoffs? (interact (tit-for-tat 0) (defects 0)) 0 4))

(define (interact a1 a2)
  (match-define (automaton current1 c1 payoff1 table1) a1)
  (match-define (automaton current2 c2 payoff2 table2) a2)
  (match-define (cons p1 p2) (payoff current1 current2))
  (define n1 (vector-ref (vector-ref table1 current1) current2))
  (define n2 (vector-ref (vector-ref table2 current2) current1))
  (define next1 (automaton n1 c1 (+ payoff1 p1) table1))
  (define next2 (automaton n2 c2 (+ payoff2 p2) table2))
  (values next1 next2))

;; -----------------------------------------------------------------------------
;; PayoffTable = [Vectorof k [Vectorof k (cons Payoff Payoff)]]
(: PAYOFF-TABLE [Vectorof [Vectorof (cons Payoff Payoff)]])
(define PAYOFF-TABLE
  (vector (vector (cons 3 3) (cons 0 4))
          (vector (cons 4 0) (cons 1 1))))

(: payoff (-> State State [cons Payoff Payoff]))
(define (payoff current1 current2)
  (vector-ref (vector-ref PAYOFF-TABLE current1) current2))
