#lang racket

(provide
 ;; type Population = [Listof Automaton] of even length
 ;; type Payoff     = PositiveNumber 
 
 ;; -> Population 
 A
 ;; Automata Automata -> Payoff Payoff Automata Automata 
 interact)

;; ---------------------------------------------------------------------------------------------------
;; AUTOMATON
(struct automaton (current-state states) #:transparent #:mutable)
(struct state (name actions) #:transparent #:mutable)
(struct action (event result) #:transparent #:mutable)
;; Automaton = (automaton Name [Listof State])
;;           % name of current state, combined with transitions
;; State     = (state Name [Listof Action])
;;           % name of state, combined with transitions in response to events 
;; Action    = (action Event Name)
;; Name      = Event % ??
;; Event     = COOPERATE | DEFECT
(define COOPERATE 'c)
(define DEFECT    'd)

; a transition rule: an event and the result state
; a state: name and many transition rules
; the machine itself: current state + states

(define (A)
  (for/list ([n (in-range 100)])
            (create (one-of COOPERATE DEFECT)
                    (one-of COOPERATE DEFECT)
                    (one-of COOPERATE DEFECT)
                    (one-of COOPERATE DEFECT)
                    (one-of COOPERATE DEFECT))))

;; Name Name Name Name Name -> Automaton
(define (create seed a000 a001 a100 a101)
  (define state1 (state COOPERATE (list (action COOPERATE a000) (action DEFECT a001))))
  (define state2 (state DEFECT    (list (action COOPERATE a100) (action DEFECT a101))))
  (automaton seed (list state1 state2)))

(define (interact auto1 auto2)
  (match-define (automaton strat1 states1) auto1)
  (match-define (automaton strat2 states2) auto2)
  [define-values (payoff1 payoff2) (match-strategies strat1 strat2)]
  (define actions1 (apply-to-first states1 strat1 state-name state-actions))
  (define next1    (apply-to-first actions1 strat2 action-event action-result))
  (define actions2 (apply-to-first states2 strat2 state-name state-actions))
  (define next2    (apply-to-first actions2 strat1 action-event action-result))
  (values payoff1 payoff2 (automaton next1 states1) (automaton next2 states2)))

;; Event Event ->* Payoff Payoff
(define (match-strategies strat1 strat2)
  (cond 
    [(and (equal? COOPERATE strat1) (equal? COOPERATE strat2)) (values 3 3)]
    [(and (equal? COOPERATE strat1) (equal? DEFECT strat2))    (values 0 4)]
    [(and (equal? DEFECT strat1) (equal? COOPERATE strat2))    (values 4 0)]
    [else                                                      (values 1 1)]))

#;
(define (one-of . x) (list-ref x (random (length x))))

;; X X -> X
(define (one-of x y) (if (= (random 2) 0) x y))

;; [Listof X] Y [X -> Y] [X -> Z] -> '() or Z
(define (apply-to-first l x sel f)
  (define result (for/first ([a (in-list l)] #:when (equal? x (sel a))) a))
  (if result (f result) '()))

;; ---------------------------------------------------------------------------------------------------
;; CLASSIC AUTOMATA
(define all-defects (create COOPERATE DEFECT DEFECT DEFECT DEFECT))

(define all-cooperates (create DEFECT COOPERATE COOPERATE COOPERATE COOPERATE))

;; the tit-for-tat strategy starts out optimistic:
;; it cooperates initially
;; if the opponent defects, it switches to defecting
;; if the opponent cooperates, it plays cooperately
(define tit-for-tat (create COOPERATE COOPERATE DEFECT COOPERATE DEFECT))

;; the grim trigger also starts out optimistic,
;; but the opponent defects for just once then
;; it jumps to defect forever
;; it doesnt forgive, and doesnt forget
(define grim-trigger (create 0 0 1 1 1))