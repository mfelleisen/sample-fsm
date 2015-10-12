#lang racket

(provide
 COOPERATE
 DEFECT 
 automaton-current-state
 A
 react
 update)

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

;; GENERATE POPULATION
(define (A)
  (for/list ([n (in-range 100)])
    (create (one-of COOPERATE DEFECT)
            (one-of COOPERATE DEFECT)
            (one-of COOPERATE DEFECT)
            (one-of COOPERATE DEFECT)
            (one-of COOPERATE DEFECT))))

#;
(define (one-of . x) (list-ref x (random (length x))))
(define (one-of x y) (if (= (random 2) 0) x y))

;; Name Name Name Name Namw -> Automaton
;; seed is either COOPERATE or DEFECT 
(define (create seed a000 a001 a100 a101)
  (define state1 (state COOPERATE (list (action COOPERATE a000) (action DEFECT a001))))
  (define state2 (state DEFECT (list (action COOPERATE a100) (action DEFECT a101))))
  (automaton seed (list state1 state2)))

; extract the result of the needed action, given an event
(define (react event automaton)
  (define name (automaton-current-state automaton))
  (define states (automaton-states automaton))
  (define actions (apply-to-first states name state-name state-actions))
  (apply-to-first actions event action-event action-result))

(define (apply-to-first l x sel f)
  (define result (for/first ([a (in-list l)] #:when (equal? x (sel a))) a))
  (if result (f result) '()))

; update the state of the auto, return the auto
(define (update old-auto new-state)
  (set-automaton-current-state! old-auto new-state)
  old-auto)

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