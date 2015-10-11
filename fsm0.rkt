#lang racket
;; INTRODUCTION
;; I generate a population of finite state automata randomly
;; in each cycle, they are pair-matched to play a repeated game
;; their *fitness* is their relative payoff.
;; At the end of the cycle, I kill randomly 10%
;; and resurrect an equivalent amount to keep the population constant.
;; Automata are ressurrect based on their *fitness*.

;; Technically, the fitness vector is a vector that sums up to 1,
;; and we randomised over this vector to choose which automata to ressurrect
;; independently.

;; The game is a 2 player game,
;; each player has 2 possible stragies {Cooperate, Defect},
;; and they move simultaneously.
;; Here is the payoff matrix:
;;               Cooperate      Defect
;; Cooperate        3,3           0,4
;; Defect           4,0           1,1
;; This is a very familiar game in social science.
;; If both players "cooperate", they both get the reward of 3.
;; If one cooperates and the other defects, the cooperator is called "sucker"
;; because he gets 0 and the other gets 4. The payoff 4 is called "temptation".
;; If both defect, they both get the "punishment" payoff of 1.
;; This game captures the tradeoff between self interest (temptation)
;; and social welfare (both get reward).

(provide main)

(module automaton racket
  
  (provide
   COOPERATE
   DEFECT 
   automaton-current-state
   A
   react
   update)
  
  ;; AUTOMATON
  (struct automaton (current-state states) #:transparent #:mutable)
  (struct state (name actions) #:transparent #:mutable)
  (struct action (event result) #:transparent #:mutable)
  ;; Automaton = (automaton Name [Listof State])
  ;;           % name of current state, combined with transitions
  ;; State     = (state Name [Listof Action])
  ;;           % name of state, combined with transitions in response to events 
  ;; Action    = (action Event Name)
  ;; Name      : unspecified
  ;;           % use Name = Event
  ;; Event     = COOPERATE | DEFECT
  (define COOPERATE 0)
  (define DEFECT    1)
  
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

  (define (one-of . x)
    (list-ref x (random (length x))))

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
  (define grim-trigger (create 0 0 1 1 1)))

(module evolve racket
  (provide
   evolve)
  
  (require (submod ".." automaton))
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
        ;; MF: THIS LOOKS LIKE IT MAY "RESURRECT" AUTOM. THAT ARE ALIVE
        [define successors (randomise-over-fitness accum-fitness population speed)]
        [define new-population (shuffle (append survivors successors))]
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
  
  ;; REGENERATE FITTEST AUTOMATA
  ;; at the end of the cycle, i kill 10%
  ;; so i resurrect automata by randomising over the fitness vector
  (define (randomise-over-fitness accumulated-payoff-percentage population speed)
    (for/list ([n (in-range speed)])
      [define r (random)]
      (for/and ([p (in-list population)][a (in-list accumulated-payoff-percentage)]
                                        #:break (< r a))
        p)))
  
  ;; Population N -> [Listof Number]
  ;; MATCH POPULATION
  (define (match-population population rounds-per-match)
    (let loop ([population population])
      (cond
        [(empty? population) '()]
        [(empty? (rest population)) '()]
        [else
         (define auto1 (first population))
         (define auto2 (second population))
         (define-values (sum1 sum2 _1 _2 _3 _4)
           (for/fold ([sum1 0]
                      [sum2 0]
                      [auto1 auto1]
                      [auto2 auto2]
                      [strat1 (automaton-current-state auto1)]
                      [strat2 (automaton-current-state auto2)])
                     ([_ (in-range rounds-per-match)])
             [define next1 (react strat2 auto1)]
             [define next2 (react strat1 auto2)]
             [define-values (step1 step2) (match-strategies strat1 strat2)]
             (values (+ step1 sum1) (+ step2 sum2) (update auto1 next1) (update auto2 next2) next1 next2)))
         (list* sum1 sum2 (loop (rest (rest population))))])))
  
  ;; Strategy Strategy ->* N N 
  (define (match-strategies strat1 strat2)
    (cond 
      [(and (equal? COOPERATE strat1) (equal? COOPERATE strat2)) (values 3 3)]
      [(and (equal? COOPERATE strat1) (equal? DEFECT strat2))    (values 0 4)]
      [(and (equal? DEFECT strat1) (equal? COOPERATE strat2))    (values 4 0)]
      [else                                                      (values 1 1)])))

(module full racket
  (provide main)
  (require (submod ".." evolve) (submod ".." automaton) plot) ; this is to plot the result
  
  (plot-new-window? #t)
  
  ;; the result is the population average:
  ;; how much an average automaton gets in the game in each cycle
  ;; if the average is 3, the society is in a prosperous period
  ;; in which all are cooperators
  ;; if the average gets down to 1, the society is in a state
  ;; of everybody defecting everybody
  (define (main)
    (time
     (plot-mean
      (evolve (A) 1000 10 20))))
  
  ;; TV?
  (define (plot-mean data)
    (define coors (map list (build-list (length data) values) data))
    (plot (lines coors))))

;; ACKNOWLEDGEMENT
;; Thanks to the blog post of Tim Thornton,
;; http://timthornton.net/blog/id/538fa6f2f09a16ba0674813d
;; i know where to start
;; Thanks to Racket mailing list
;; https://groups.google.com/forum/#!topic/racket-users/4o1goSwrLHA
;; and IRC #racket for all the discussions
;; http://pastebin.com/sxrCnwRV

(require 'full)