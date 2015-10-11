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
   automaton-current-state
   A
   react
   update)
  
  ;; AUTOMATON
  (struct automaton (current-state states) #:transparent #:mutable)
  (struct state (name actions) #:transparent #:mutable)
  (struct action (event result) #:transparent #:mutable)
  ;; Automaton = (automaton State [Listof State])
  ;; State     = (state Name??? [Listof Action])
  ;; Action    = (action Event??? [Listof State])
  
  ; a transition rule: an event and the result state
  ; a state: name and many transition rules
  ; the machine itself: current state + states
  
  ;; GENERATE POPULATION
  (define (A)
    (for/list ([n (in-range 100)])
      (create (random 2) (random 2) (random 2) (random 2) (random 2))))

  (define (create seed a000 a001 a100 a101)
    (automaton seed
               (list (state 0 (list (action 0 a000) (action 1 a001)))
                     (state 1 (list (action 0 a100) (action 1 a101))))))
  
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
  ;; let 0 = cooperate, 1 = defect
  ;; some classic automata
  ;; the all defector has 2 states of cooperate and defect
  ;; but it always defects, no matter what
  ;; the opponent may play cooperate or defect
  ;; it doesnt care, it always stay in the state defect
  (define all-defects (create 1 1 1 1 1))
  (define all-cooperates (create 0 0 0 0 0))
  ;; the tit for tat starts out optimistic, it cooperates initially
  ;; however, if the opponent defects, it punishes by switching to defecting
  ;; if the opponent cooperates, it returns to play cooperate again
  (define tit-for-tat (create 0 0 1 0 1))
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
    (match (list strat1 strat2)
      [(list 0 0) (values 3 3)]
      [(list 0 1) (values 0 4)]
      [(list 1 0) (values 4 0)]
      [(list 1 1) (values 1 1)])))

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