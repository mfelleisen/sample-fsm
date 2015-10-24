#! /usr/bin/env racket -tm
#lang typed/racket/gui

;; Run a Simulation of Interacting Automata

(require "type-utility.rkt")

(provide/type
 (main (-> (U Void (Instance Snip%)))))


;; =============================================================================
(require "population.rkt" "utilities.rkt" plot)

(plot-new-window? #t)

;; effect: run timed simulation, create and display plot of average payoffs
;; effect: measure time needed for the simulation
(define (main)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define ps
    (time
     (simulation->lines
      (evolve (build-random-population 100) 1000 10 20))))
  (define h3 (function (lambda (x) 3) #:color "blue"))
  (define h1 (function (lambda (x) 1) #:color "red"))
  (plot (list h3 h1 ps) #:y-min 0.0 #:y-max 4.0))

(: simulation->lines (-> [Listof Payoff] renderer2d))
;; turn average payoffs into a list of Cartesian points 
(define (simulation->lines data)
  (define coors
    (for/list : [Listof [List Integer Real]]
      ([d : Payoff (in-list data)][n : Integer (in-naturals)])
      (list n d)))
  (lines coors))

(: evolve (-> Population Natural Natural Natural [Listof Payoff]))
;; computes the list of average payoffs over the evolution of population p for
;; c cycles of of match-ups with r rounds per match and at birth/death rate of s
(define (evolve p c s r)
  (cond
    [(zero? c) '()]
    [else (define p2 (match-up* p r))
          ;; Note: r is typed as State even though State is not exported 
          (define pp (population-payoffs p2))
          (define p3 (regenerate p2 s))
          ;; Note: s same as r
          ({inst cons Payoff [Listof Payoff]}
           (cast (relative-average pp r) Payoff)
           ;; Note: evolve is assigned (-> ... [Listof Probability])
           ;; even though it is explicitly typed ... [Listof Payoff]
           (evolve p3 (- c 1) s r))]))

;; -----------------------------------------------------------------------------
(module+ five
  (main)
  (main)
  (main)
  (main)
  (main))

