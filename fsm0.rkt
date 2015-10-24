#! /usr/bin/env racket -tm
#lang racket

;; Run a Simulation of Interacting Automata

(provide main)

;; =============================================================================
(require "population.rkt" "utilities.rkt" plot)

(plot-new-window? #t)

;; -> Void
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

;; [Listof Payoff] -> [Listof [List Real Real]]
;; turn average payoffs into a list of Cartesian points 
(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

;; Population N N N -> [Listof Payoff]
;; computes the list of average payoffs over the evolution of population p for
;; c cycles of of match-ups with r rounds per match and at birth/death rate of s
(define (evolve p c s r)
  (cond
    [(zero? c) '()]
    [else (define p2 (match-up* p r))
          (define pp (population-payoffs p2))
          (define p3 (regenerate p2 s))
          (cons (relative-average pp r) (evolve p3 (- c 1) s r))]))


;; -----------------------------------------------------------------------------
(module+ five
  (main)
  (main)
  (main)
  (main)
  (main))

