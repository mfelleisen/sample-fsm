#! /usr/bin/env racket -tm
#lang racket

(provide main)

;; ---------------------------------------------------------------------------------------------------
(require "evolution.rkt" "population.rkt" plot)

(plot-new-window? #t)

(define (main)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define ps (time (simulation->lines)))
  (define h3 (function (lambda (x) 3) #:color "blue"))
  (define h1 (function (lambda (x) 1) #:color "red"))
  (plot (list h3 h1 ps) #:y-min 0.0 #:y-max 4.0))

;; -> [Listof [List Real Real]]
(define (simulation->lines)
  (define data (evolve (build-random-population 100) 1000 10 20))
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

(module+ test
  (main)
  (main)
  (main)
  (main)
  (main))

