#lang racket

(provide
 ;; type Population = [Listof X] of even length

 ;; N [N -> X] -> Population
 ;; (build-population n c) for even n, build a population of size n with c
 build-population

 ;; Population N [X X -> Y] -> [Listof Y]
 ;; (match-ups p r) matches up neighboring pairs of
 ;; automata in population p for r rounds per match 
 match-ups

 ;; Population [Listof [0,1]] N -> Population 
 death-birth)

;; ---------------------------------------------------------------------------------------------------

(define (build-population n f)
  (build-list n f))

(define (match-ups population rounds-per-match interact)
  ;; Automata Automata ->* Number Number Any Any Any Any 
  ;; the sum of pay-offs for the two respective automata over all rounds 
  (define (match-up auto1 auto2)
    (for/fold ([sum1 0] [sum2 0] [auto1 auto1] [auto2 auto2]) ([_ (in-range rounds-per-match)])
      (define-values (d1 d2 next1 next2) (interact auto1 auto2))
      (values (+ d1 sum1) (+ d2 sum2) next1 next2)))
  ;; -- IN --
  (let pair-up-loop ([population population])
    (cond
      [(empty? population) '()] ; a population is a list of even length 
      [else (define-values (sum1 sum2 _1 _2) (match-up (first population) (second population)))
            (list* sum1 sum2 (pair-up-loop (rest (rest population))))])))

(define (death-birth population fitness speed)
  ;; MF: why are we dropping of the first 'speed'?
  [define survivors (drop population speed)]
  [define substitutes (randomise-over-fitness population fitness speed)]
  (shuffle (append survivors substitutes)))

;; Population [Listof [0,1]] N -> Population 
;; spawn another set of fitt automata
;; at the end of the cycle, kill N%; then spawn child-copies of "fittest"
(define (randomise-over-fitness population fitness speed)
  (for/list ([n (in-range speed)])
    [define r (random)] ;; SHOULDN"T THIS LINE BE OUTSIDE OF THE for/list COMPREHENSION?
    (for/last ([p (in-list population)]
               [f (in-list fitness)]
              #:break (< r f))
      p)))
