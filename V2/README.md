A Finite-state Simulation of Social-science Games
=================================================

Running the simulation
----------------------

To run five simulations in a row: 

    $ raco test fsm0.rkt 


To run a single simulation: 

    $ racket -tm fsm0.rkt 
or 

    $ chmod +x fsm0.rkt 
    $ ./fsm0.rkt 

Introduction
------------

This program is a simple simulation of the tension between individuals
focused on their own interest and the interests of the general population. 

Each individual is represented by an automaton that can pursue one of two
strategies: cooperate with others in interactions or defect. Each
interaction of two individuals is evaluated according to the following
matrix:

| paypff matrix |  cooperate |  defect |
| ------------- | ---------- | ------- |
|cooperate  |     3,3    |    0,4  |
|defect     |     4,0    |    1,1  |

That is, when a defecting individual meets on a cooperating one, the former
receives four "credits" and the latter none. After an interaction is done,
each individual chooses how to act during the next one. These transition
are described via a finite-state diagram (or matrix). 

Here are the transition matrices for three classical individuals:

* the selfless individual starts with the cooperative strategy
  and sticks to it, no matter what the other individual does: 
 
      *        | cooperate | defect 
------------- -| --------- | ---------
   cooperate   | cooperate | cooperate
   defect      | cooperate | cooperate

* the self-interested individual starts with the defecting strategy
  and sticks to it, no matter what the other individual does: 

      *      | cooperate | defect 
------------ | --------- | ------------
 cooperate   |   defect  | defect
 defect      |   defect  | defect

* the tit-for-tat individual starts with the cooperative strategy
  but switches to defective if some individual defects during an
  interaction. It switches back to a cooperative attitude after a
  cooperative interaction: 

    *       | cooperate | defect 
----------- | --------- | ------------
 cooperate  | cooperate | defect
 defect     | cooperate | defect


Interpretation 
--------------

If per-automaton payoff trends towards three credits, the population
consists of selfless, cooperating members. If the average payoff converges
to one credit, the population consists of self-interested individuals;
everybody defects everybody else. 

Details of the Simulation 
-------------------------

The simulation runs for c cycles. During each cycle, every individual
interacts with some other individual for r rounds. The simulation program
records the average payoff of all interactions. At the end of a cycle, s
individuals are replaced with s new ones. The replacements are cloned from
individuals in the existing population. The choice process gives each
individual a chance for cloning in proportion to its high payoffs from the
preceding round of simulation. Finally, the simulation wipes clean the
individuals, that is, it eliminates the current payoff (100% estate tax)
and it resets their starting strategy to the initial one. 


Files 
-----

fsm0:       is the top-level file 
evolution:  runs the complete simulation 
population: a data representation for a population 
automata:   a data representation for automata 

Acknowledgment 
--------------

The original code is Nguyen Linh Chi from the University of Trento, Italy. 
See [github](https://github.com/ayaderaghul/sample-fsm)

Chi, in turn, acknowledges a [blog post of Tim Thornton](http://timthornton.net/blog/id/538fa6f2f09a16ba0674813d)
