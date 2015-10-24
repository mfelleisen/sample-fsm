A Finite-state Simulation of Economics Games
============================================

Running the simulation
----------------------

To run five simulations in a row: 

    $ raco test -s five fsm0.rkt 


To run a single simulation: 

    $ racket -tm fsm0.rkt 
or 

    $ chmod +x fsm0.rkt 
    $ ./fsm0.rkt 

Introduction
------------

This program is a simple simulation of the tension between individuals
focused on their own interest and the interests of the general population. 

Individuals are represented by automata. A state in an automaton describes
how an individual acts; the transitions from a state to another state
explain how the individual interacts with some other individual. The
simulation assumes that there is a fixed number of actions for the entire
game. In general, each automata may differ in its number of states from
every other automaton.

A classical game assumes that an individual may take one of two actions:
"cooperate" or "defect". This choice dictates a two-by-two payoff matrix
such as this one: 

| paypff matrix |  cooperate |  defect |
| ------------- |:----------:|:-------:|
|cooperate      |      3,3   |  0,4    |
|defect         |      4,0   |  1,1    |

That is, when a currently defecting individual meets on a currently
cooperating one, the former receives four "credits" and the latter
none. 

Here are the transition matrices for three classical individuals:

* the selfless individual starts with the cooperative strategy
  and sticks to it, no matter what the other individual does: 
 
      *       | cooperate | defect 
------------- | --------- | ---------
   cooperate  | cooperate | cooperate
   defect     | cooperate | cooperate

  It is possible to represent this individual with a single state in which
  it always acts in a cooperative manner, and no matter how the other
  individual acts, it transitions to the very same state. 

* the self-interested individual starts with the defecting strategy
  and sticks to it, no matter what the other individual does: 

      *      | cooperate | defect 
------------ | --------- | ------------
 cooperate   |   defect  | defect
 defect      |   defect  | defect

  It is also possible to represent this individual with a single state in
  which it always defects, and no matter how the other individual acts, it
  transitions to the very same state.


* the tit-for-tat individual starts with the cooperative strategy but
  switches to defecting one when it encounters an individual that defects
  during an interaction. It switches back to a cooperative attitude after a
  cooperative interaction:

    *       | cooperate | defect 
----------- | --------- | ------------
 cooperate  | cooperate | defect
 defect     | cooperate | defect

  Representing such an individual calls for two states. In the first one it
  acts in a cooperative manner. If the transaction partner is cooperative,
  too, it transitions to the same state at the end of the transaction;
  otherwise it moves to the second state. In the second state, this
  individual defects but if the partner is cooperative it goes back to the
  first state; otherwise it stays in the same state. 

The "Nash Demand Game" uses *three* actions: "high", "medium", "low". Each
action represents what kind of share an individual wishes to grab from a
shared pie: eight slices, five slices, two slices. Here is the typical
payoff matrix:

| paypff matrix |  high      | medium  |  low  |
| ------------- |:----------:|:-------:|:-------:|
| high          |      0,0   |  0,0    |  8,2
| medium        |      0,0   |  5,5    |  5,2
| low           |      2,8   |  2,5    |  2,2

That is, when a "high" individual meets a "low" individual, the former
receives eight slices of the pie and the latter two. In general, two
individuals receive the desired slices *if* their total claims to the pie
are below or equal to ten and nothing otherwise. 

The "Ultimatum game" uses *asymmetric* actions. A *proposer* suggests a
division of $10 between two players. The proposer may offer a "low" or a
"fair" share, which represent two and five dollars, respectively. The
responder may take one of two actions: accept or reject. If the responder
accepts, both get the proposed share of the amount; otherwise neither gets
anything. Here is the obvious payoff matrix for this game:

| paypff matrix |  low       |  fair |
| ------------- |:----------:|:-------:|
| accept        |      2,8   |  5,5    |
| reject        |      0,0   |  0,0    |


Current Implementation 
----------------------

The current implementation simulates random N-state, 2-action automata with
the payoff matrix from above.


Interpretation 
--------------

If per-automaton payoff trends towards three credits, the population
consists of selfless, cooperating members. If the average payoff converges
to one credit, the population consists of self-interested individuals;
everybody defects everybody else. 

Details of the Simulation 
-------------------------

The simulation runs for *c* cycles with a population of size *p*. During
each cycle, every individual interacts with one other individual for *r*
rounds. The simulation program records the average payoff of all
interactions. At the end of a cycle, *s* individuals (*s* << *p*) are
replaced with *s* new ones. The replacements are cloned from individuals in
the existing population. The choice process gives each individual a chance
for cloning in proportion to its payoffs from the preceding round of
simulation. Finally, the simulation resets all individuals in the
"regenerated" population, that is, it eliminates the current payoff (100%
estate tax) and it resets their starting strategy to the initial one.

Files 
-----

file | purpose
---- | --------
fsm0 |       is the top-level file 
population | a data representation for a population 
automata   |    a data representation for automata 

Deprecated
----------

V2 and VOO are deprecated versions of this repository. 


Acknowledgment 
--------------

The original code is Nguyen Linh Chi from the University of Trento, Italy. 
See [github](https://github.com/ayaderaghul/sample-fsm)

Chi also reviewed several stages of this code development. 

Chi, in turn, acknowledges a [blog post of Tim Thornton](http://timthornton.net/blog/id/538fa6f2f09a16ba0674813d)

The original ideas are due to Axelrod (1987) with rule-based automata that
have memory. Fogel (1993) showed how to use finite state machines. 
