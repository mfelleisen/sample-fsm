A Finite-state Simulation of Social-science Games
=================================================

Running the Simulation
----------------------

To run five simulations in a row: 

    $ raco test -s five fsm0.rkt 


To run a single simulation: 

    $ racket -tm fsm0.rkt 
or 

    $ chmod +x fsm0.rkt 
    $ ./fsm0.rkt 

The Simple Model
----------------

1. V2 is a simple 2-states/2-actions version. It uses a mostly functional
style. The sub-directory typed/ contains a typed functional version. 

2. VOO is also a simple 2-states/2-actions version. It uses a mostly
object-oriented style. The sub-directory typed/ contains a typed OO
version. 

3. The general model uses n-states/2-actions or even an n-states/k-actions
approach. 


The General Model 
-----------------

    ;; Automaton = (automaton Index State Payoff [Index -> State])
    ;; Action    = COOPERATE | DEFECT 
    ;; State     = (state Action [Action -> Index])
    ;; Index     = Natural

    (automaton c c0 p t) :
      means the automaton is in state c and its original state is c0
      when an interaction takes place,
      -- let n = t(c) 
      -- n = [a,i] :
	 means it takes action a and uses i(o) as the next state
	 if o is the action taken by the other agent 

