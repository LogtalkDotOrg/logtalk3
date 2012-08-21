
:- object(counter).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Counter implemented with asserta/retract.']).

	:- initialization(init).

	:- public(increment/0).
	:- mode(increment, one).
	:- info(increment/0, [
		comment is 'Increment the counter by 1.']).

	:- public(increase/1).
	:- mode(increase(+number), one).
	:- info(increase/1, [
		comment is 'Increments the counter by the specified amount.',
		argname is ['I']]).
	
	:- public(set/1).
	:- mode(set(+number), one).
	:- info(set/1, [
		comment is 'Sets the counter to the specified amount.',
		argname is ['N']]).

	:- public(value/1).
	:- mode(value(?number), one).
	:- info(value/1, [
		comment is 'Gets the current value of the counter.',
		argname is ['N']]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets the counter to zero.']).

	:- private(c/1).
	:- dynamic(c/1).

	init :-
		retractall(c(_)),
		asserta(c(0)).

	increment :-
		retract(c(N0)),
		N is N0 + 1,
		asserta(c(N)).
   
	increase(I) :-
		retract(c(N0)),
		N is N0 + I,
		asserta(c(N)).   

	set(N) :-
		retract(c(_)),
		asserta(c(N)).   

	value(N) :- 
		c(N).

	reset :- 
		retract(c(_)),
		asserta(c(0)).

:- end_object.
