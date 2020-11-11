
:- object(bench_planner,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for planning in a blocks world.'
	]).

	member(X, [X|_]) if true.
	member(X, [_|Xs]) if
		member(X, Xs).

	transform(State1,State2,Plan) if
		transform(State1,State2, [State1], Plan).

	transform(State,State,_,[]) if true.
	transform(State1,State2,Visited,[Action|Actions]) if
	   legal_action(Action,State1) and
	   update(Action,State1,State) and
	   not(member(State,Visited)) and
	   transform(State,State2,[State|Visited],Actions).

	legal_action(to_place(Block,Y,Place),State) if
	   on(Block,Y,State) and
	   clear(Block,State) and
	   place(Place) and
	   clear(Place,State).
	legal_action(to_block(Block1,Y,Block2),State) if
	   on(Block1,Y,State) and
	   clear(Block1,State) and
	   block(Block2) and
	   {Block1 \== Block2} and
	   clear(Block2,State).

	clear(X,State) if not(above(X, State)).
	above(X, State) if member(on(_, X), State).
	on(X,Y,State) if member(on(X,Y),State).

	update(to_block(X,Y,Z),State,State1) if
	   substitute(on(X,Y), on(X,Z),State,State1).
	update(to_place(X,Y,Z),State,State1) if
	   substitute(on(X,Y),on(X,Z),State,State1).

	substitute(X,Y,[X|Xs],[Y|Xs]) if true.
	substitute(X,Y,[X1|Xs],[X1|Ys]) if
		{X \== X1} and
		substitute(X,Y,Xs,Ys).

	block(a) if true. block(b) if true. block(c) if true.

	place(p) if true. place(q) if true. place(r) if true.

	initial_state(test, [on(a, b), on(b, p), on(c, r)]).
	final_state(test, [on(a, b), on(b, c), on(c, r)]).

	bench_goal(transform(I, F, Plan)) :-
		initial_state(Name, I),
		final_state(Name, F).

:- end_object.