
:- object(bench_planner,
	implements(databasep)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'A simple database for planning in a blocks world.']).

	member(X, [X|_]) <- true.
	member(X, [_|Xs]) <-
		member(X, Xs).

	transform(State1,State2,Plan) <- 
		transform(State1,State2, [State1], Plan).

	transform(State,State,_,[]) <- true.
	transform(State1,State2,Visited,[Action|Actions]) <-
	   legal_action(Action,State1) &
	   update(Action,State1,State) &
	   not(member(State,Visited)) &
	   transform(State,State2,[State|Visited],Actions).

	legal_action(to_place(Block,Y,Place),State) <- 
	   on(Block,Y,State) &
	   clear(Block,State) &
	   place(Place) &
	   clear(Place,State).
	legal_action(to_block(Block1,Y,Block2),State) <- 
	   on(Block1,Y,State) &
	   clear(Block1,State) &
	   block(Block2) &
	   {Block1 \== Block2} &
	   clear(Block2,State).

	clear(X,State) <- not(above(X, State)).
	above(X, State) <- member(on(_, X), State).
	on(X,Y,State) <- member(on(X,Y),State).

	update(to_block(X,Y,Z),State,State1) <-
	   substitute(on(X,Y), on(X,Z),State,State1).
	update(to_place(X,Y,Z),State,State1) <-
	   substitute(on(X,Y),on(X,Z),State,State1).

	substitute(X,Y,[X|Xs],[Y|Xs]) <- true.
	substitute(X,Y,[X1|Xs],[X1|Ys]) <-
		{X \== X1} &
		substitute(X,Y,Xs,Ys).

	block(a) <- true. block(b) <- true. block(c) <- true.

	place(p) <- true. place(q) <- true. place(r) <- true.

	initial_state(test, [on(a, b), on(b, p), on(c, r)]).
	final_state(test, [on(a, b), on(b, c), on(c, r)]).

	bench_goal(transform(I, F, Plan)) :-
		initial_state(Name, I),
		final_state(Name, F).

:- end_object.