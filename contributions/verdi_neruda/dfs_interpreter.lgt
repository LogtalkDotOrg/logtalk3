
:- object(dfs_interpreter,
	implements(interpreterp)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Depth-first interpreter for general logic programs.']).

	prove(Goal, DB) :- 
		prove_body([Goal], -1, DB).

	prove(Goal, Limit, DB) :-
		prove_body([Goal], Limit, DB).

	prove_body([], _, _).
	prove_body([Goal|Goals], Limit, DB) :-
		Limit =\= 0,
		Limit0 is Limit - 1,
		(	Goal = not(G) ->
			(	prove(G, DB) ->
				fail
			;	counter::increment, %Inference counting.
				prove_body(Goals, Limit0, DB)
			)
		;	rule(Goal, Body, Goals, DB),
			counter::increment, %Inference counting.
			prove_body(Body, Limit0, DB)
		).

	rule(Head, Body, Tail, DB) :-
		(	Head = {Goal} ->
			call(Goal),
			Body = Tail
		;	DB::rule(Head, Body, Tail)
		).	

:- end_object.
