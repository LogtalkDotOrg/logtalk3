%%TODO: Only allow a new bound if some progress has been made.

:- object(iddfs_interpreter(_Increment),
	implements(interpreterp)).	

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Iterative deepening depth-first interpreter for general logic programs. Based on source code from The Craft of Prolog, by Richard O''Keefe. The default value for the increment is 1.',
		parnames is ['Increment']]).

	prove(Goal, DB) :-
		prove(Goal, -1, DB).		

	prove(Goal, Limit, DB) :-
		parameter(1, Increment),
		(	var(Increment) ->
			% use a default value
			Increment = 1
		;	% use parameter value
			true
		),
		prove([Goal], 1, Increment, Limit, DB).

	prove(Goals, Bound, Increment, Limit, DB) :-
		Limit =\= 0,
		bounded_prove(Goals, Bound, Remaining, DB),
		Remaining < Increment.
	prove(Goals, Bound, Increment, Limit, DB) :-
		Limit =\= 0,
		Limit0 is Limit - 1,		 
		Bound1 is Bound + Increment,
		prove(Goals, Bound1, Increment, Limit0, DB).

	bounded_prove([], Remaining, Remaining, _).
	bounded_prove([not(Goal)|Goals], Bound, Remaining, DB) :-
		%%TODO::Rewrite as an if-then-else instead?
		!, 
		Bound1 is Bound - 1,
		Bound1 >= 0,
		%%This is a temporary workaround that allows iddfs to handle negation.
		(	dfs_interpreter::prove(Goal, DB) -> 
			fail 
		; 	counter::increment, %Inference counting.
			bounded_prove(Goals, Bound1, Remaining, DB)
		).
	bounded_prove([Goal|Goals], Bound, Remaining, DB) :-
		Bound1 is Bound - 1,
		Bound1 >= 0,		
		rule(Goal, Body, Goals, DB),
		counter::increment, %Inference counting.
		bounded_prove(Body, Bound1, Remaining, DB).

	rule(Head, Body, Tail, DB) :-
		(	Head = {Goal} ->
			call(Goal),
			Body = Tail
		;	DB::rule(Head, Body, Tail)
		).	
 
:- end_object.
