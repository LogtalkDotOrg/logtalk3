
:- object(a_star_interpreter(_W),
	imports(best_first)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'A* interpreter for general logic programs. The parameter W is used to fine tune the behaviour. W = 0 gives us a breadth-first search and W = 1 gives us a greedy best-first search. The default value for W is 0.5.',
		parnames is ['W']]).

	f(Length1, Length2, Depth, Cost) :-
		parameter(1, W),
		(	var(W) ->
			% use a default value
			W = 0.5
		;	% use parameter value
			true
		),
		Cost is Depth * (1 - W) + (Length1 + Length2 - 1) * W.	   

:- end_object.  
