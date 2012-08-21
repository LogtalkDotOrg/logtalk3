
:- object(bup_interpreter,
	implements(interpreterp)).

	:- info([
		version is 1.0,
		author is 'Ulf Nilsson. Ported to Logtalk and augmented with negation by Victor Lagerkvist.',
		date is 2010/06/13,
		comment is 'Semi-naive bottom-up interpreter for general (stratified) logic programs. Magic transformation is realized through an expansion hook.']).

	prove(Goal, DB) :-
		prove(Goal, -1, DB).			
	%%Does not work with negated goals! This is a minor issue since these goals
	%%can be rewritten as rules instead.
	prove(Goal, Limit, DB) :-
		magic::magic(Goal, MagicGoal),
		prove(Goal, [MagicGoal], [MagicGoal], _FixPoint, Limit, DB).

	prove(Goal, I, DI, FixPoint, Limit, DB) :-
		subsumption_iterate(Goal, I, DI, [], Pending, FixPoint0, Limit, DB),
		(	Pending = [] ->
			FixPoint = FixPoint0
		;	satisfy_negative_literals(Pending, FixPoint0, Satisfied),
			subsumption_union(FixPoint0, Satisfied, FixPoint1),
			prove(Goal, FixPoint1, Satisfied, FixPoint, Limit, DB)
		).

	satisfy_negative_literals([], _, []).
	satisfy_negative_literals([not(X)|Pending], FixPoint, Satisfied) :-
		counter::increment, %Inference counting.
		(	\+ list::member(X, FixPoint) ->
			Satisfied = [not(X)|Satisfied1],
			satisfy_negative_literals(Pending, FixPoint, Satisfied1)
		;	satisfy_negative_literals(Pending, FixPoint, Satisfied)						
		).

	subsumption_iterate(Goal, _, DI, _, _, _, _, _) :-
		list::member(Goal, DI).
	subsumption_iterate(Goal, I, DI, Pending0, Pending, Fix, Limit, DB) :-
		Limit \= 0,
		Limit0 is Limit - 1,
		debug((
			write('I is: '), write(I), nl,
			write('DI is: '), write(DI), nl,
			write('Pending0 is: '), write(Pending0), nl
		)),
		subsumption_next(I, DI, NextI, NextDi, NextPending, DB),
		(	NextDi = [], NextPending = [] ->
			Fix = NextI,
			Pending = Pending0
		;	list::append(NextPending, Pending0, Pending1),
			list::sort(Pending1, Pending2),
			subsumption_iterate(Goal, NextI, NextDi, Pending2, Pending, Fix, Limit0, DB)
		).

	subsumption_next(I, Di, NextI, NextDi, Pending, DB) :-
		collect(I, Di, Tmp, Pending, DB),
		subsumption_sort(Tmp, NextDi),
		subsumption_union(I, NextDi, NextI).

	collect(I, Di, Heads, Pendings, DB) :-
		findall(
			Head,
			(DB::rule(Head, Body, PosOrNeg),
			 debug((write('Trying rule: '), write(rule(Head, Body, PosOrNeg)), nl)),
			 satisfy_one(Body, Di, NewBody, DB),
			 debug((write(rule(Head, Body, PosOrNeg)),nl)),
			 satisfy_all(NewBody, I, [], DB),
			 debug((write('Rule satisfied: '), write(rule(Head, Body, PosOrNeg)), nl)),
			 \+ subsumption_member(Head, I)),
			Heads),
		findall(
			Pending,
			(DB::rule(Head, Body, negative),
			 satisfy_one(Body, Di, NewBody, DB),
			 satisfy_all(NewBody, I, [Pending], DB)),
			Pendings).

	subsumption_member(X, [Y|Ys]) :-
		(	subsumed(X, Y) ->
			true
		;	X @>= Y,
			subsumption_member(X, Ys)
		).

	subsumption_sort([], []) :-
		!.
	subsumption_sort([X], [X]) :-
		!.
	subsumption_sort(UnSorted, Sorted) :-
		split(UnSorted, UnSorted1, UnSorted2),
		subsumption_sort(UnSorted1, Sorted1),
		subsumption_sort(UnSorted2, Sorted2),
		subsumption_union(Sorted1, Sorted2, Sorted).

	subsumption_union([], Xs, Xs) :-
		!.
	subsumption_union(Xs, [], Xs) :-
		!.
	subsumption_union([X|Xs], [Y|Ys], Zs) :-
		subsumed(Y, X), !, subsumption_union([X|Xs], Ys, Zs).
	subsumption_union([X|Xs], [Y|Ys], Zs) :-
		subsumed(X, Y), !, subsumption_union(Xs, [Y|Ys], Zs).
	subsumption_union([X|Xs], [Y|Ys], [X|Zs]) :-
		X @< Y, !, subsumption_union(Xs, [Y|Ys], Zs).
	subsumption_union([X|Xs], [Y|Ys], [Y|Zs]) :-
		subsumption_union([X|Xs], Ys, Zs).

	satisfy_one([X|Xs], I, Xs, DB) :-
		X \= {_},
		satisfy_atom(I, X, DB).
	satisfy_one([X|Xs], I, [X|Ys], DB) :-
		satisfy_one(Xs, I, Ys, DB).

	satisfy_all([], _, [], _).
	satisfy_all([not(X)|Xs], Int, Pending, DB) :-
		!,
		(	satisfy_atom(Int, not(X), DB) ->
			satisfy_all(Xs, Int, Pending, DB)
		;	satisfy_atom(Int, X, DB) -> 
			fail
		;	Pending = [not(X)]
		).
						  
	satisfy_all([X|Xs], Int, Pending, DB) :-
		satisfy_atom(Int, X, DB),
		satisfy_all(Xs, Int, Pending, DB).

	satisfy_atom(_, {A}, _) :-
		!,
		counter::increment,		%Inference counting.			  
		call(A).

	satisfy_atom([X| Xs], A, DB) :-
		(	counter::increment,	%Inference counting.
			copy_term(X, A)
		;	satisfy_atom(Xs, A, DB)
		).

	split([], [], []).
	split([X|Xs], [X|Ys], Zs) :-
		split(Xs, Zs, Ys).

	%%The double negation is a dirty hack to avoid binding any variables.
	subsumed(X, Y) :-
		%counter::increment, %Uncomment this if the subsumed operation should be counted
							 %as 1 inference. 
		\+ \+ term::subsumes(Y, X).

:- end_object.
