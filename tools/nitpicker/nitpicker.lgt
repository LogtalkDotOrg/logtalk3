
:- object(nitpicker).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/07/02,
		comment is 'Simple code coverage tool.']).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- public(collect/0).
	:- public(stop/0).
	:- public(reset/0).
	:- public(report/0).

	:- private(collecting_/0).
	:- dynamic(collecting_/0).

	:- private(fired_/3).
	:- dynamic(fired_/3).

	reset :-
		retractall(fired_(_, _, _)).

	collect :-
		(	collecting_ ->
			write('Code coverage is on: collecting information for all entities compiled in debug mode.'), nl
		;	assertz(collecting_),
			write('Code coverage switched on: collecting information for all entities compiled in debug mode.'), nl
		).

	stop :-
		retractall(collecting_),
		write('Code coverage switched off.'), nl.

	report :-
		(	setof(Entity, Pred^N^fired_(Entity, Pred, N), Entities) ->
			report(Entities)
		;	write('No coverage information collected.'), nl	
		).

	report([]).
	report([Entity| Entities]) :-
		report_entity(Entity),
		report(Entities).

	report_entity(Entity) :-
		setof(N, fired_(Entity, Functor/Arity, N), Ns),
		number_of_clauses(Entity, Functor/Arity, Total),
		format('~q: ~q - ~w (~d)~N', [Entity, Functor/Arity, Ns, Total]),
		fail.
	report_entity(Entity) :-
		object_property(Entity, defines(Functor/Arity, Properties)),
		\+ fired_(Entity, Functor/Arity, _),
		memberchk(number_of_clauses(Total), Properties),
		format('~q: ~q - ~w (~d)~N', [Entity, Functor/Arity, [], Total]),
		fail.
	report_entity(Entity) :-
		category_property(Entity, defines(Functor/Arity, Properties)),
		\+ fired_(Entity, Functor/Arity, _),
		memberchk(number_of_clauses(Total), Properties),
		format('~q: ~q - ~w (~d)~N', [Entity, Functor/Arity, [], Total]),
		fail.
	report_entity(_).

	number_of_clauses(Entity, Functor/Arity, Total) :-
		object_property(Entity, defines(Functor/Arity, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(Entity, Functor/Arity, Total) :-
		category_property(Entity, defines(Functor/Arity, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(_, _, (?)).

	:- multifile(user::logtalk_debug_event/2).

	user::logtalk_debug_event(fact(Entity, Fact, N), _) :-
		(	collecting_ ->
			fired(Entity, Fact, N)
		;	true
		).
	user::logtalk_debug_event(rule(Entity, Head, N), _) :-
		(	collecting_ ->
			fired(Entity, Head, N)
		;	true
		).
	user::logtalk_debug_event(top_goal(Goal, TGoal), ExCtx) :-
		user::logtalk_debug_event(goal(Goal, TGoal), ExCtx).
	user::logtalk_debug_event(goal(_, TGoal), _) :-
		{TGoal}.

	fired(Entity, Head, N) :-
		functor(Head, Functor, Arity),
		(	fired_(Entity, Functor/Arity, N) ->
			true
		;	assertz(fired_(Entity, Functor/Arity, N))
		).

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

:- end_object.
