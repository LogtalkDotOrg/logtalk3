
:- object(message_counter,
	implements(monitoring),
	imports(monitor)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2006/12/14,
		comment is 'Message counter monitor.']).

	:- public(report/0).
	:- mode(report, one).
	:- info(report/0,
		[comment is 'Reports current calls and exits message counts.']).

	:- public(stop/0).
	:- mode(stop, one).
	:- info(stop/0,
		[comment is 'Stops message counting.']).

	:- private(calls/2).
	:- dynamic(calls/2).
	:- mode(calls(?object, ?integer), zero_or_more).

	:- private(calls/3).
	:- dynamic(calls/3).
	:- mode(calls(?object, ?predicate_indicator,?integer), zero_or_more).

	:- private(exits/2).
	:- dynamic(exits/2).
	:- mode(exits(?object, ?integer), zero_or_more).

	:- private(exits/3).
	:- dynamic(exits/3).
	:- mode(exits(?object, ?predicate_indicator,?integer), zero_or_more).

	report :-
		forall(
			::calls(Object, Calls),
			(writeq(Object), nl,
			 write('  total of calls: '), write(Calls), nl,
			 write('  total of exits: '),
             (	::exits(Object, Exits) ->
                write(Exits), nl, nl
			 ;	write(0), nl, nl
			 ),
			forall(
				::calls(Object, Functor/Arity, Calls2),
				(write('  '), writeq(Functor/Arity), nl,
				 write('    calls: '), write(Calls2), nl,
				 write('    exits: '),
				 (::exits(Object, Functor/Arity, Exits2) ->
						write(Exits2), nl, nl
						;
						write(0), nl, nl))))).

	stop :-
		::retractall(calls(_, _)),
		::retractall(exits(_, _)),
		::retractall(calls(_, _, _)),
		::retractall(exits(_, _, _)),
		::reset_monitor.

	before(Object, Message, _) :-
		(	::retract(calls(Object, Old)) ->
			New is Old + 1
		;	New = 1
		),
		::assertz(calls(Object, New)),
		functor(Message, Functor, Arity),
		(	::retract(calls(Object, Functor/Arity, Old2)) ->
			New2 is Old2 + 1
		;	New2 = 1
		),
		::assertz(calls(Object, Functor/Arity, New2)).

	after(Object, Message, _) :-
		(	::retract(exits(Object, Old)) ->
			New is Old + 1
		;	New = 1
		),
		::assertz(exits(Object, New)),
		functor(Message, Functor, Arity),
		(	::retract(exits(Object, Functor/Arity, Old2)) ->
			New2 is Old2 + 1
		;	New2 = 1
		),
		::assertz(exits(Object, Functor/Arity, New2)).

:- end_object.
