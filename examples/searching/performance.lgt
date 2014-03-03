%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(performance,
	implements(monitoring)).	% built-in protocol for event handler methods

	:- info([
		version is 1.5,
		author is 'Paulo Moura',
		date is 2013/04/23,
		comment is 'Performance monitor for state space searches.'
	]).

	:- uses(list, [length/2]).
	:- uses(numberlist, [min/2, max/2, sum/2]).
	:- uses(time, [cpu_time/1]).

	:- private(transitions_/3).
	:- dynamic(transitions_/3).
	:- mode(transitions_(?state, ?state, ?integer), zero_or_more).

	:- private(solution_length_/1).
	:- dynamic(solution_length_/1).
	:- mode(solution_length_(?integer), zero_or_one).

	:- private(time_/1).
	:- dynamic(time_/1).
	:- mode(time_(-number), zero_or_one).

	:- public(time/1).
	:- mode(time(-number), zero_or_one).

	:- public(transitions/1).
	:- mode(transitions(-number), zero_or_one).

	:- public(branching/3).
	:- mode(branching(-integer, -float, -integer), zero_or_one).

	:- public(report/0).
	:- mode(report, zero_or_one).

	:- public(init/0).
	:- mode(init, one).

	:- public(stop/0).
	:- mode(stop, one).

	report :-
		solution_length_(Length),
		transitions(Number),
		Ratio is Length / Number,
		branching(Minimum, Average, Maximum),
		time(Time),
		write('solution length: '), write(Length), nl,
		write('state transitions (including previous solutions): '), write(Number), nl,
		write('ratio solution length / state transitions: '), write(Ratio), nl,
		write('minimum branching degree: '), write(Minimum), nl,
		write('average branching degree: '), write(Average), nl,
		write('maximum branching degree: '), write(Maximum), nl,
		write('time: '), write(Time), nl.

	report :-		% clean up for next solution
		retractall(time_(_)),
		retractall(solution_length_(_)),
		cpu_time(Start),
		asserta(time_(Start)),
		fail.

	transitions(Number) :-
		findall(N, transitions_(_, _, N), List),
		sum(List, Number).

	time(Time) :-
		cpu_time(End),
		retract(time_(Start)),
		Time is End - Start.

	branching(Minimum, Average, Maximum) :-
		findall(
			Length, 
			(transitions_(State1, _, _),
			 findall(State2, transitions_(State1, State2, _), States2),
			 length(States2, Length)),
			Lengths),
		min(Lengths, Minimum),
		max(Lengths, Maximum),
		sum(Lengths, Sum),
		length(Lengths, Length),
		Average is Sum / Length.

	init :-
		self(Self),
		event_registry::set_monitor(_, solve(_, _, _), _, Self),
		after_event_registry::set_monitor(_, next_state(_, _), _, Self),
		event_registry::set_monitor(_, solve(_, _, _, _), _, Self),
		after_event_registry::set_monitor(_, next_state(_, _, _), _, Self),
		retractall(transitions_(_, _, _)),
		retractall(time_(_)),
		retractall(solution_length_(_)),
		set_logtalk_flag(events, allow).	% solve/3-4 messages are sent from "user"

	stop :-
		set_logtalk_flag(events, deny),
		self(Self),
		before_event_registry::del_monitors(_, _, _, Self),
		after_event_registry::del_monitors(_, _, _, Self).

	before(_, solve(_, _, _), _) :-
		!,
		retractall(transitions_(_, _, _)),
		cpu_time(Start),
		retractall(time_(_)),
		asserta(time_(Start)).
	before(_, solve(_, _, _, _), _) :-
		!,
		retractall(transitions_(_, _, _)),
		cpu_time(Start),
		retractall(time_(_)),
		asserta(time_(Start)).

	after(_, next_state(S1, S2), _) :-
		!,
		(	retract(transitions_(S1, S2, N)) ->
			N2 is N + 1
		;	N2 is 1
		),
		assertz(transitions_(S1, S2, N2)).
	after(_, next_state(S1, S2, _), _) :-
		!,
		(	retract(transitions_(S1, S2, N)) ->
			N2 is N + 1
		;	N2 is 1
		),
		assertz(transitions_(S1, S2, N2)).
	after(_, solve(_, _, Solution), _) :-
		!,
		length(Solution, Length),
		retractall(solution_length_(_)),
		asserta(solution_length_(Length)).
	after(_, solve(_, _, Solution, _), _) :-
		!,
		length(Solution, Length),
		retractall(solution_length_(_)),
		asserta(solution_length_(Length)).

:- end_object.
