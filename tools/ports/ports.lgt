%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(ports).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2014/08/21,
		comment is 'Box model port profiler.'
	]).

	:- public(profile/1).
	:- meta_predicate(profile(0)).
	:- mode(profile(@callable), zero_or_more).
	:- info(profile/1, [
		comment is 'Proves a goal while collecting port profiling information.',
		argnames is ['Goal']
	]).

	:- public(data/0).
	:- mode(data, one).
	:- info(data/0, [
		comment is 'Prints a table with all port profiling data.'
	]).

	:- public(data/1).
	:- mode(data(+entity_identifier), one).
	:- info(data/1, [
		comment is 'Prints a table with all port profiling data for the specified entity.',
		argnames is ['Entity']
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets all port profiling data.'
	]).

	:- public(reset/1).
	:- mode(reset(+entity_identifier), one).
	:- info(reset/1, [
		comment is 'Resets all port profiling data for the specified entity.',
		argnames is ['Entity']
	]).

	:- private(port_/5).
	:- dynamic(port_/5).

	% there can only be one debug handler provider loaded at the same time;
	% the Logtalk runtime uses the logtalk::debug_handler_provider/1 hook
	% predicate for detecting multiple instances of the handler and for
	% better error reporting
	:- multifile(logtalk::debug_handler_provider/1).
	:- if((current_logtalk_flag(prolog_dialect, qp); current_logtalk_flag(prolog_dialect, xsb))).
		% Qu-Prolog and XSB don't support static multifile predicates
		:- dynamic(logtalk::debug_handler_provider/1).
	:- endif.

	logtalk::debug_handler_provider(debugger).

	:- multifile(logtalk::debug_handler/2).
	:- if((current_logtalk_flag(prolog_dialect, qp); current_logtalk_flag(prolog_dialect, xsb))).
		% Qu-Prolog and XSB don't support static multifile predicates
		:- dynamic(logtalk::debug_handler/2).
	:- endif.

	logtalk::debug_handler(Event, ExCtx) :-
		debug_handler(Event, ExCtx).

	debug_handler(fact(Goal, _, _), ExCtx) :-
		logtalk::execution_context(ExCtx, Entity, _, _, _, _, _),
		functor(Entity, Functor, Arity),
		functor(Template, Functor, Arity),
		numbervars(Template, 0, _),
		port(Goal, fact, Template).
	debug_handler(rule(Goal, _, _), ExCtx) :-
		logtalk::execution_context(ExCtx, Entity, _, _, _, _, _),
		functor(Entity, Functor, Arity),
		functor(Template, Functor, Arity),
		numbervars(Template, 0, _),
		port(Goal, rule, Template).
	debug_handler(top_goal(Goal, TGoal), ExCtx) :-
		debug_handler(goal(Goal, TGoal), ExCtx).
	debug_handler(goal(Goal, TGoal), ExCtx) :-
		logtalk::execution_context(ExCtx, Entity, _, _, _, _, _),
		functor(Entity, Functor, Arity),
		functor(Template, Functor, Arity),
		numbervars(Template, 0, _),
		port(Goal, call, Template),
		(	catch(call_goal(TGoal, Deterministic), Error, exception(Goal, Error, Template)),
			(	Deterministic == true ->
				!,
				port(Goal, exit, Template)
			;	(	port(Goal, nd_exit, Template)
				;	port(Goal, redo, Template),
					fail
				)
			)
		;	port(Goal, fail, Template),
			fail
		).

	% inore calls to control constructs and ...
	port(_::_, _, _) :- !.
	port(::_, _, _) :- !.
	port(^^_, _, _) :- !.
	port(:_, _, _) :- !.
	port(':'(_,_), _, _) :- !.
	port(_<<_, _, _) :- !.
	port(_>>_, _, _) :- !.
	port({_}, _, _) :- !.
	% ... consider only calls to user predicates
	port(Goal, Port, Entity) :-
		functor(Goal, Functor, Arity),
		(	\+ entity_defines(Entity, Functor/Arity) ->
			true
		;	retract(port_(Port, Entity, Functor, Arity, OldCount)) ->
			NewCount is OldCount + 1,
			assertz(port_(Port, Entity, Functor, Arity, NewCount))
		;	assertz(port_(Port, Entity, Functor, Arity, 1))
		).

	entity_defines(Entity, Predicate) :-
		(	current_object(Entity) ->
			object_property(Entity, defines(Predicate, Properties))
		;	category_property(Entity, defines(Predicate, Properties))
		),
		\+ member(auxiliary, Properties).

	exception(Goal, Error, Entity) :-
		port(Goal, exception, Entity),
		throw(Error).

	data :-
		(	setof(
				Entity-Functor/Arity,
				Port^Count^port_(Port, Entity, Functor, Arity, Count),
				Predicates
			) ->
			write_data(Predicates)
		;	write_data([])
		).

	write_data(Predicates) :-
		table_rule(Rule),
		write(Rule), nl,
		table_label(Label),
		write(Label), nl,
		write(Rule), nl,
		(	Predicates == [] ->
			write('(no profiling data available)'), nl
		;	write_data_rows(Predicates)
		),
		write(Rule), nl.

	write_data_rows([]).
	write_data_rows([Entity-Functor/Arity| Predicates]) :-
		port(fact, Entity, Functor, Arity, Fact),
		port(rule, Entity, Functor, Arity, Rule),
		port(call, Entity, Functor, Arity, Call),
		port(exit, Entity, Functor, Arity, Exit),
		port(nd_exit, Entity, Functor, Arity, NDExit),
		port(fail, Entity, Functor, Arity, Fail),
		port(redo, Entity, Functor, Arity, Redo),
		functor(Entity, Name, Parameters),
		Template = Name/Parameters,
		entity_to_padded_atom(Template, TemplateAtom),
		predicate_indicator_to_padded_atom(Functor/Arity, PredicateAtom),
		write_list([TemplateAtom, PredicateAtom, Fact, Rule, Call, Exit, NDExit, Fail, Redo]), nl,
		write_data_rows(Predicates).

	write_list([]).
	write_list([Term| Terms]) :-
		write(Term),
		write_list(Terms).

	port(Port, Entity, Functor, Arity, CountAtom) :-
		(	port_(Port, Entity, Functor, Arity, Count) ->
			true
		;	Count = 0
		),
		integer_to_padded_atom(Count, CountAtom).

	entity_to_padded_atom(Functor/Arity, Atom) :-
		(	Arity =:= 0 ->
			Atom1 = Functor
		;	atom_concat(Functor, '/', Atom0),
			number_codes(Arity, Codes),
			atom_codes(ArityAtom, Codes),
			atom_concat(Atom0, ArityAtom, Atom1)
		),
		atom_length(Atom1, Length1),
		PadLength is 24 - Length1,
		pad_atom(PadLength, Pad),
		atom_concat(Atom1, Pad, Atom).

	predicate_indicator_to_padded_atom(Functor/Arity, Atom) :-
		atom_concat(Functor, '/', Atom0),
		number_codes(Arity, Codes),
		atom_codes(ArityAtom, Codes),
		atom_concat(Atom0, ArityAtom, Atom1),
		atom_length(Atom1, Length1),
		PadLength is 24 - Length1,
		pad_atom(PadLength, Pad),
		atom_concat(Atom1, Pad, Atom).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		atom_codes(Atom0, Codes),
		atom_length(Atom0, Length0),
		PadLength is 10 - Length0,
		pad_atom(PadLength, Pad),
		atom_concat(Pad, Atom0, Atom).

	pad_atom( 0, '').
	pad_atom( 1, ' ').
	pad_atom( 2, '  ').
	pad_atom( 3, '   ').
	pad_atom( 4, '    ').
	pad_atom( 5, '     ').
	pad_atom( 6, '      ').
	pad_atom( 7, '       ').
	pad_atom( 8, '        ').
	pad_atom( 9, '         ').
	pad_atom( 9, '         ').
	pad_atom(10, '          ').
	pad_atom(11, '           ').
	pad_atom(12, '            ').
	pad_atom(13, '             ').
	pad_atom(14, '              ').
	pad_atom(15, '               ').
	pad_atom(16, '                ').
	pad_atom(17, '                 ').
	pad_atom(18, '                  ').
	pad_atom(19, '                   ').
	pad_atom(20, '                    ').
	pad_atom(21, '                     ').
	pad_atom(22, '                      ').
	pad_atom(23, '                       ').

	table_rule('----------------------------------------------------------------------------------------------------------------------').

	table_label('Entity                  Predicate                     Fact      Rule      Call      Exit     *Exit      Fail      Redo').

	reset :-
		retractall(port_(_, _, _, _, _)).

	data(Entity) :-
		(	setof(
				Entity-Functor/Arity,
				Port^Count^port_(Port, Entity, Functor, Arity, Count),
				Predicates
			) ->
			write_data(Predicates)
		;	write_data([])
		).

	reset(Entity) :-
		retractall(port_(_, Entity, _, _, _)).

	:- if((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == b; Dialect == qp; Dialect == swi; Dialect == yap)
	)).

		call_goal(TGoal, Deterministic) :-
			{setup_call_cleanup(true, TGoal, Deterministic = true)}.

	:- elif((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == cx; Dialect == sicstus; Dialect == xsb)
	)).

		call_goal(TGoal, Deterministic) :-
			{call_cleanup(TGoal, Deterministic = true)}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		call_goal(TGoal, Deterministic) :-
			{call_det(TGoal, Deterministic0)},
			(	Deterministic0 == true ->
				Deterministic = Deterministic0
			;	true
			).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		call_goal(TGoal, Deterministic) :-
			{sepia_kernel:get_cut(Before),
			 call(TGoal),
			 sepia_kernel:get_cut(After)},
			(	Before == After ->
				!,
				Deterministic = true
			;	true
			).

	:- else.

		call_goal(TGoal, _) :-
			{TGoal}.

	:- endif.

	% auxiliary predicates; we could use the Logtalk standard library but
	% we prefer to make this object self-contained given its purpose

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.
