%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(ports_profiler).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2020-05-25,
		comment is 'Predicate execution box model port profiler.'
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

	:- public(port/5).
	:- mode(port(?atom, ?entity_identifier, ?atom, ?integer, ?integer), zero_or_more).
	:- info(port/5, [
		comment is 'Enumerates, by backtracking, all collected port profiling data.',
		argnames is ['Port', 'Entity', 'Functor', 'Arity', 'Count']
	]).

	:- private(port_/5).
	:- dynamic(port_/5).
	:- mode(port_(?atom, ?entity_identifier, ?atom, ?integer, ?integer), zero_or_more).
	:- info(port_/5, [
		comment is 'Internal table of collected port profiling data.',
		argnames is ['Port', 'Entity', 'Functor', 'Arity', 'Count']
	]).

	% there can only be one debug handler provider loaded at the same time;
	% the Logtalk runtime uses the logtalk::debug_handler_provider/1 hook
	% predicate for detecting multiple instances of the handler and for
	% better error reporting
	:- multifile(logtalk::debug_handler_provider/1).
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		% Qu-Prolog does not support static multifile predicates
		:- dynamic(logtalk::debug_handler_provider/1).
	:- endif.

	logtalk::debug_handler_provider(ports_profiler).

	:- multifile(logtalk::debug_handler/2).
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		% Qu-Prolog does not support static multifile predicates
		:- dynamic(logtalk::debug_handler/2).
	:- endif.

	logtalk::debug_handler(Event, ExCtx) :-
		debug_handler(Event, ExCtx).

	debug_handler(fact(_, Goal, _, _, _), ExCtx) :-
		logtalk::execution_context(ExCtx, Entity, _, _, _, _, _),
		ground_entity_identifier(Entity, GroundEntity),
		port(Goal, fact, GroundEntity).
	debug_handler(rule(_, Goal, _, _, _), ExCtx) :-
		logtalk::execution_context(ExCtx, Entity, _, _, _, _, _),
		ground_entity_identifier(Entity, GroundEntity),
		port(Goal, rule, GroundEntity).
	debug_handler(top_goal(Goal, TGoal), ExCtx) :-
		debug_handler(goal(Goal, TGoal), ExCtx).
	debug_handler(goal(Goal, TGoal), ExCtx) :-
		logtalk::execution_context(ExCtx, Entity, _, _, _, _, _),
		ground_entity_identifier(Entity, GroundEntity),
		port(Goal, call, GroundEntity),
		(	catch(call_goal(TGoal, Deterministic), Error, exception(Goal, Error, GroundEntity)),
			(	Deterministic == true ->
				!,
				port(Goal, exit, GroundEntity)
			;	(	port(Goal, nd_exit, GroundEntity)
				;	port(Goal, redo, GroundEntity),
					fail
				)
			)
		;	port(Goal, fail, GroundEntity),
			fail
		).

	ground_entity_identifier(Entity, GroundEntity) :-
		(	atom(Entity) ->
			GroundEntity = Entity
		;	functor(Entity, Functor, Arity),
			functor(GroundEntity, Functor, Arity),
			numbervars(GroundEntity, 0, _)
		).

	% ignore calls to control constructs and ...
	port(_::_, _, _) :- !.
	port(::_, _, _) :- !.
	port(^^_, _, _) :- !.
	port(':'(_,_), _, _) :- !.
	port(_<<_, _, _) :- !.
	port(_>>_, _, _) :- !.
	port({_}, _, _) :- !.
	% ... consider only calls to user-defined predicates
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
		% only consider user-defined predicates
		\+ member(auxiliary, Properties).

	exception(Goal, Error, Entity) :-
		port(Goal, exception, Entity),
		throw(Error).

	% main predicates

	data :-
		(	port_(_, _, _, _, _) ->
			setof(
				Entity-Functor/Arity,
				Port^Count^port_(Port, Entity, Functor, Arity, Count),
				Predicates
			),
			write_data(Predicates, _)
		;	% no profiling data collected so far
			write_data([], _)
		).

	reset :-
		retractall(port_(_, _, _, _, _)).

	data(Entity) :-
		entity_spec_to_template(Entity, EntityTemplate),
		(	\+ \+ port_(_, EntityTemplate, _, _, _) ->
			setof(
				EntityTemplate-Functor/Arity,
				Port^Count^port_(Port, EntityTemplate, Functor, Arity, Count),
				Predicates
			),
			write_data_entity(Predicates, Entity)
		;	% no profiling data collected so far for this entity
			write_data_entity([], _)
		).

	reset(Entity) :-
		entity_spec_to_template(Entity, EntityTemplate),
		retractall(port_(_, EntityTemplate, _, _, _)).

	port(Port, Entity, Functor, Arity, Count) :-
		port_(Port, Entity, Functor, Arity, Count).

	% auxiliary predicates

	entity_spec_to_template(Entity, EntityTemplate) :-
		nonvar(Entity),
		(	atom(Entity) ->
			EntityTemplate = Entity
		;	Entity = Name/Parameters ->
			functor(EntityTemplate, Name, Parameters)
		;	functor(Entity, Name, Parameters),
			functor(EntityTemplate, Name, Parameters)
		).

	write_data(Predicates, Entity) :-
		maximum_width_entity(Predicates, MaximumWidthEntity),
		maximum_width_predicate(Predicates, MaximumWidthPredicate),
		maximum_width_result(Entity, MaximumWidthCount),
		table_ruler(MaximumWidthEntity, MaximumWidthPredicate, MaximumWidthCount, Ruler),
		write(Ruler), nl,
		write_table_label(MaximumWidthEntity, MaximumWidthPredicate, MaximumWidthCount),
		write(Ruler), nl,
		(	Predicates == [] ->
			write('(no profiling data available)'), nl
		;	write_data_rows(Predicates, MaximumWidthEntity, MaximumWidthPredicate, MaximumWidthCount)
		),
		write(Ruler), nl.

	write_data_entity(Predicates, Entity) :-
		maximum_width_predicate(Predicates, MaximumWidthPredicate),
		maximum_width_result(Entity, MaximumWidthCount),
		table_ruler(MaximumWidthPredicate, MaximumWidthCount, Ruler),
		write(Ruler), nl,
		write_table_label(MaximumWidthPredicate, MaximumWidthCount),
		write(Ruler), nl,
		(	Predicates == [] ->
			write('(no profiling data available)'), nl
		;	write_data_rows(Predicates, MaximumWidthPredicate, MaximumWidthCount)
		),
		write(Ruler), nl.

	maximum_width_entity(Predicates, MaximumWidthEntity) :-
		maximum_width_entity(Predicates, 0, MaximumWidthEntity).

	maximum_width_entity([], Max0, Max) :-
		% 6 characters in the "Entity" label
		Max is max(Max0, 6).
	maximum_width_entity([Entity-_| Predicates], Max0, Max) :-
		(	atom(Entity) ->
			atom_length(Entity, Length),
			Max1 is max(Max0, Length)
		;	functor(Entity, Name, Parameters),
			atom_length(Name, NameLength),
			number_codes(Parameters, Codes),
			atom_codes(ParametersAtom, Codes),
			atom_length(ParametersAtom, ParametersLength),
			Max1 is max(Max0, NameLength+1+ParametersLength)
		),
		maximum_width_entity(Predicates, Max1, Max).

	maximum_width_predicate(Predicates, MaximumWidthEntity) :-
		maximum_width_predicate(Predicates, 0, MaximumWidthEntity).

	maximum_width_predicate([], Max0, Max) :-
		% 9 characters in the "Predicate" label
		Max is max(Max0, 9).
	maximum_width_predicate([_-Functor/Arity| Predicates], Max0, Max) :-
		atom_length(Functor, FunctorLength),
		number_codes(Arity, Codes),
		atom_codes(ArityAtom, Codes),
		atom_length(ArityAtom, ArityLength),
		Max1 is max(Max0, FunctorLength+1+ArityLength),
		maximum_width_predicate(Predicates, Max1, Max).

	maximum_width_result(Entity, MaximumWidthCount) :-
		(	var(Entity),
			setof(Count, count(Count), Counts) ->
			true
		;	numbervars(Entity, 0, _),
			setof(Count, entity_count(Entity, Count), Counts) ->
			true
		;	Counts = [9999]
		),
		reverse(Counts, [MaxCount| _]),
		(	MaxCount =< 9999 ->
			% port labels have a maximum length of 4
			MaximumWidthCount = 4
		;	number_codes(MaxCount, Codes),
			atom_codes(Atom, Codes),
			atom_length(Atom, MaximumWidthCount)
		).

	count(Count) :-
		port_(_, _, _, _, Count).

	entity_count(Entity, Count) :-
		port_(_, Entity, _, _, Count).

	write_table_label(MaximumWidthEntity, MaximumWidthPredicate, MaximumWidthCount) :-
		atom_to_right_padded_atom('Entity', MaximumWidthEntity, Entity),
		atom_to_right_padded_atom('Predicate', MaximumWidthPredicate, Predicate),
		atom_to_left_padded_atom(' Fact', MaximumWidthCount, Fact),
		atom_to_left_padded_atom(' Rule', MaximumWidthCount, Rule),
		atom_to_left_padded_atom(' Call', MaximumWidthCount, Call),
		atom_to_left_padded_atom(' Exit', MaximumWidthCount, Exit),
		atom_to_left_padded_atom('*Exit', MaximumWidthCount, NDExit),
		atom_to_left_padded_atom(' Fail', MaximumWidthCount, Fail),
		atom_to_left_padded_atom(' Redo', MaximumWidthCount, Redo),
		atom_to_left_padded_atom('Error', MaximumWidthCount, Error),
		write_list([Entity, Predicate, Fact, Rule, Call, Exit, NDExit, Fail, Redo, Error]), nl.

	write_table_label(MaximumWidthPredicate, MaximumWidthCount) :-
		atom_to_right_padded_atom('Predicate', MaximumWidthPredicate, Predicate),
		atom_to_left_padded_atom(' Fact', MaximumWidthCount, Fact),
		atom_to_left_padded_atom(' Rule', MaximumWidthCount, Rule),
		atom_to_left_padded_atom(' Call', MaximumWidthCount, Call),
		atom_to_left_padded_atom(' Exit', MaximumWidthCount, Exit),
		atom_to_left_padded_atom('*Exit', MaximumWidthCount, NDExit),
		atom_to_left_padded_atom(' Fail', MaximumWidthCount, Fail),
		atom_to_left_padded_atom(' Redo', MaximumWidthCount, Redo),
		atom_to_left_padded_atom('Error', MaximumWidthCount, Error),
		write_list([Predicate, Fact, Rule, Call, Exit, NDExit, Fail, Redo, Error]), nl.

	write_data_rows([], _, _, _).
	write_data_rows([Entity-Functor/Arity| Predicates], MaximumWidthEntity, MaximumWidthPredicate, MaximumWidthCount) :-
		port(fact, Entity, Functor, Arity, MaximumWidthCount, FactAtom),
		port(rule, Entity, Functor, Arity, MaximumWidthCount, RuleAtom),
		port(call, Entity, Functor, Arity, MaximumWidthCount, CallAtom),
		port(exit, Entity, Functor, Arity, MaximumWidthCount, ExitAtom),
		port(nd_exit, Entity, Functor, Arity, MaximumWidthCount, NDExitAtom),
		port(fail, Entity, Functor, Arity, MaximumWidthCount, FailAtom),
		port(redo, Entity, Functor, Arity, MaximumWidthCount, RedoAtom),
		port(exception, Entity, Functor, Arity, MaximumWidthCount, ExceptionAtom),
		functor(Entity, Name, Parameters),
		Template = Name/Parameters,
		entity_to_padded_atom(Template, MaximumWidthEntity, TemplateAtom),
		predicate_to_padded_atom(Entity, Functor/Arity, MaximumWidthPredicate, PredicateAtom),
		write_list([TemplateAtom, PredicateAtom, FactAtom, RuleAtom, CallAtom, ExitAtom, NDExitAtom, FailAtom, RedoAtom, ExceptionAtom]), nl,
		write_data_rows(Predicates, MaximumWidthEntity, MaximumWidthPredicate, MaximumWidthCount).

	write_data_rows([], _, _).
	write_data_rows([Entity-Functor/Arity| Predicates], MaximumWidthPredicate, MaximumWidthCount) :-
		port(fact, Entity, Functor, Arity, MaximumWidthCount, FactAtom),
		port(rule, Entity, Functor, Arity, MaximumWidthCount, RuleAtom),
		port(call, Entity, Functor, Arity, MaximumWidthCount, CallAtom),
		port(exit, Entity, Functor, Arity, MaximumWidthCount, ExitAtom),
		port(nd_exit, Entity, Functor, Arity, MaximumWidthCount, NDExitAtom),
		port(fail, Entity, Functor, Arity, MaximumWidthCount, FailAtom),
		port(redo, Entity, Functor, Arity, MaximumWidthCount, RedoAtom),
		port(exception, Entity, Functor, Arity, MaximumWidthCount, ExceptionAtom),
		predicate_to_padded_atom(Entity, Functor/Arity, MaximumWidthPredicate, PredicateAtom),
		write_list([PredicateAtom, FactAtom, RuleAtom, CallAtom, ExitAtom, NDExitAtom, FailAtom, RedoAtom, ExceptionAtom]), nl,
		write_data_rows(Predicates, MaximumWidthPredicate, MaximumWidthCount).

	write_list([]).
	write_list([Atom| Atoms]) :-
		write(Atom),
		write_list(Atoms).

	port(Port, Entity, Functor, Arity, MaximumWidth, CountAtom) :-
		(	port_(Port, Entity, Functor, Arity, Count) ->
			true
		;	Count = 0
		),
		integer_to_padded_atom(Count, MaximumWidth, CountAtom).

	atom_to_right_padded_atom(Atom, MaximumWidth, PaddedAtom) :-
		atom_length(Atom, Length),
		PadLength is MaximumWidth + 2 - Length,
		generate_atom(PadLength, ' ', Pad),
		atom_concat(Atom, Pad, PaddedAtom).

	atom_to_left_padded_atom(Atom, MaximumWidth, PaddedAtom) :-
		atom_length(Atom, Length),
		PadLength is MaximumWidth + 2 - Length,
		generate_atom(PadLength, ' ', Pad),
		atom_concat(Pad, Atom, PaddedAtom).

	entity_to_padded_atom(Functor/Arity, MaximumWidth, Atom) :-
		(	Arity =:= 0 ->
			Atom1 = Functor
		;	atom_concat(Functor, '/', Atom0),
			number_codes(Arity, Codes),
			atom_codes(ArityAtom, Codes),
			atom_concat(Atom0, ArityAtom, Atom1)
		),
		atom_length(Atom1, Length1),
		PadLength is MaximumWidth + 2 - Length1,
		generate_atom(PadLength, ' ', Pad),
		atom_concat(Atom1, Pad, Atom).

	predicate_to_padded_atom(Entity, Functor/Arity0, MaximumWidth, Atom) :-
		(	current_object(Entity),
			object_property(Entity, defines(Functor/Arity0, Properties)),
			member(non_terminal(_//Arity), Properties) ->
			atom_concat(Functor, '//', Atom0)
		;	current_category(Entity),
			category_property(Entity, defines(Functor/Arity0, Properties)),
			member(non_terminal(_//Arity), Properties) ->
			atom_concat(Functor, '//', Atom0)
		;	Arity = Arity0,
			atom_concat(Functor, '/', Atom0)
		),
		number_codes(Arity, Codes),
		atom_codes(ArityAtom, Codes),
		atom_concat(Atom0, ArityAtom, Atom1),
		atom_length(Atom1, Length1),
		PadLength is MaximumWidth + 2 - Length1,
		generate_atom(PadLength, ' ', Pad),
		atom_concat(Atom1, Pad, Atom).

	integer_to_padded_atom(Integer, MaximumWidth, Atom) :-
		number_codes(Integer, Codes),
		atom_codes(Atom0, Codes),
		atom_length(Atom0, Length0),
		PadLength is MaximumWidth + 2 - Length0,
		generate_atom(PadLength, ' ', Pad),
		atom_concat(Pad, Atom0, Atom).

	table_ruler(MaximumWidthEntity, MaximumWidthPredicate, MaximumWidthCount, Ruler) :-
		Length is MaximumWidthEntity + 2 + MaximumWidthPredicate + 2 + 8*MaximumWidthCount + 2*8,
		generate_atom(Length, '-', Ruler).

	table_ruler(MaximumWidthPredicate, MaximumWidthCount, Ruler) :-
		Length is MaximumWidthPredicate + 2 + 8*MaximumWidthCount + 2*8,
		generate_atom(Length, '-', Ruler).

	generate_atom(Length, Character, Atom) :-
		generate_atom(Length, Character, '', Atom).

	generate_atom(0, _, Atom, Atom) :-
		!.
	generate_atom(Length0, Character, Atom0, Atom) :-
		atom_concat(Character, Atom0, Atom1),
		Length1 is Length0 - 1,
		generate_atom(Length1, Character, Atom1, Atom).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == b; Dialect == qp; Dialect == swi; Dialect == yap)
	)).

		call_goal(TGoal, Deterministic) :-
			{setup_call_cleanup(true, TGoal, Deterministic = true)}.

	:- elif((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == cx; Dialect == ji; Dialect == sicstus; Dialect == xsb)
	)).

		call_goal(TGoal, Deterministic) :-
			{call_cleanup(TGoal, Deterministic = true)}.

	:- elif((	current_logtalk_flag(prolog_dialect, Dialect),
				(Dialect == ciao; Dialect == gnu)
	)).

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

	% list auxiliary predicates; we could use the Logtalk standard library
	% but we prefer to make this object self-contained given its purpose

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

	reverse(List, Reversed) :-
		reverse(List, [], Reversed).

	reverse([], Reversed, Reversed).
	reverse([Head| Tail], List, Reversed) :-
		reverse(Tail, [Head| List], Reversed).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, swi)).
	% add dummy meta_predicate/1 directive to avoid cluttering the make/0 analysis report
	:- meta_predicate(':'(user,'$ports#0.call_goal#2'(*,*,*))).
:- endif.
