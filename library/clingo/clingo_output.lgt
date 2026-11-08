%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(clingo_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-08,
		comment is 'Whole-document Clingo JSON output decoding.'
	]).

	:- public(decode/6).
	:- mode(decode(+list(integer), +term, +compound, +list(integer), +list(compound), -compound), one_or_error).
	:- info(decode/6, [
		comment is 'Decodes CLI JSON and process metadata into a collected ``clingo_result(Status, Models, Properties)`` term.',
		argnames is ['Codes', 'RawStatus', 'NormalizedStatus', 'Diagnostics', 'SourceMap', 'Result'],
		exceptions is [
			'An input is insufficiently instantiated' - instantiation_error,
			'A code-list input is not a character-code list' - type_error(codes, 'Input'),
			'``SourceMap`` is not a list' - type_error(list, 'SourceMap'),
			'``Codes`` does not contain parsable JSON' - domain_error(json, codes('Codes')),
			'A JSON field has an invalid type or value' - domain_error(clingo_output, 'Field'-'Value'),
			'The JSON simultaneously reports a time limit and another interruption' - domain_error(clingo_output, interruption_flags),
			'The process status is fatal or inconsistent with the solver result' - domain_error(clingo_process_status, 'NormalizedStatus'),
			'A shown value is not a supported symbol' - domain_error(clingo_symbol, 'Codes'),
			'A numeric symbol cannot be represented as an integer' - representation_error(clingo_integer)
		]
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		member/2
	]).

	decode(Codes, RawStatus, NormalizedStatus, Diagnostics, SourceMap, Result) :-
		context(Context),
		check(ground, Codes-RawStatus-NormalizedStatus-Diagnostics-SourceMap, Context),
		check(codes, Codes, Context),
		check(codes, Diagnostics, Context),
		check(list, SourceMap, Context),
		json(list, dash, atom)::parse(codes(Codes), JSON),
		object_field(root, JSON),
		(	JSON = json(Pairs),
			member('Result'-Label, Pairs) ->
			(	result_status(Label, Status) ->
				true
			;	domain_error(clingo_output, 'Result'-Label)
			)
		;	domain_error(clingo_output, 'Result'-missing)
		),
		field('Call', JSON, [], Calls),
		list_field('Call', Calls),
		call_models(Calls, Models, []),
		field('Models', JSON, json([]), Summary),
		object_field('Models', Summary),
		field('More', Summary, unknown, More),
		(	completeness(More, Complete) ->
			true
		;	domain_error(clingo_output, 'More'-More)
		),
		field('Optimum', Summary, none, Optimum),
		(	optimality(Optimum, Optimality) ->
			true
		;	domain_error(clingo_output, 'Optimum'-Optimum)
		),
		field('Number', Summary, unknown, Count),
		(	Count == unknown ->
			true
		;	nonnegative_integer('Number', Count)
		),
		field('Costs', Summary, none, Costs),
		cost_field(Costs),
		interrupt_flag('TIME LIMIT', JSON, TimeLimit),
		interrupt_flag('INTERRUPTED', JSON, Interrupted),
		termination(TimeLimit, Interrupted, Termination),
		validate_status(NormalizedStatus, Status, Complete, Termination),
		(	consistent_result(Label, Status, Models, Complete, Optimality) ->
			true
		;	domain_error(clingo_output, 'Result'-Label)
		),
		field('Solver', JSON, unknown, Solver),
		(	atom(Solver) ->
			true
		;	domain_error(clingo_output, 'Solver'-Solver)
		),
		field('Time', JSON, none, Time),
		field('Stats', JSON, none, Statistics),
		Result = clingo_result(Status, Models, [
			solver(Solver), raw_status(RawStatus), process_status(NormalizedStatus),
			termination(Termination), enumeration_complete(Complete), optimality(Optimality),
			model_count(Count), costs(Costs), time(Time), statistics(Statistics),
			diagnostics(Diagnostics), source_map(SourceMap), raw_json(JSON)
		]).

	field(Key, json(Pairs), Default, Value) :-
		(	member(Key-Found, Pairs) ->
			Value = Found
		;	Value = Default
		).

	object_field(Key, Object) :-
		(	Object = json(Pairs),
			valid(list(pair(atom, term)), Pairs) ->
			true
		;	domain_error(clingo_output, Key-Object)
		).

	list_field(Key, List) :-
		(	valid(list, List) ->
			true
		;	domain_error(clingo_output, Key-List)
		).

	nonnegative_integer(Key, Integer) :-
		(	integer(Integer),
			Integer >= 0 ->
			true
		;	domain_error(clingo_output, Key-Integer)
		).

	call_models([], Models, Models).
	call_models([Call| Calls], Models, Tail) :-
		object_field('Call', Call),
		field('Witnesses', Call, [], Witnesses),
		list_field('Witnesses', Witnesses),
		witness_models(Witnesses, Models, Next),
		call_models(Calls, Next, Tail).

	witness_models([], Models, Models).
	witness_models([Witness| Witnesses], Models, Tail) :-
		object_field('Witness', Witness),
		(	Witness = json(Pairs),
			member('Value'-Values, Pairs) ->
			(	valid(list(atom), Values) ->
				true
			;	domain_error(clingo_output, 'Value'-Values)
			),
			parse_values(Values, Symbols),
			field('Costs', Witness, none, Costs),
			cost_field(Costs),
			Models = [model(Symbols, Costs, Values)| Next]
		;	Models = Next
		),
		witness_models(Witnesses, Next, Tail).

	parse_values([], []).
	parse_values([Value| Values], [Symbol| Symbols]) :-
		atom_codes(Value, Codes),
		clingo_symbols::parse(Codes, Symbol),
		parse_values(Values, Symbols).

	cost_field(Costs) :-
		(	Costs == none ->
			true
		;	valid(list(integer), Costs) ->
			true
		;	domain_error(clingo_output, 'Costs'-Costs)
		).

	interrupt_flag(Key, JSON, Flag) :-
		field(Key, JSON, 0, Value),
		(	Value == 0 ->
			Flag = false
		;	Value == 1 ->
			Flag = true
		;	domain_error(clingo_output, Key-Value)
		).

	termination(true, false, time_limit) :-
		!.
	termination(false, true, interrupted) :-
		!.
	termination(false, false, completed) :-
		!.
	termination(true, true, _) :-
		domain_error(clingo_output, interruption_flags).

	result_status('SATISFIABLE', satisfiable).
	result_status('OPTIMUM FOUND', satisfiable).
	result_status('UNSATISFIABLE', unsatisfiable).
	result_status('UNKNOWN', unknown).

	completeness(no, true).
	completeness(yes, false).
	completeness(unknown, unknown).

	optimality(yes, proven).
	optimality(unknown, unproven).
	optimality(no, unproven).
	optimality(none, not_applicable).

	consistent_result(Label, Status, Models, Complete, Optimality) :-
		(	Status == satisfiable ->
			true
		;	Models == []
		),
		(	Label == 'OPTIMUM FOUND' ->
			Optimality == proven,
			Complete == true
		;	Optimality \== proven
		),
		(	Status == unsatisfiable ->
			Complete \== false
		;	true
		).

	validate_status(NormalizedStatus, Status, Complete, Termination) :-
		(	status_agrees(NormalizedStatus, Status, Complete, Termination) ->
			true
		;	domain_error(clingo_process_status, NormalizedStatus)
		).

	status_agrees(exit(Code), Status, Complete, Termination) :-
		integer(Code),
		Interrupt is Code mod 2,
		Base is Code - Interrupt,
		(	Termination == completed ->
			Interrupt =:= 0
		;	Interrupt =:= 1
		),
		(	Status == satisfiable ->
			(	Base =:= 10 ->
				true
			;	Base =:= 30
			)
		;	(	Status == unsatisfiable ->
				Base =:= 20
			;	Base =:= 0
			)
		),
		(	Base >= 20 ->
			Complete \== false
		;	Complete \== true
		).
	status_agrees(unknown(_), _, _, _).
	status_agrees(killed(_), _, _, Termination) :-
		Termination \== completed.

:- end_object.
