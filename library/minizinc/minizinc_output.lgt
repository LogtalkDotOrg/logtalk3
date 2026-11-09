%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(minizinc_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Decoding of the MiniZinc ``--json-stream`` message sequence into a collected result.'
	]).

	:- public(decode/5).
	:- mode(decode(+list(integer), +term, +compound, +list(integer), -compound), one_or_error).
	:- info(decode/5, [
		comment is 'Decodes CLI JSON-stream output and process metadata into a collected ``minizinc_result(Status, Solutions, Properties)`` term.',
		argnames is ['Codes', 'RawStatus', 'NormalizedStatus', 'Diagnostics', 'Result'],
		exceptions is [
			'``Codes``, ``RawStatus``, ``NormalizedStatus``, or ``Diagnostics`` is a variable' - instantiation_error,
			'``Codes`` is neither a variable nor a list of character codes' - type_error(codes, 'Codes'),
			'``Diagnostics`` is neither a variable nor a list of character codes' - type_error(codes, 'Diagnostics'),
			'A ``--json-stream`` line is not valid JSON' - domain_error(minizinc_output, json-'Line'),
			'A JSON message field has an invalid type or value' - domain_error(minizinc_output, 'Field'-'Value'),
			'A JSON message is missing a required field' - domain_error(minizinc_output, 'Field'-missing),
			'``NormalizedStatus`` is not ``exit(0)`` and no JSON-stream message was produced' - domain_error(minizinc_process_status, 'NormalizedStatus')
		]
	]).

	:- uses(list, [
		memberchk/2, reverse/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	decode(Codes, RawStatus, NormalizedStatus, Diagnostics, Result) :-
		context(Context),
		check(ground, Codes-RawStatus-NormalizedStatus-Diagnostics, Context),
		check(codes, Codes, Context),
		check(codes, Diagnostics, Context),
		split_lines(Codes, Lines),
		parse_lines(Lines, Messages),
		(	Messages == [],
			NormalizedStatus \== exit(0) ->
			domain_error(minizinc_process_status, NormalizedStatus)
		;	true
		),
		classify_messages(Messages, state(Solutions, StatusInfo, Statistics, Warnings, Errors)),
		derive_status(Errors, StatusInfo, Solutions, Status, SolverStatusAtom, SolverTime),
		derive_optimality(SolverStatusAtom, Optimality),
		derive_enumeration_complete(SolverStatusAtom, EnumerationComplete),
		Result = minizinc_result(Status, Solutions, [
			solver_status(SolverStatusAtom), raw_status(RawStatus), process_status(NormalizedStatus),
			optimality(Optimality), enumeration_complete(EnumerationComplete),
			time(SolverTime), statistics(Statistics), warnings(Warnings), errors(Errors),
			diagnostics(Diagnostics), raw_json(Messages)
		]).

	% -----------------------------------------------------------------------
	% Line splitting and JSON parsing
	% -----------------------------------------------------------------------

	split_lines(Codes, Lines) :-
		split_on_newline(Codes, RawLines),
		exclude_empty(RawLines, Lines).

	split_on_newline([], [[]]) :-
		!.
	split_on_newline([13, 10| Rest], [[]| Lines]) :-
		!,
		split_on_newline(Rest, Lines).
	split_on_newline([10| Rest], [[]| Lines]) :-
		!,
		split_on_newline(Rest, Lines).
	split_on_newline([Code| Rest], [[Code| Line]| Lines]) :-
		split_on_newline(Rest, [Line| Lines]).

	exclude_empty([], []).
	exclude_empty([[]| Lines], Rest) :-
		!,
		exclude_empty(Lines, Rest).
	exclude_empty([Line| Lines], [Line| Rest]) :-
		exclude_empty(Lines, Rest).

	parse_lines([], []).
	parse_lines([Line| Lines], [JSON| JSONs]) :-
		catch(
			json(list, dash, atom)::parse(codes(Line), JSON),
			_,
			domain_error(minizinc_output, json-codes(Line))
		),
		parse_lines(Lines, JSONs).

	% -----------------------------------------------------------------------
	% Message classification
	% -----------------------------------------------------------------------

	classify_messages(Messages, Result) :-
		classify_messages(Messages, state([], none, [], [], []), state(RSolutions, Status, RStatistics, RWarnings, RErrors)),
		reverse(RSolutions, Solutions),
		reverse(RStatistics, Statistics),
		reverse(RWarnings, Warnings),
		reverse(RErrors, Errors),
		Result = state(Solutions, Status, Statistics, Warnings, Errors).

	classify_messages([], State, State).
	classify_messages([Message| Messages], State0, State) :-
		classify_message(Message, State0, State1),
		classify_messages(Messages, State1, State).

	classify_message(Message, state(Sols, Status, Stats, Warns, Errs), State) :-
		required_field(type, Message, Type),
		(	Type == solution ->
			decode_solution(Message, Solution),
			State = state([Solution| Sols], Status, Stats, Warns, Errs)
		;	Type == status ->
			decode_status(Message, StatusTerm),
			State = state(Sols, StatusTerm, Stats, Warns, Errs)
		;	Type == statistics ->
			required_field(statistics, Message, StatsObject),
			State = state(Sols, Status, [StatsObject| Stats], Warns, Errs)
		;	Type == warning ->
			decode_warning(Message, WarningTerm),
			State = state(Sols, Status, Stats, [WarningTerm| Warns], Errs)
		;	Type == error ->
			decode_error(Message, ErrorTerm),
			State = state(Sols, Status, Stats, Warns, [ErrorTerm| Errs])
		;	% comment, time, trace, profiling, paths, core, and checker messages
			% are retained only in the raw JSON message list, not specially decoded
			State = state(Sols, Status, Stats, Warns, Errs)
		).

	decode_solution(json(MessagePairs), solution(Values, Objective, Time)) :-
		required_field(output, json(MessagePairs), Output),
		(	Output = json(OutputPairs) ->
			true
		;	domain_error(minizinc_output, output-Output)
		),
		(	memberchk(json-Section, OutputPairs) ->
			(	Section = json(SectionPairs) ->
				true
			;	domain_error(minizinc_output, json-Section)
			),
			decode_pairs(SectionPairs, Values),
			(	memberchk('_objective'-ObjectiveRaw, SectionPairs) ->
				decode_value(ObjectiveRaw, Objective)
			;	Objective = none
			)
		;	Values = [], Objective = none
		),
		optional_field(time, json(MessagePairs), none, Time).

	decode_status(json(MessagePairs), status(StatusAtom, Time)) :-
		required_field(status, json(MessagePairs), Label),
		status_atom(Label, StatusAtom),
		optional_field(time, json(MessagePairs), none, Time).

	status_atom(Label, StatusAtom) :-
		(	Label == 'ALL_SOLUTIONS' -> StatusAtom = all_solutions
		;	Label == 'OPTIMAL_SOLUTION' -> StatusAtom = optimal_solution
		;	Label == 'UNSATISFIABLE' -> StatusAtom = unsatisfiable
		;	Label == 'UNBOUNDED' -> StatusAtom = unbounded
		;	Label == 'UNSAT_OR_UNBOUNDED' -> StatusAtom = unsat_or_unbounded
		;	Label == 'UNKNOWN' -> StatusAtom = unknown
		;	Label == 'ERROR' -> StatusAtom = error
		;	domain_error(minizinc_output, status-Label)
		).

	decode_warning(json(MessagePairs), warning(Message, Location)) :-
		required_field(message, json(MessagePairs), Message),
		optional_field(location, json(MessagePairs), none, LocationRaw),
		decode_location(LocationRaw, Location).

	decode_error(json(MessagePairs), error(What, Message, Location)) :-
		required_field(what, json(MessagePairs), What),
		required_field(message, json(MessagePairs), Message),
		optional_field(location, json(MessagePairs), none, LocationRaw),
		decode_location(LocationRaw, Location).

	decode_location(none, none) :-
		!.
	decode_location(json(Pairs), location(File, FirstLine, FirstColumn, LastLine, LastColumn)) :-
		!,
		optional_field(filename, json(Pairs), unknown, File),
		optional_field(firstLine, json(Pairs), unknown, FirstLine),
		optional_field(firstColumn, json(Pairs), unknown, FirstColumn),
		optional_field(lastLine, json(Pairs), unknown, LastLine),
		optional_field(lastColumn, json(Pairs), unknown, LastColumn).
	decode_location(Other, _) :-
		domain_error(minizinc_output, location-Other).

	% -----------------------------------------------------------------------
	% Result classification
	% -----------------------------------------------------------------------

	derive_status(Errors, StatusInfo, Solutions, Status, SolverStatusAtom, SolverTime) :-
		(	Errors \= [] ->
			Status = error, SolverStatusAtom = error, SolverTime = none
		;	StatusInfo = status(SolverStatusAtom, SolverTime) ->
			(	SolverStatusAtom == unsatisfiable -> Status = unsatisfiable
			;	SolverStatusAtom == unsat_or_unbounded -> Status = unsatisfiable
			;	SolverStatusAtom == unknown, Solutions == [] -> Status = unknown
			;	Status = satisfiable
			)
		;	% no status message was produced (e.g. a satisfy goal stopped after
			% the first solution without -a): infer satisfiability from whether
			% any solution was actually found
			SolverStatusAtom = none, SolverTime = none,
			(	Solutions == [] -> Status = unknown ; Status = satisfiable )
		).

	derive_optimality(SolverStatusAtom, proven) :-
		SolverStatusAtom == optimal_solution,
		!.
	derive_optimality(_, unproven).

	derive_enumeration_complete(SolverStatusAtom, true) :-
		(	SolverStatusAtom == all_solutions
		;	SolverStatusAtom == optimal_solution
		;	SolverStatusAtom == unsatisfiable
		;	SolverStatusAtom == unbounded
		),
		!.
	derive_enumeration_complete(SolverStatusAtom, false) :-
		SolverStatusAtom == unknown,
		!.
	derive_enumeration_complete(_, unknown).

	% -----------------------------------------------------------------------
	% JSON value decoding
	% -----------------------------------------------------------------------

	decode_pairs([], []).
	decode_pairs([Key-Value| Pairs], [Key-DecodedValue| DecodedPairs]) :-
		decode_value(Value, DecodedValue),
		decode_pairs(Pairs, DecodedPairs).

	decode_value(Number, Number) :-
		number(Number),
		!.
	decode_value('@'(true), true) :-
		!.
	decode_value('@'(false), false) :-
		!.
	decode_value('@'(null), null) :-
		!.
	decode_value(json(Pairs), Decoded) :-
		!,
		decode_object(Pairs, Decoded).
	decode_value(List, Decoded) :-
		valid(list, List),
		!,
		decode_list(List, Decoded).
	decode_value(Atom, Atom) :-
		atom(Atom),
		!.
	decode_value(Value, _) :-
		domain_error(minizinc_output, value-Value).

	decode_list([], []).
	decode_list([Value| Values], [Decoded| Decodeds]) :-
		decode_value(Value, Decoded),
		decode_list(Values, Decodeds).

	decode_object(Pairs, set(Elements)) :-
		memberchk(set-SetList, Pairs),
		!,
		valid(list, SetList),
		decode_set_elements(SetList, Elements).
	decode_object(Pairs, enum_constructor(Constructor, Value)) :-
		memberchk(c-Constructor, Pairs),
		memberchk(e-ValueRaw, Pairs),
		!,
		decode_value(ValueRaw, Value).
	decode_object(Pairs, enum(Case)) :-
		memberchk(e-Case, Pairs),
		!.
	decode_object(Pairs, object(Decoded)) :-
		decode_pairs(Pairs, Decoded).

	decode_set_elements([], []).
	decode_set_elements([Element| Elements], [Decoded| Decodeds]) :-
		(	valid(list, Element),
			Element = [LowRaw, HighRaw] ->
			decode_value(LowRaw, Low),
			decode_value(HighRaw, High),
			Decoded = range(Low, High)
		;	decode_value(Element, Decoded)
		),
		decode_set_elements(Elements, Decodeds).

	% -----------------------------------------------------------------------
	% Field access helpers
	% -----------------------------------------------------------------------

	required_field(Key, json(Pairs), Value) :-
		(	memberchk(Key-Value, Pairs) ->
			true
		;	domain_error(minizinc_output, Key-missing)
		).

	optional_field(Key, json(Pairs), Default, Value) :-
		(	memberchk(Key-Found, Pairs) ->
			Value = Found
		;	Value = Default
		).

:- end_object.
