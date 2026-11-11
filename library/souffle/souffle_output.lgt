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


:- object(souffle_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-11,
		comment is 'Decoding of captured stdout (``.printsize`` lines) and per-relation tab-separated CSV file content into a collected result.'
	]).

	:- public(decode/8).
	:- mode(decode(+list(code), +list(code), +list(compound), +term, +compound, +list(pair), +list(atom), -compound), one_or_error).
	:- info(decode/8, [
		comment is 'Decodes captured stdout, stderr, per-relation CSV file content, and process metadata into a collected ``souffle_result(Relations, Sizes, Properties)`` term.',
		argnames is [
			'Codes', 'Diagnostics', 'RelationCodes', 'RawStatus', 'NormalizedStatus', 'Outputs',
			'Printsizes', 'Result'
		],
		exceptions is [
			'``Codes`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Codes`` is neither a partial list nor a list of character codes' - type_error(codes, 'Codes'),
			'``Diagnostics`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Diagnostics`` is neither a partial list nor a list of character codes' - type_error(codes, 'Diagnostics'),
			'``RelationCodes``, ``Outputs``, or ``Printsizes`` is a partial list or has a variable element' - instantiation_error,
			'``RelationCodes``, ``Outputs``, or ``Printsizes`` is neither a partial list nor a list' - type_error(list, 'RelationCodes'),
			'``RawStatus`` is not a ground term' - instantiation_error,
			'``NormalizedStatus`` is not a ground term' - instantiation_error,
			'``NormalizedStatus`` is not ``exit(0)``' - domain_error(souffle_process_status, 'NormalizedStatus'),
			'A requested relation in ``Outputs`` has no entry, or a ``missing`` entry, in ``RelationCodes``' - domain_error(souffle_output, 'Name'-missing),
			'A CSV row for a requested relation has a different number of columns than declared in ``Outputs``' - domain_error(souffle_output, column_count-('Row'-'Types')),
			'A ``number`` or ``unsigned`` column value is not a valid integer, or an ``unsigned`` value is negative' - domain_error(souffle_output, 'Type'-'Value'),
			'A ``float`` column value is not a valid number' - domain_error(souffle_output, float-'Value'),
			'A requested relation in ``Printsizes`` has no matching ``Name\\tCount`` line in ``Codes``' - domain_error(souffle_output, 'Name'-missing)
		]
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		check/3
	]).

	decode(Codes, Diagnostics, RelationCodes, RawStatus, NormalizedStatus, Outputs, Printsizes, Result) :-
		context(Context),
		check(ground, Codes-Diagnostics-RelationCodes-RawStatus-NormalizedStatus-Outputs-Printsizes, Context),
		check(codes, Codes, Context),
		check(codes, Diagnostics, Context),
		check(list, RelationCodes, Context),
		check(list, Outputs, Context),
		check(list, Printsizes, Context),
		validate_status(NormalizedStatus),
		decode_relations(Outputs, RelationCodes, Relations),
		split_lines(Codes, Lines),
		decode_sizes(Printsizes, Lines, Sizes),
		Result = souffle_result(Relations, Sizes, [
			raw_status(RawStatus), process_status(NormalizedStatus), diagnostics(Diagnostics)
		]).

	validate_status(exit(0)) :-
		!.
	validate_status(NormalizedStatus) :-
		domain_error(souffle_process_status, NormalizedStatus).

	% -----------------------------------------------------------------------
	% ``.output`` relation decoding (tab-separated CSV file content, no header)
	% -----------------------------------------------------------------------

	decode_relations([], _, []).
	decode_relations([Name-Types| Outputs], RelationCodes, [relation(Name, Types, Rows)| Relations]) :-
		(	memberchk(Name-codes(FileCodes), RelationCodes) ->
			decode_csv(FileCodes, Types, Rows)
		;	domain_error(souffle_output, Name-missing)
		),
		decode_relations(Outputs, RelationCodes, Relations).

	decode_csv([], _, []) :-
		!.
	decode_csv(Codes, Types, [Row| Rows]) :-
		split_line(Codes, LineCodes, Rest),
		length(Types, Count),
		split_columns(LineCodes, Count, ColumnCodesList),
		decode_row(ColumnCodesList, Types, Row),
		decode_csv(Rest, Types, Rows).

	split_line([], [], []) :-
		!.
	split_line([10| Rest], [], Rest) :-
		!.
	split_line([Code| Codes], [Code| LineCodes], Rest) :-
		split_line(Codes, LineCodes, Rest).

	% Splits on the first Count-1 tabs only; the last column keeps any remaining
	% text verbatim (a documented limitation for symbol values containing tabs).
	split_columns(Codes, 1, [Codes]) :-
		!.
	split_columns(Codes, Count, [Field| Fields]) :-
		(	append(Field, [9| Rest], Codes) ->
			Count1 is Count - 1,
			split_columns(Rest, Count1, Fields)
		;	Field = Codes, Fields = []
		).

	decode_row([], [], []) :-
		!.
	decode_row([Codes| ColumnCodesList], [Type| Types], [Value| Row]) :-
		!,
		decode_value(Type, Codes, Value),
		decode_row(ColumnCodesList, Types, Row).
	decode_row(ColumnCodesList, Types, _) :-
		domain_error(souffle_output, column_count-(ColumnCodesList-Types)).

	decode_value(number, Codes, Integer) :-
		!,
		(	catch(number_codes(Integer, Codes), _, fail),
			integer(Integer) ->
			true
		;	domain_error(souffle_output, number-Codes)
		).
	decode_value(unsigned, Codes, Integer) :-
		!,
		(	catch(number_codes(Integer, Codes), _, fail),
			integer(Integer),
			Integer >= 0 ->
			true
		;	domain_error(souffle_output, unsigned-Codes)
		).
	decode_value(float, Codes, Float) :-
		!,
		(	catch(number_codes(Number, Codes), _, fail) ->
			(	float(Number) -> Float = Number ; Float is Number * 1.0 )
		;	domain_error(souffle_output, float-Codes)
		).
	decode_value(symbol, Codes, Atom) :-
		!,
		atom_codes(Atom, Codes).

	% -----------------------------------------------------------------------
	% ``.printsize`` line decoding (``Name\tCount`` lines on captured stdout)
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

	decode_sizes([], _, []).
	decode_sizes([Name| Names], Lines, [size(Name, Count)| Sizes]) :-
		(	printsize_line(Name, Lines, Count) ->
			true
		;	domain_error(souffle_output, Name-missing)
		),
		decode_sizes(Names, Lines, Sizes).

	printsize_line(Name, Lines, Count) :-
		atom_codes(Name, NameCodes),
		member(Line, Lines),
		append(NameCodes, [9| CountCodes], Line),
		catch(number_codes(Count, CountCodes), _, fail),
		integer(Count),
		!.

:- end_object.
