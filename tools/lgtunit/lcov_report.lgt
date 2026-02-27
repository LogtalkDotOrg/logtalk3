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


:- initialization((
	% define a flag to allow the logtalk_tester script to pass the
	% option to suppress the test file and directory path prefix
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)]),
	% define a flag to allow overriding the directory where tests reports
	% are created (e.g., when running tests defined in a directory different
	% from the directory that contains the tests driver file)
	create_logtalk_flag(tests_report_directory, '', [type(atom), keep(true)])
)).


:- object(lcov_report).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-27,
		comment is 'Intercepts unit test execution messages and generates a ``lcov.info`` file with code coverage results.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(lcov_report))``.'
		]
	]).

	:- private(entity_file_/2).
	:- dynamic(entity_file_/2).

	:- private(predicate_coverage_/7).
	:- dynamic(predicate_coverage_/7).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		% ground the message term
		numbervars(Message, 0, _),
		message_hook(Message),
		% allow default processing of the messages
		fail.

	message_hook(tests_started) :-
		retractall(entity_file_(_, _)),
		retractall(predicate_coverage_(_, _, _, _, _, _, _)).

	message_hook(running_tests_from_object_file(_Object, File)) :-
		stream_property(_, alias(lcov_report)),
		!,
		logtalk::loaded_file_property(File, directory(_)).
	message_hook(running_tests_from_object_file(_Object, File)) :-
		( 	% bypass the compiler as the flag is only created after loading this file
			{current_logtalk_flag(tests_report_directory, Directory)}, Directory \== '' ->
			true
		;	logtalk::loaded_file_property(File, directory(Directory))
		),
		atom_concat(Directory, 'lcov.info', ReportFile),
		open(ReportFile, write, _, [alias(lcov_report)]).

	message_hook(entity_coverage_starts(Entity)) :-
		entity_file(Entity, File),
		assertz(entity_file_(Entity, File)).

	message_hook(entity_predicate_coverage(Entity, Predicate, Covered, Total, Percentage, Clauses)) :-
		entity_predicate_line(Predicate, Entity, Line),
		assertz(predicate_coverage_(Entity, Predicate, Line, Covered, Total, Percentage, Clauses)).

	message_hook(tests_ended) :-
		forall(
			entity_file_(Entity, File),
			write_entity(Entity, File)
		),
		close(lcov_report).

	write_entity(Entity, File) :-
		write(lcov_report, 'TN:'), nl(lcov_report),
		write(lcov_report, 'SF:'),
		write(lcov_report, File), nl(lcov_report),
		forall(
			predicate_coverage_(Entity, Predicate, Line, _Covered, _Total, _Percentage, _Clauses),
			( 	write(lcov_report, 'FN:'),
				write(lcov_report, Line),
				write(lcov_report, ','),
				write(lcov_report, Predicate), nl(lcov_report)
			)
		),
		forall(
			predicate_coverage_(Entity, Predicate, _Line, Covered, _Total, _Percentage, _Clauses),
			( 	write(lcov_report, 'FNDA:'),
				write(lcov_report, Covered),
				write(lcov_report, ','),
				write(lcov_report, Predicate), nl(lcov_report)
			)
		),
		count_functions(Entity, FunctionTotal, FunctionCovered),
		write(lcov_report, 'FNF:'), write(lcov_report, FunctionTotal), nl(lcov_report),
		write(lcov_report, 'FNH:'), write(lcov_report, FunctionCovered), nl(lcov_report),
		forall(
			predicate_coverage_(Entity, _Predicate, Line, Covered, _Total, _Percentage, _Clauses),
			( 	write(lcov_report, 'DA:'),
				write(lcov_report, Line),
				write(lcov_report, ','),
				write(lcov_report, Covered), nl(lcov_report)
			)
		),
		count_lines(Entity, LinesTotal, LinesCovered),
		write(lcov_report, 'LF:'), write(lcov_report, LinesTotal), nl(lcov_report),
		write(lcov_report, 'LH:'), write(lcov_report, LinesCovered), nl(lcov_report),
		write(lcov_report, 'end_of_record'), nl(lcov_report).

	count_functions(Entity, Total, Covered) :-
		findall(Predicate,
			predicate_coverage_(Entity, Predicate, _Line, _Hits, _Total, _Percentage, _Clauses),
			Predicates
		),
		list_length(Predicates, Total),
		findall(Predicate,
			( 	predicate_coverage_(Entity, Predicate, _Line, Hits, _Total, _Percentage, _Clauses),
				Hits > 0
			),
			CoveredPredicates
		),
		list_length(CoveredPredicates, Covered).

	count_lines(Entity, Total, Covered) :-
		findall(Hits,
			predicate_coverage_(Entity, _Predicate, _Line, Hits, _Total, _Percentage, _Clauses),
			HitsList
		),
		list_length(HitsList, Total),
		count_non_zero(HitsList, Covered).

	count_non_zero([], 0).
	count_non_zero([Head| Tail], Count) :-
		count_non_zero(Tail, TailCount),
		( 	Head > 0 ->
			Count is TailCount + 1
		;	Count = TailCount
		).

	list_length([], 0).
	list_length([_| Tail], Length) :-
		list_length(Tail, TailLength),
		Length is TailLength + 1.

	entity_file(Entity, File) :-
		( 	current_object(Entity) ->
			object_property(Entity, file(File0))
		;	current_category(Entity) ->
			category_property(Entity, file(File0))
		;	atom(Entity), current_protocol(Entity) ->
			protocol_property(Entity, file(File0))
		;	File0 = ''
		),
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		( 	atom_concat(Prefix, File, File0) ->
			true
		;	File = File0
		).

	entity_predicate_line(Other::Functor//Arity, Entity, Line) :-
		ExtendedArity is Arity + 2,
		entity_predicate_line(Other::Functor/ExtendedArity, Entity, Line).
	entity_predicate_line(Other::Functor/Arity, Entity, Line) :-
		( 	current_object(Entity) ->
			object_property(Entity, provides(Functor/Arity, Other, Properties))
		;	current_category(Entity) ->
			category_property(Entity, provides(Functor/Arity, Other, Properties))
		;	Properties = []
		),
		( 	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		).
	entity_predicate_line(Functor//Arity, Entity, Line) :-
		ExtendedArity is Arity + 2,
		entity_predicate_line(Functor/ExtendedArity, Entity, Line).
	entity_predicate_line(Functor/Arity, Entity, Line) :-
		( 	current_object(Entity) ->
			object_property(Entity, defines(Functor/Arity, Properties))
		;	current_category(Entity) ->
			category_property(Entity, defines(Functor/Arity, Properties))
		;	Properties = []
		),
		( 	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

:- end_object.
