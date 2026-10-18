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


:- object(sonar_report).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-18,
		comment is 'Intercepts unit test execution messages and generates a ``sonar.xml`` file with code coverage results using the SonarQube and SonarCloud Generic Test Coverage XML format.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(sonar_report))``.',
			'Analysis parameter' - 'Pass the generated report to the Sonar scanner using the ``sonar.coverageReportPaths`` analysis parameter.',
			'File paths' - 'Sonar requires file paths to be either absolute or relative to the project root. Use the ``suppress_path_prefix`` flag to strip the prefix of the source file paths when necessary.',
			'Lines to cover' - 'Each predicate is reported as a single line to cover, the line of its first clause. The line is reported as covered when at least one of the predicate clauses is covered. Predicates without line information and with lines outside the source file are not reported.',
			'Branches to cover' - 'Predicates with two or more clauses also report the number of clauses as the number of branches to cover and the number of covered clauses as the number of covered branches.',
			'Shared lines and files' - 'Entities defined in the same source file are reported under a single file element. Predicates that share the same line are reported as a single line to cover.'
		]
	]).

	:- private(predicate_coverage_/4).
	:- dynamic(predicate_coverage_/4).
	:- mode(predicate_coverage_(?atom, ?integer, ?integer, ?integer), zero_or_more).
	:- info(predicate_coverage_/4, [
		comment is 'Table of predicate coverage data.',
		argnames is ['File', 'Line', 'Covered', 'Total']
	]).

	:- uses(user, [
		sort/2
	]).

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
		retractall(predicate_coverage_(_, _, _, _)).

	message_hook(running_tests_from_object_file(_Object, File)) :-
		stream_property(_, alias(sonar)),
		% assume output stream already created, typically as a
		% consequence of using a parametric test object to test
		% multiple implementations of the same protocol
		!,
		logtalk::loaded_file_property(File, directory(_)).
	message_hook(running_tests_from_object_file(_Object, File)) :-
		(	% bypass the compiler as the flag is only created after loading this file
			{current_logtalk_flag(tests_report_directory, Directory)}, Directory \== '' ->
			true
		;	logtalk::loaded_file_property(File, directory(Directory))
		),
		atom_concat(Directory, 'sonar.xml', ReportFile),
		open(ReportFile, write, _, [alias(sonar)]).

	message_hook(entity_predicate_coverage(Entity, Predicate, Covered, Total, _Percentage, _Clauses)) :-
		entity_predicate_location(Predicate, Entity, File, Line),
		assertz(predicate_coverage_(File, Line, Covered, Total)).

	message_hook(tests_ended) :-
		write(sonar, '<?xml version="1.0" encoding="UTF-8"?>'), nl(sonar),
		write(sonar, '<coverage version="1">'), nl(sonar),
		findall(File, predicate_coverage_(File, _, _, _), Files0),
		sort(Files0, Files),
		forall(
			member(File, Files),
			write_file(File)
		),
		write(sonar, '</coverage>'), nl(sonar),
		close(sonar).

	% a file may contain several entities and the Sonar generic coverage
	% format expects a single file element per file with unique line numbers

	write_file(File) :-
		findall(
			Line-counts(Covered, Total),
			(	predicate_coverage_(File, Line, Covered, Total),
				% skip predicates without line information
				Line >= 1
			),
			Pairs0
		),
		(	Pairs0 == [] ->
			% nothing to report for this file
			true
		;	keysort(Pairs0, Pairs),
			merge_lines(Pairs, Lines),
			write(sonar, '  <file path="'),
			write_xml_attribute(File),
			write(sonar, '">'), nl(sonar),
			forall(
				member(LineNumber-counts(LineCovered, LineTotal), Lines),
				write_line(LineNumber, LineCovered, LineTotal)
			),
			write(sonar, '  </file>'), nl(sonar)
		).

	merge_lines([], []).
	merge_lines([Line-counts(Covered0, Total0)| Pairs], [Line-counts(Covered, Total)| Lines]) :-
		merge_same_line(Pairs, Line, Covered0, Total0, Covered, Total, Rest),
		merge_lines(Rest, Lines).

	merge_same_line([Line-counts(Covered1, Total1)| Pairs], Line, Covered0, Total0, Covered, Total, Rest) :-
		!,
		Covered2 is Covered0 + Covered1,
		Total2 is Total0 + Total1,
		merge_same_line(Pairs, Line, Covered2, Total2, Covered, Total, Rest).
	merge_same_line(Rest, _, Covered, Total, Covered, Total, Rest).

	write_line(Line, Covered, Total) :-
		write(sonar, '    <lineToCover lineNumber="'),
		write(sonar, Line),
		write(sonar, '" covered="'),
		(	Covered > 0 ->
			write(sonar, true)
		;	write(sonar, false)
		),
		(	Total > 1 ->
			% report each clause as a branch
			write(sonar, '" branchesToCover="'),
			write(sonar, Total),
			write(sonar, '" coveredBranches="'),
			write(sonar, Covered)
		;	true
		),
		write(sonar, '"/>'), nl(sonar).

	% XML auxiliary predicates

	write_xml_attribute(Text) :-
		atom_chars(Text, Chars),
		phrase(escape_xml_characters(Chars), EscapedChars),
		atom_chars(Escaped, EscapedChars),
		write(sonar, Escaped).

	escape_xml_characters([]) -->
		[].
	escape_xml_characters(['&'| Chars]) -->
		['&', a, m, p, ';'],
		!,
		escape_xml_characters(Chars).
	escape_xml_characters(['<'| Chars]) -->
		['&', l, t, ';'],
		!,
		escape_xml_characters(Chars).
	escape_xml_characters(['>'| Chars]) -->
		['&', g, t, ';'],
		!,
		escape_xml_characters(Chars).
	escape_xml_characters(['"'| Chars]) -->
		['&', q, u, o, t, ';'],
		!,
		escape_xml_characters(Chars).
	escape_xml_characters(['\''| Chars]) -->
		['&', a, p, o, s, ';'],
		!,
		escape_xml_characters(Chars).
	escape_xml_characters([Char| Chars]) -->
		[Char],
		escape_xml_characters(Chars).

	% other auxiliary predicates

	entity_file(Entity, File) :-
		(	current_object(Entity) ->
			object_property(Entity, file(File))
		;	current_category(Entity) ->
			category_property(Entity, file(File))
		;	atom(Entity), current_protocol(Entity) ->
			protocol_property(Entity, file(File))
		;	File = ''
		).

	entity_predicate_location(Other::Functor//Arity, Entity, File, Line) :-
		ExtendedArity is Arity + 2,
		entity_predicate_location(Other::Functor/ExtendedArity, Entity, File, Line).
	entity_predicate_location(Other::Functor/Arity, Entity, File, Line) :-
		(	current_object(Entity) ->
			object_property(Entity, provides(Functor/Arity, Other, Properties))
		;	current_category(Entity) ->
			category_property(Entity, provides(Functor/Arity, Other, Properties))
		;	Properties = []
		),
		predicate_source_location(Entity, Properties, File, Line).
	entity_predicate_location(Functor//Arity, Entity, File, Line) :-
		ExtendedArity is Arity + 2,
		entity_predicate_location(Functor/ExtendedArity, Entity, File, Line).
	entity_predicate_location(Functor/Arity, Entity, File, Line) :-
		(	current_object(Entity) ->
			object_property(Entity, defines(Functor/Arity, Properties))
		;	current_category(Entity) ->
			category_property(Entity, defines(Functor/Arity, Properties))
		;	Properties = []
		),
		predicate_source_location(Entity, Properties, File, Line).

	predicate_source_location(Entity, Properties, File, Line) :-
		(	member(include(File0), Properties) ->
			true
		;	entity_file(Entity, File0)
		),
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, File, File0) ->
			true
		;	File = File0
		),
		(	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

:- end_object.
