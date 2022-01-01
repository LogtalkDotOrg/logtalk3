%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
	% option to supress the test file and directory path prefix
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)]),
	% define a flag to allow overriding the directory where tests reports
	% are created (e.g. when running tests defined in a directory different
	% from the directory that contains the tests driver file)
	create_logtalk_flag(tests_report_directory, '', [type(atom), keep(true)])
)).


:- object(coverage_report).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2021-04-10,
		comment is 'Intercepts unit test execution messages and generates a ``coverage_report.xml`` file with a test suite code coverage results.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(coverage_report))``.'
		]
	]).

	% the timestamp message is printed before the message that prints
	% the testsuite file that we require to create the XML report
	:- private(timestamp_/6).
	:- dynamic(timestamp_/6).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		% ground the message term
		numbervars(Message, 0, _),
		message_hook(Message),
		% allow default processing of the messages
		fail.

	% timestamp
	message_hook(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		!,
		retractall(timestamp_(_, _, _, _, _, _)),
		assertz(timestamp_(Year, Month, Day, Hours, Minutes, Seconds)).

	% start
	message_hook(running_tests_from_object_file(_, _)) :-
		stream_property(_, alias(coverage_report)),
		% assume output stream already created, typically as a
		% consequence of using a parametric test object to test
		% multiple implementations of the same protocol
		!.
	message_hook(running_tests_from_object_file(Object, File)) :-
		(	% bypass the compiler as the flag is only created after loading this file
			{current_logtalk_flag(tests_report_directory, Directory)}, Directory \== '' ->
			true
		;	logtalk::loaded_file_property(File, directory(Directory))
		),
		atom_concat(Directory, 'coverage_report.xml', ReportFile),
		open(ReportFile, write, _, [alias(coverage_report)]),
		write(coverage_report, '<?xml version="1.0" encoding="UTF-8"?>'), nl(coverage_report),
		write(coverage_report, '<!DOCTYPE cover SYSTEM "coverage_report.dtd">'), nl(coverage_report),
		write(coverage_report, '<?xml-stylesheet type="text/xsl" href="coverage_report.xsl"?>'), nl(coverage_report),
		write_xml_open_tag(cover),
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, Suffix, File) ->
			write_xml_element(testsuite, Suffix)
		;	write_xml_element(testsuite, File)
		),
		copy_term(Object, Copy),
		numbervars(Copy, 0, _),
		write_xml_element(object, Copy),
		timestamp_(Year, Month, Day, Hours, Minutes, Seconds),
		date_time_to_timestamp(Year, Month, Day, Hours, Minutes, Seconds, TimeStamp),
		write_xml_element(timestamp, TimeStamp),
		write_xml_open_tag(entities).

	% entity start
	message_hook(entity_coverage_starts(Entity)) :-
		write_xml_open_tag(entity),
		write_xml_element(name, Entity),
		write_xml_open_tag(predicates).

	% predicate statistics
	message_hook(entity_predicate_coverage(Entity, Predicate, Covered, Total, Percentage, Clauses)) :-
		write_xml_open_tag(predicate),
		write_xml_element(name, Predicate),
		write_xml_element(clauses, Clauses),
		write_xml_element(covered, Covered),
		write_xml_element(total, Total),
		write_xml_element(percentage, Percentage),
		entity_predicate_line(Predicate, Entity, Line),
		write_xml_element(line, Line),
		write_xml_close_tag(predicate).

	% entity statistics
	message_hook(entity_coverage(Entity, Covered, Total, Percentage)) :-
		write_xml_close_tag(predicates),
		write_xml_element(covered, Covered),
		write_xml_element(total, Total),
		write_xml_element(percentage, Percentage),
		entity_file(Entity, File, Line),
		write_xml_element(file, File),
		write_xml_element(line, Line).

	% entity end
	message_hook(entity_coverage_ends(_Entity)) :-
		write_xml_close_tag(entity).

	message_hook(declared_entities_and_clause_numbers(_TotalEntities, _TotalClauses)) :-
		write_xml_close_tag(entities).

	message_hook(no_code_coverage_information_collected) :-
		write_xml_close_tag(entities),
		write_xml_element(entities_covered, 0),
		write_xml_element(entities_total, '?'),
		write_xml_element(entities_percentage, 0.0),
		write_xml_element(clauses_covered, 0),
		write_xml_element(clauses_total, '?'),
		write_xml_element(clauses_percentage, 0.0).

	% global statistics
	message_hook(covered_entities_numbers(Covered, Total, Percentage)) :-
		write_xml_element(entities_covered, Covered),
		write_xml_element(entities_total, Total),
		write_xml_element(entities_percentage, Percentage).

	message_hook(covered_clause_numbers(Covered, Total, Percentage)) :-
		write_xml_element(clauses_covered, Covered),
		write_xml_element(clauses_total, Total),
		write_xml_element(clauses_percentage, Percentage).

	% stop
	message_hook(tests_ended) :-
		write_xml_close_tag(cover),
		close(coverage_report).

	% date and time auxiliary predicates

	date_time_to_timestamp(Year, Month, Day, Hours, Minutes, Seconds, TimeStamp) :-
		integers_to_atoms([Year,Month,Day,Hours,Minutes,Seconds], [AYear,AMonth0,ADay0,AHours0,AMinutes0,ASeconds0]),
		pad_single_char_atoms([AMonth0,ADay0,AHours0,AMinutes0,ASeconds0], [AMonth,ADay,AHours,AMinutes,ASeconds]),
		concatenate_atoms([AYear,'-',AMonth,'-',ADay,'T',AHours,':',AMinutes,':',ASeconds], '', TimeStamp).

	integers_to_atoms([], []).
	integers_to_atoms([Integer| Integers], [Atom| Atoms]) :-
		number_codes(Integer, Codes),
		atom_codes(Atom, Codes),
		integers_to_atoms(Integers, Atoms).

	pad_single_char_atoms([], []).
	pad_single_char_atoms([Atom| Atoms], [PaddedAtom| PaddedAtoms]) :-
		(	atom_length(Atom, 1) ->
			atom_concat('0', Atom, PaddedAtom)
		;	PaddedAtom = Atom
		),
		pad_single_char_atoms(Atoms, PaddedAtoms).

	concatenate_atoms([], Concat, Concat).
	concatenate_atoms([Atom| Atoms], Concat0, Concat) :-
		atom_concat(Concat0, Atom, Concat1),
		concatenate_atoms(Atoms, Concat1, Concat).

	% XML auxiliary predicates

	write_xml_open_tag(Tag) :-
		write(coverage_report, '<'),
		write(coverage_report, Tag),
		write(coverage_report, '>'), nl(coverage_report).

	write_xml_close_tag(Tag) :-
		write(coverage_report, '</'),
		write(coverage_report, Tag),
		write(coverage_report, '>'), nl(coverage_report).

	write_xml_element(Tag, Text) :-
		write(coverage_report, '<'),
		write(coverage_report, Tag),
		write(coverage_report, '>'),
		write(coverage_report, Text),
		write(coverage_report, '</'),
		write(coverage_report, Tag),
		write(coverage_report, '>'), nl(coverage_report).

	write_xml_empty_tag(Tag) :-
		write(coverage_report, '<'),
		write(coverage_report, Tag),
		write(coverage_report, '/>'), nl(coverage_report).

	% other auxiliary predicates

	entity_file(Entity, File, Line) :-
		(	current_object(Entity) ->
			object_property(Entity, file(File0)),
			object_property(Entity, lines(Line, _))
		;	current_category(Entity) ->
			category_property(Entity, file(File0)),
			category_property(Entity, lines(Line, _))
		;	atom(Entity), current_protocol(Entity) ->
			protocol_property(Entity, file(File0)),
			protocol_property(Entity, lines(Line, _))
		;	File0 = '',
			Line = -1
		),
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, File, File0) ->
			true
		;	File = File0
		).

	entity_predicate_line(Other::Functor//Arity, Entity, Line) :-
		ExtendedArity is Arity + 2,
		entity_predicate_line(Other::Functor/ExtendedArity, Entity, Line).
	entity_predicate_line(Other::Functor/Arity, Entity, Line) :-
		(	current_object(Entity) ->
			object_property(Entity, provides(Functor/Arity, Other, Properties))
		;	current_category(Entity) ->
			category_property(Entity, provides(Functor/Arity, Other, Properties))
		;	Properties = []
		),
		(	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		).
	entity_predicate_line(Functor//Arity, Entity, Line) :-
		ExtendedArity is Arity + 2,
		entity_predicate_line(Functor/ExtendedArity, Entity, Line).
	entity_predicate_line(Functor/Arity, Entity, Line) :-
		(	current_object(Entity) ->
			object_property(Entity, defines(Functor/Arity, Properties))
		;	current_category(Entity) ->
			category_property(Entity, defines(Functor/Arity, Properties))
		;	Properties = []
		),
		(	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

:- end_object.
