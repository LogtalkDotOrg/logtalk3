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


:- object(cobertura_report).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-27,
		comment is 'Intercepts unit test execution messages and generates a ``cobertura.xml`` file with code coverage results.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(cobertura_report))``.'
		]
	]).

	:- private(timestamp_/6).
	:- dynamic(timestamp_/6).

	:- private(class_/5).
	:- dynamic(class_/5).

	:- private(method_/7).
	:- dynamic(method_/7).

	:- private(lines_covered_/1).
	:- dynamic(lines_covered_/1).

	:- private(lines_total_/1).
	:- dynamic(lines_total_/1).

	:- private(summary_/2).
	:- dynamic(summary_/2).

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
		retractall(timestamp_(_, _, _, _, _, _)),
		retractall(class_(_, _, _, _, _)),
		retractall(method_(_, _, _, _, _, _, _)),
		retractall(lines_covered_(_)),
		retractall(lines_total_(_)),
		retractall(summary_(_, _)),
		assertz(lines_covered_(0)),
		assertz(lines_total_(0)).

	message_hook(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		!,
		assertz(timestamp_(Year, Month, Day, Hours, Minutes, Seconds)).

	message_hook(running_tests_from_object_file(_Object, File)) :-
		stream_property(_, alias(cobertura_report)),
		!,
		logtalk::loaded_file_property(File, directory(_)).
	message_hook(running_tests_from_object_file(_Object, File)) :-
		( 	% bypass the compiler as the flag is only created after loading this file
			{current_logtalk_flag(tests_report_directory, Directory)}, Directory \== '' ->
			true
		;	logtalk::loaded_file_property(File, directory(Directory))
		),
		atom_concat(Directory, 'cobertura.xml', ReportFile),
		open(ReportFile, write, _, [alias(cobertura_report)]).

	message_hook(entity_coverage_starts(Entity)) :-
		entity_file(Entity, File, Line),
		assertz(class_(Entity, File, Line, 0, 0)).

	message_hook(entity_predicate_coverage(Entity, Predicate, Covered, Total, Percentage, Clauses)) :-
		entity_predicate_line(Predicate, Entity, Line),
		assertz(method_(Entity, Predicate, Line, Covered, Total, Percentage, Clauses)),
		retract(lines_covered_(Covered0)),
		Covered1 is Covered0 + Covered,
		assertz(lines_covered_(Covered1)),
		retract(lines_total_(Total0)),
		Total1 is Total0 + Total,
		assertz(lines_total_(Total1)).

	message_hook(entity_coverage(Entity, Covered, Total, _Percentage)) :-
		retract(class_(Entity, File, Line, _, _)),
		assertz(class_(Entity, File, Line, Covered, Total)).

	message_hook(covered_entities_numbers(Covered, Total, _Percentage)) :-
		assertz(summary_(Covered, Total)).

	message_hook(tests_ended) :-
		lines_covered_(LinesCovered),
		lines_total_(LinesTotal),
		( 	LinesTotal =:= 0 ->
			LineRate is 0.0
		;	LineRate is float(LinesCovered / LinesTotal)
		),
		( 	summary_(EntitiesCovered, EntitiesTotal) ->
			true
		;	EntitiesCovered = 0,
			EntitiesTotal = 0
		),
		( 	EntitiesTotal =:= 0 ->
			EntitiesRate = 0.0
		;	EntitiesRate is float(EntitiesCovered / EntitiesTotal)
		),
		timestamp_value(TimeStamp),
		write(cobertura_report, '<?xml version="1.0" encoding="UTF-8"?>'), nl(cobertura_report),
		write(cobertura_report, '<coverage line-rate="'),
		write(cobertura_report, LineRate),
		write(cobertura_report, '" branch-rate="0.0" lines-covered="'),
		write(cobertura_report, LinesCovered),
		write(cobertura_report, '" lines-valid="'),
		write(cobertura_report, LinesTotal),
		write(cobertura_report, '" branches-covered="0" branches-valid="0" complexity="0.0" version="lgtunit" timestamp="'),
		write(cobertura_report, TimeStamp),
		write(cobertura_report, '">'), nl(cobertura_report),
		write(cobertura_report, '  <sources>'), nl(cobertura_report),
		write(cobertura_report, '    <source>.</source>'), nl(cobertura_report),
		write(cobertura_report, '  </sources>'), nl(cobertura_report),
		write(cobertura_report, '  <packages>'), nl(cobertura_report),
		write(cobertura_report, '    <package name="tests" line-rate="'),
		write(cobertura_report, EntitiesRate),
		write(cobertura_report, '" branch-rate="0.0" complexity="0.0">'), nl(cobertura_report),
		write(cobertura_report, '      <classes>'), nl(cobertura_report),
		forall(
			class_(Entity, File, _Line, Covered, Total),
			write_class(Entity, File, Covered, Total)
		),
		write(cobertura_report, '      </classes>'), nl(cobertura_report),
		write(cobertura_report, '    </package>'), nl(cobertura_report),
		write(cobertura_report, '  </packages>'), nl(cobertura_report),
		write(cobertura_report, '</coverage>'), nl(cobertura_report),
		close(cobertura_report).

	timestamp_value(TimeStamp) :-
		( 	timestamp_(Year, Month, Day, Hours, Minutes, Seconds) ->
			TimeStamp is Year*10000000000 + Month*100000000 + Day*1000000 + Hours*10000 + Minutes*100 + Seconds
		;	TimeStamp is 0
		).

	write_class(Entity, File, Covered, Total) :-
		( 	Total =:= 0 ->
			ClassRate is 0.0
		;	ClassRate is float(Covered / Total)
		),
		write(cobertura_report, '        <class name="'),
		write(cobertura_report, Entity),
		write(cobertura_report, '" filename="'),
		write(cobertura_report, File),
		write(cobertura_report, '" line-rate="'),
		write(cobertura_report, ClassRate),
		write(cobertura_report, '" branch-rate="0.0" complexity="0.0">'), nl(cobertura_report),
		write(cobertura_report, '          <methods>'), nl(cobertura_report),
		forall(
			method_(Entity, Predicate, Line, Covered0, Total0, _MethodRate, _Clauses),
			write_method(Predicate, Line, Covered0, Total0)
		),
		write(cobertura_report, '          </methods>'), nl(cobertura_report),
		write(cobertura_report, '          <lines>'), nl(cobertura_report),
		forall(
			method_(Entity, _Predicate, Line, Covered, _Total, _MethodRate, _Clauses),
			write_line(Line, Covered)
		),
		write(cobertura_report, '          </lines>'), nl(cobertura_report),
		write(cobertura_report, '        </class>'), nl(cobertura_report).

	write_method(Predicate, Line, Covered, Total) :-
		( 	Total =:= 0 ->
			MethodRate is 0.0
		;	MethodRate is float(Covered / Total)
		),
		write(cobertura_report, '            <method name="'),
		write(cobertura_report, Predicate),
		write(cobertura_report, '" signature="" line-rate="'),
		write(cobertura_report, MethodRate),
		write(cobertura_report, '" branch-rate="0.0">'), nl(cobertura_report),
		write(cobertura_report, '              <lines>'), nl(cobertura_report),
		write_line(Line, Covered),
		write(cobertura_report, '              </lines>'), nl(cobertura_report),
		write(cobertura_report, '            </method>'), nl(cobertura_report).

	write_line(Line, Covered) :-
		( 	Covered > 0 ->
			Hits = Covered
		;	Hits = 0
		),
		write(cobertura_report, '                <line number="'),
		write(cobertura_report, Line),
		write(cobertura_report, '" hits="'),
		write(cobertura_report, Hits),
		write(cobertura_report, '" branch="false"/>'), nl(cobertura_report).

	entity_file(Entity, File, Line) :-
		( 	current_object(Entity) ->
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
