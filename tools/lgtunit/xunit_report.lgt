%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
	% are created (e.g. when running tests defined in a directory different
	% from the directory that contains the tests driver file)
	create_logtalk_flag(tests_report_directory, '', [type(atom), keep(true)]),
	% define a flag to allow the logtalk_tester script to pass the
	% base URL for generating links to test files
	create_logtalk_flag(tests_base_url, '', [type(atom), keep(true)])
)).


:- object(xunit_report).

	:- info([
		version is 5:0:1,
		author is 'Paulo Moura',
		date is 2024-04-01,
		comment is 'Intercepts unit test execution messages and generates a ``xunit_report.xml`` file using the xUnit XML format in the same directory as the tests object file.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(xunit_report))``.'
		]
	]).

	:- private(message_cache_/1).
	:- dynamic(message_cache_/1).
	:- mode(message_cache_(?callable), zero_or_more).
	:- info(message_cache_/1, [
		comment is 'Table of messages emitted by the lgtunit tool when running tests.',
		argnames is ['Message']
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		message_hook(Message),
		% allow default processing of the messages
		fail.

	% start
	message_hook(tests_start_date_time(Year,Month,Day,Hours,Minutes,Seconds)) :-
		!,
		retractall(message_cache_(_)),
		assertz(message_cache_(tests_start_date_time(Year,Month,Day,Hours,Minutes,Seconds))).
	message_hook(running_tests_from_object_file(Object, File)) :-
		!,
		(	% bypass the compiler as the flag is only created after loading this file
			{current_logtalk_flag(tests_report_directory, Directory)}, Directory \== '' ->
			true
		;	logtalk::loaded_file_property(File, directory(Directory))
		),
		atom_concat(Directory, 'xunit_report.xml', ReportFile),
		(	stream_property(_, alias(xunit_report)) ->
			true
		;	open(ReportFile, write, _, [alias(xunit_report)])
		),
		assertz(message_cache_(running_tests_from_object_file(Object,File))).
	% stop
	message_hook(tests_ended) :-
		!,
		close(xunit_report).
	message_hook(tests_end_date_time(Year,Month,Day,Hours,Minutes,Seconds)) :-
		!,
		assertz(message_cache_(tests_end_date_time(Year,Month,Day,Hours,Minutes,Seconds))),
		generate_xml_report.
	% "testcase" tag predicates
	message_hook(passed_test(Object, Test, File, Position, Note, CPUTime, WallTime)) :-
		!,
		assertz(message_cache_(test(Object, Test, passed_test(File, Position, Note, CPUTime, WallTime)))).
	message_hook(failed_test(Object, Test, File, Position, Reason, Note, CPUTime, WallTime)) :-
		!,
		assertz(message_cache_(test(Object, Test, failed_test(File, Position, Reason, Note, CPUTime, WallTime)))).
	message_hook(skipped_test(Object, Test, File, Position, Note)) :-
		!,
		assertz(message_cache_(test(Object, Test, skipped_test(File, Position, Note)))).
	% catchall clause
	message_hook(Message) :-
		assertz(message_cache_(Message)).

	% generate the XML report

	generate_xml_report :-
		write_report_header,
		write_testsuites_element.

	write_report_header :-
		write(xunit_report, '<?xml version="1.0" encoding="UTF-8"?>'), nl(xunit_report).

	write_testsuites_element :-
		testsuites_duration(Duration),
		write_xml_open_tag(testsuites, [duration-Duration]),
		write_testsuite_elements,
		write_xml_close_tag(testsuites).

	write_testsuite_elements :-
		message_cache_(running_tests_from_object_file(Object, _)),
		write_testsuite_element(Object),
		fail.
	write_testsuite_elements.

	write_testsuite_element(Object) :-
		message_cache_(running_tests_from_object_file(Object, _)),
		testsuite_stats(Object, Tests, Errors, Failures, Skipped),
		testsuite_name(Object, Name),
		testsuite_package(Object, Package),
		testsuite_time(Object, Time),
		testsuite_timestamp(TimeStamp),
		write_xml_open_tag(testsuite,
			[	package-Package, name-Name,
				tests-Tests, errors-Errors, failures-Failures, skipped-Skipped,
				time-Time, timestamp-TimeStamp
			]
		),
		write_test_elements(Object),
		write_xml_close_tag(testsuite).

	write_test_elements(Object) :-
		message_cache_(tests_skipped(Object, File, Note)),
		Object::test(Name),
		write_testcase_element_tags(skipped_test(File, 0-0, Note), Object, Name),
		fail.
	write_test_elements(Object) :-
		message_cache_(test(Object, Name, Test)),
		write_testcase_element_tags(Test, Object, Name),
		fail.
	write_test_elements(_).

	write_testcase_element_tags(passed_test(File, Position, Note, _, WallTime), ClassName, Name) :-
		suppress_path_prefix(File, Short),
		write_xml_open_tag(testcase, [classname-ClassName, name-Name, time-WallTime]),
		write_testcase_properties(Short, Position, Note),
		write_xml_close_tag(testcase).
	write_testcase_element_tags(failed_test(File, Position, Reason, Note, _, WallTime), ClassName, Name) :-
		suppress_path_prefix(File, Short),
		failed_test(Reason, Description, Type, Error),
		write_xml_open_tag(testcase, [classname-ClassName, name-Name, time-WallTime]),
		write_testcase_properties(Short, Position, Note),
		(	Error == '' ->
			write_xml_empty_tag(failure, [message-Description, type-Type])
		;	writeq_xml_cdata_element(failure, [message-Description, type-Type], Error)
		),
		write_xml_close_tag(testcase).
	write_testcase_element_tags(skipped_test(File, Position, Note), ClassName, Name) :-
		suppress_path_prefix(File, Short),
		write_xml_open_tag(testcase, [classname-ClassName, name-Name, time-0.0]),
		write_testcase_properties(Short, Position, Note),
		write_xml_empty_tag(skipped, [message-'Skipped test']),
		write_xml_close_tag(testcase).

	% failed_test(Reason, Description, Type, Error)
	failed_test(non_deterministic_success, 'Non-deterministic success', non_deterministic_success, '').
	failed_test(failure_instead_of_error(Error), 'Failure instead of error', failure_instead_of_error, Error).
	failed_test(failure_instead_of_success, 'Failure instead of success', failure_instead_of_success, '').
	failed_test(error_instead_of_success(Error), 'Error instead of success', error_instead_of_success, Error).
	failed_test(error_instead_of_failure(Error), 'Error instead of failure', error_instead_of_failure, Error).
	failed_test(success_instead_of_error(Error), 'Success instead of error', success_instead_of_error, Error).
	failed_test(success_instead_of_failure, 'Success instead of failure', success_instead_of_failure, '').
	failed_test(wrong_error(_, Error), 'Wrong error', wrong_error, Error).
	failed_test(quick_check_failed(Error, _, _), 'QuickCheck test failed', quick_check_failed, Error).
	failed_test(quick_check_error(Error, _, _), 'QuickCheck test error', quick_check_error, Error).
	failed_test(step_error(_, Error), 'Test step error', step_error, Error).
	failed_test(step_failure(Step), 'Test step failure', step_failure, Step).

	% "testsuites" tag attributes

	testsuites_duration(Duration) :-
		message_cache_(tests_start_date_time(Year0, Month0, Day0, Hours0, Minutes0, Seconds0)),
		message_cache_(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		julian_day(Year0, Month0, Day0, JulianDay0),
		julian_day(Year, Month, Day, JulianDay),
		Duration is
			(JulianDay - JulianDay0) * 86400 +
			(Hours - Hours0) * 3600 +
			(Minutes - Minutes0) * 60 +
			Seconds - Seconds0.

	% "testsuite" tag attributes

	testsuite_stats(Object, Tests, 0, 0, Tests) :-
		message_cache_(tests_skipped(Object, _File, _Note)),
		!,
		Object::number_of_tests(Tests).
	testsuite_stats(Object, Tests, 0, Failures, Skipped) :-
		once(message_cache_(tests_results_summary(Object, Tests, Skipped, _, Failures, _, _))),
		!.

	testsuite_name(Object, Name::Object) :-
		once(message_cache_(running_tests_from_object_file(Object, File))),
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, Name, File) ->
			true
		;	Name = File
		).

	testsuite_package(Object, Package) :-
		once(message_cache_(running_tests_from_object_file(Object, File))),
		(	logtalk::loaded_file_property(File, library(Library)),
			Library \== startup ->
			Package = library(Library)
		;	% use the object file directory
			object_property(Object, file(_,Directory)),
			% bypass the compiler as the flag is only created after loading this file
			{current_logtalk_flag(suppress_path_prefix, Prefix)},
			(	atom_concat(Prefix, Package, Directory) ->
				true
			;	Package = Directory
			)
		).

	testsuite_time(Object, Time) :-
		findall(
			WallTime,
			(	message_cache_(test(Object, _, passed_test(_, _, _, _, WallTime)))
			;	message_cache_(test(Object, _, failed_test(_, _, _, _, _, WallTime)))
			),
			WallTimes
		),
		sum(WallTimes, 0, Time).

	testsuite_timestamp(TimeStamp) :-
		message_cache_(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		date_time_to_timestamp(Year, Month, Day, Hours, Minutes, Seconds, TimeStamp).

	% "testcase" tag properties

	suppress_path_prefix(Path, ShortPath) :-
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, ShortPath, Path) ->
			true
		;	ShortPath = Path
		).

	write_testcase_properties(Short, Position, Note) :-
		write_xml_open_tag(properties, []),
		write_xml_empty_tag(property, [name-file, value-Short]),
		write_xml_empty_tag(property, [name-position, value-Position]),
		(	tests_url(Short, Position, URL) ->
			write_xml_empty_tag(property, [name-url, value-URL])
		;	true
		),
		(	Note \== '' ->
			write_xml_empty_tag(property, [name-note, value-Note])
		;	true
		),
		write_xml_close_tag(properties).

	tests_url(Short, Position, URL) :-
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(tests_base_url, BaseURL)},
		BaseURL \== '',
		Position = Line-_,
		(	sub_atom(BaseURL, _, _, _, bitbucket) ->
			LineSeparator = '#'
		;	% assume GitHub or GitLab host
			LineSeparator = '#L'
		),
		(	sub_atom(BaseURL, _, 1, 0, '/') ->
			(	sub_atom(Short, 0, 1, _, '/') ->
				sub_atom(Short, 1, _, 0, ShortSuffix),
				atomic_list_concat([BaseURL, ShortSuffix, LineSeparator, Line], URL)
			;	atomic_list_concat([BaseURL, Short, LineSeparator, Line], URL)
			)
		;	(	sub_atom(Short, 0, 1, _, '/') ->
				atomic_list_concat([BaseURL, Short, LineSeparator, Line], URL)
			;	atomic_list_concat([BaseURL, '/', Short, LineSeparator, Line], URL)
			)
		).

	% date and time auxiliary predicates

	julian_day(Year, Month, Day, JulianDay) :-
		% code copied from Daniel L. Dudley iso8601 contribution to Logtalk
		A is (14 - Month) // 12,
		Y is Year + 4800 - A,
		M is Month + (12 * A) - 3,
		D is Day + ((153 * M + 2) // 5) + (365 * Y) + (Y // 4),
		JulianDay is D - (Y // 100) + (Y // 400) - 32045.

	date_time_to_timestamp(Year, Month, Day, Hours, Minutes, Seconds, TimeStamp) :-
		integers_to_atoms([Year,Month,Day,Hours,Minutes,Seconds], [AYear,AMonth0,ADay0,AHours0,AMinutes0,ASeconds0]),
		pad_single_char_atoms([AMonth0,ADay0,AHours0,AMinutes0,ASeconds0], [AMonth,ADay,AHours,AMinutes,ASeconds]),
		atomic_list_concat([AYear,'-',AMonth,'-',ADay,'T',AHours,':',AMinutes,':',ASeconds], TimeStamp).

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

	% XML auxiliary predicates

	write_xml_empty_tag(Tag, Atts) :-
		write(xunit_report, '<'),
		write(xunit_report, Tag),
		write_xml_tag_attributes(Atts),
		write(xunit_report, '/>'), nl(xunit_report).

	write_xml_open_tag(Tag, Atts) :-
		write(xunit_report, '<'),
		write(xunit_report, Tag),
		write_xml_tag_attributes(Atts),
		write(xunit_report, '>'), nl(xunit_report).

	writeq_xml_cdata_element(Tag, Atts, Text) :-
		write(xunit_report, '<'),
		write(xunit_report, Tag),
		write_xml_tag_attributes(Atts),
		write(xunit_report, '><![CDATA['),
		pretty_print_vars_quoted(Text),
		write(xunit_report, ']]></'),
		write(xunit_report, Tag),
		write(xunit_report, '>'), nl(xunit_report).

	write_xml_tag_attributes([]).
	write_xml_tag_attributes([Attribute-Value| Rest]) :-
		write(xunit_report, ' '),
		write(xunit_report, Attribute),
		write(xunit_report, '="'),
		escape_special_characters(Value, Escaped),
		write(xunit_report, Escaped),
		write(xunit_report, '"'),
		write_xml_tag_attributes(Rest).

	write_xml_close_tag(Tag) :-
		write(xunit_report, '</'),
		write(xunit_report, Tag),
		write(xunit_report, '>'), nl(xunit_report).

	escape_special_characters(Term, Escaped) :-
		(	var(Term) ->
			Escaped = Term
		;	atom(Term) ->
			atom_chars(Term, List),
			phrase(escape_special_characters(List), EscapedList),
			atom_chars(Escaped, EscapedList)
		;	compound(Term) ->
			Term =.. List,
			escape_special_characters_list(List, EscapedList),
			Escaped =.. EscapedList
		;	Escaped = Term
		).

	escape_special_character((<))  --> [(&),l,t,';'].
	escape_special_character((>))  --> [(&),g,t,';'].
	escape_special_character('"')  --> [(&),q,u,o,t,';'].
	escape_special_character('\'') --> [(&),a,p,o,s,';'].
	escape_special_character((&))  --> [(&),a,m,p,';'].

	escape_special_characters([]) -->
		[].
	escape_special_characters([Char| Chars]) -->
		escape_special_character(Char),
		!,
		escape_special_characters(Chars).
	escape_special_characters([Char| Chars]) -->
		[Char],
		escape_special_characters(Chars).

	escape_special_characters_list([], []).
	escape_special_characters_list([Argument| Arguments], [EscapedArgument| EscapedArguments]) :-
		escape_special_characters(Argument, EscapedArgument),
		escape_special_characters_list(Arguments, EscapedArguments).

	pretty_print_vars_quoted(Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(xunit_report, Term, [numbervars(true), quoted(true)])
		).

	sum([], Sum, Sum).
	sum([X| Xs], Acc, Sum) :-
		Acc2 is Acc + X,
		sum(Xs, Acc2, Sum).

:- end_object.
