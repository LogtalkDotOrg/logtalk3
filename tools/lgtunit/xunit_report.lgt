%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


% define a flag to allow the logtalk_tester script to pass the
% option to supress the test file and directory path prefix
:- initialization(
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)])
).


:- object(xunit_report).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/12/15,
		comment is 'Intercepts unit test execution messages and generates a xunit_report.xml file using the xUnit XML format in the same directory as the tests object file.'
	]).

	:- private(message_cache_/1).
	:- dynamic(message_cache_/1).
	:- mode(message_cache_(?callable), zero_or_more).
	:- info(message_cache_/1, [
		comment is 'Table of messages emitted by the lgtunit tool when running tests.',
		argnames is ['Message']
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
	message_hook(running_tests_from_object_file(_, File)) :-
		logtalk::loaded_file_property(File, directory(Directory)),
		atom_concat(Directory, 'xunit_report.xml', ReportFile),
		(	stream_property(_, alias(xunit_report)) ->
			true
		;	open(ReportFile, write, _, [alias(xunit_report)])
		).
	% stop
	message_hook(tests_ended) :-
		close(xunit_report).
	message_hook(tests_end_date_time(Year,Month,Day,Hours,Minutes,Seconds)) :-
		!,
		assertz(message_cache_(tests_end_date_time(Year,Month,Day,Hours,Minutes,Seconds))),
		generate_xml_report.
%	message_hook(running_tests_from_object_file(Object,File)) :-
%		!,
%		assertz(message_cache_(running_tests_from_object_file(Object,File))),
%		fail.
%	message_hook(tests_ended) :-
%		!,
%		assertz(message_cache_(tests_ended)),
%		fail.
	message_hook(Message) :-
		assertz(message_cache_(Message)).

	% generate the XML report

	generate_xml_report :-
		write_report_header,
		write_testsuites_element.
		
	write_report_header :-
		write(xunit_report, '<?xml version="1.0" encoding="UTF-8"?>'), nl(xunit_report).

	write_testsuites_element :-
%		testsuites_duration(Duration),
%		write_xml_open_tag(testsuites, [duration-Duration]),
		write_xml_open_tag(testsuites, []),
		write_testsuite_element,
		write_xml_close_tag(testsuites).

	write_testsuite_element :-
		testsuite_stats(Tests, Errors, Failures, Skipped),
		testsuite_name(Name),
		testsuite_package(Package),
		testsuite_time(Time),
		testsuite_timestamp(TimeStamp),
		write_xml_open_tag(testsuite,
			[package-Package, name-Name,
			 tests-Tests, errors-Errors, failures-Failures, skipped-Skipped,
			 time-Time, timestamp-TimeStamp, id-0
			]
		),
		write_test_elements,
		write_xml_close_tag(testsuite).

	write_test_elements :-
		testcase_classname_name_time(Test, ClassName, Name, Time),
		write_testcase_element_tags(Test, ClassName, Name, Time),
		fail.
	write_test_elements.

	write_testcase_element_tags(passed_test(_Test, _File, _Position, _Note), ClassName, Name, Time) :-
		write_xml_empty_tag(testcase, [classname-ClassName,name-Name,time-Time]).
	write_testcase_element_tags(failed_test(_Test, _File, _Position, Reason, Note), ClassName, Name, Time) :-
		write_xml_open_tag(testcase, [classname-ClassName,name-Name,time-Time]),
		write_xml_element(failure, [message-Note], Reason),
		write_xml_close_tag(testcase).
	write_testcase_element_tags(skipped_test(_Test, _File, _Position, _Note), ClassName, Name, Time) :-
		write_xml_open_tag(testcase, [classname-ClassName,name-Name,time-Time]),
		write_xml_empty_tag(skipped, []),
		write_xml_close_tag(testcase).

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

	testsuite_stats(Tests, 0, Failures, Skipped) :-
		testsuite_stats(0, Tests, 0, Failures, 0, Skipped).
	
	testsuite_stats(Tests0, Tests, Failures0, Failures, Skipped0, Skipped) :-
		(	retract(message_cache_(tests_results_summary(_Object, PartialTests, PartialSkipped, _, PartialFailures, _))) ->
			Tests1 is Tests0 + PartialTests,
			Failures1 is Failures0 + PartialFailures,
			Skipped1 is Skipped0 + PartialSkipped,
			testsuite_stats(Tests1, Tests, Failures1, Failures, Skipped1, Skipped)
		;	Tests is Tests0,
			Failures is Failures0,
			Skipped is Skipped0
		).

	testsuite_name(Name) :-
		once(message_cache_(running_tests_from_object_file(_, File))),
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, Name, File) ->
			true
		;	Name = File
		).

	testsuite_package(Package) :-
		once(message_cache_(running_tests_from_object_file(Object, File))),
		(	logtalk::loaded_file_property(File, library(Library)),
			Library \== startup ->
			Package = library(Library)
		;	% use the file directory
			object_property(Object, file(_,Directory)),
			% bypass the compiler as the flag is only created after loading this file
			{current_logtalk_flag(suppress_path_prefix, Prefix)},
			(	atom_concat(Prefix, Package, Directory) ->
				true
			;	Package = Directory
			)
		).

	testsuite_time(Time) :-
		% there's a single testsuite
		testsuites_duration(Time).

	testsuite_timestamp(TimeStamp) :-
		message_cache_(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		date_time_to_timestamp(Year, Month, Day, Hours, Minutes, Seconds, TimeStamp).

	% "testcase" tag predicates

	testcase_classname_name_time(passed_test(Test, File, Position, Note), Object, Test, 0.0) :-
		message_cache_(passed_test(Object, Test, File, Position, Note)).
	testcase_classname_name_time(failed_test(Test, File, Position, Reason, Note), Object, Test, 0.0) :-
		message_cache_(failed_test(Object, Test, File, Position, Reason, Note)).
	testcase_classname_name_time(skipped_test(Test, File, Position, Note), Object, Test, 0.0) :-
		message_cache_(skipped_test(Object, Test, File, Position, Note)).

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

	write_xml_element(Tag, Atts, Text) :-
		write(xunit_report, '<'),
		write(xunit_report, Tag),
		write_xml_tag_attributes(Atts),
		write(xunit_report, '>'),
		write(xunit_report, Text),
		write(xunit_report, '</'),
		write(xunit_report, Tag),
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

	write_xml_cdata_element(Tag, Atts, Text) :-
		write(xunit_report, '<'),
		write(xunit_report, Tag),
		write_xml_tag_attributes(Atts),
		write(xunit_report, '><![CDATA['),
		pretty_print_vars(Text),
		write(xunit_report, ']]></'),
		write(xunit_report, Tag),
		write(xunit_report, '>'), nl(xunit_report).

	write_xml_tag_attributes([]).
	write_xml_tag_attributes([Attribute-Value| Rest]) :-
		write(xunit_report, ' '),
		write(xunit_report, Attribute),
		write(xunit_report, '="'),
		write(xunit_report, Value),
		write(xunit_report, '"'),
		write_xml_tag_attributes(Rest).

	write_xml_close_tag(Tag) :-
		write(xunit_report, '</'),
		write(xunit_report, Tag),
		write(xunit_report, '>'), nl(xunit_report).

	pretty_print_vars(Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(xunit_report, Term, [numbervars(true)])
		).

	pretty_print_vars_quoted(Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(xunit_report, Term, [numbervars(true), quoted(true)])
		).

:- end_object.
