%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(xunit_xml_report).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2016/02/20,
		comment is 'Intercepts unit test execution messages and generates a report.xml file using the XUnit XML format.'
	]).

	:- private(message_cache_/1).
	:- dynamic(message_cache_/1).
	:- mode(message_cache_(?callable), zero_or_more).
	:- info(message_cache_/1, [
		comment is 'Table of messages emitted by the lgtunit tool.',
		argnames is ['Message']
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		message_hook(Message),
		% allow default processing of the messages
		fail.

	message_hook(tests_start_date_time(Year,Month,Day,Hours,Minutes,Seconds)) :-
		!,
		retractall(message_cache_(_)),
		assertz(message_cache_(tests_start_date_time(Year,Month,Day,Hours,Minutes,Seconds))).
	message_hook(tests_end_date_time(Year,Month,Day,Hours,Minutes,Seconds)) :-
		!,
		assertz(message_cache_(tests_end_date_time(Year,Month,Day,Hours,Minutes,Seconds))),
		generate_xml_report.
	message_hook(Message) :-
		assertz(message_cache_(Message)).

	% generate the XML report

	generate_xml_report :-
		message_cache_(running_tests_from_object_file(_, File)),
		logtalk::loaded_file_property(File, directory(Directory)),
		atom_concat(Directory, 'report.xml', ReportFile),
		open(ReportFile, write, Stream),
		generate_report(Stream),
		close(Stream).

	generate_report(Stream) :-
		write_report_header(Stream),
		write_testsuites_element(Stream).
		
	write_report_header(Stream) :-
		write(Stream, '<?xml version="1.0" encoding="UTF-8"?>'), nl(Stream).
	
	write_testsuites_element(Stream) :-
%		testsuites_duration(Duration),
%		write_xml_open_tag(Stream, testsuites, [duration-Duration]),
		write_xml_open_tag(Stream, testsuites, []),
		write_testsuite_element(Stream),
		write_xml_close_tag(Stream, testsuites).

	write_testsuite_element(Stream) :-
		testsuite_stats(Tests, Errors, Failures, Skipped),
		testsuite_name(Name),
		testsuite_package(Package),
		testsuite_time(Time),
		testsuite_timestamp(TimeStamp),
		write_xml_open_tag(Stream, testsuite, [package-Package,name-Name,tests-Tests,errors-Errors,failures-Failures,skipped-Skipped,time-Time,timestamp-TimeStamp,id-0]),
		write_test_elements(Stream),
		write_xml_close_tag(Stream, testsuite).

	write_test_elements(Stream) :-
		test(Test),
		write_testcase_element(Test, Stream),
		fail.
	write_test_elements(_).

	write_testcase_element(Test, Stream) :-
		testcase_classname(Test, ClassName),
		testcase_name(Test, Name),
		testcase_time(Test, Time),
		write_testcase_element_tags(Test, ClassName, Name, Time, Stream).

	write_testcase_element_tags(passed_test(_Test, _File, _Position, _Note), ClassName, Name, Time, Stream) :-
		write_xml_empty_tag(Stream, testcase, [classname-ClassName,name-Name,time-Time]).
	write_testcase_element_tags(failed_test(_Test, _File, _Position, Reason, Note), ClassName, Name, Time, Stream) :-
		write_xml_open_tag(Stream, testcase, [classname-ClassName,name-Name,time-Time]),
		write_xml_element(Stream, failure, [message-Note], Reason),
		write_xml_close_tag(Stream, testcase).
	write_testcase_element_tags(skipped_test(_Test, _File, _Position, _Note), ClassName, Name, Time, Stream) :-
		write_xml_open_tag(Stream, testcase, [classname-ClassName,name-Name,time-Time]),
		write_xml_empty_tag(Stream, skipped, []).

	% testsuites tag attributes

	testsuites_duration(Duration) :-
		message_cache_(tests_start_date_time(Year0, Month0, Day0, Hours0, Minutes0, Seconds0)),
		message_cache_(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		julian_day(Year0, Month0, Day0, JulianDay0),
		julian_day(Year, Month, Day, JulianDay),
		Duration is (JulianDay - JulianDay0) * 86400 + (Hours - Hours0) * 3600 + (Minutes - Minutes0) * 60 + Seconds - Seconds0.

	% testsuite tag attributes

	testsuite_stats(Tests, 0, Failures, Skipped) :-
		message_cache_(tests_results_summary(Tests, Skipped, _, Failures, _)).

	testsuite_name(Name) :-
		message_cache_(running_tests_from_object_file(_, Name)).

	testsuite_package(Package) :-
		message_cache_(running_tests_from_object_file(Object, File)),
		(	logtalk::loaded_file_property(File, library(Library)) ->
			Package = library(Library)
		;	% use the file directory
			object_property(Object, file(_,Package))
		).

	testsuite_time(Time) :-
		% there's a songle testsuite
		testsuites_duration(Time).

	testsuite_timestamp(TimeStamp) :-
		message_cache_(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		date_time_to_timestamp(Year, Month, Day, Hours, Minutes, Seconds, TimeStamp).

	% testcase tag attributes

	testcase_classname(_Test, ClassName) :-
		message_cache_(running_tests_from_object_file(ClassName, _)).

	testcase_name(passed_test(Test,_,_,_), Test).
	testcase_name(failed_test(Test,_,_,_,_), Test).
	testcase_name(skipped_test(Test,_,_,_), Test).

	testcase_time(_, 0.0).

	% testcase tag predicates

	test(passed_test(Test, File, Position, Note)) :-
		message_cache_(passed_test(Test, File, Position, Note)).
	test(failed_test(Test, File, Position, Reason, Note)) :-
		message_cache_(failed_test(Test, File, Position, Reason, Note)).
	test(skipped_test(Test, File, Position, Note)) :-
		message_cache_(skipped_test(Test, File, Position, Note)).

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

	write_xml_empty_tag(Stream, Tag, Atts) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '/>'), nl(Stream).

	write_xml_open_tag(Stream, Tag, Atts) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '>'), nl(Stream).

	write_xml_element(Stream, Tag, Atts, Text) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '>'),
		write(Stream, Text),
		write(Stream, '</'),
		write(Stream, Tag),
		write(Stream, '>'), nl(Stream).

	writeq_xml_cdata_element(Stream, Tag, Atts, Text) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '><![CDATA['),
		pretty_print_vars_quoted(Stream, Text),
		write(Stream, ']]></'),
		write(Stream, Tag),
		write(Stream, '>'), nl(Stream).

	write_xml_cdata_element(Stream, Tag, Atts, Text) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '><![CDATA['),
		pretty_print_vars(Stream, Text),
		write(Stream, ']]></'),
		write(Stream, Tag),
		write(Stream, '>'), nl(Stream).

	write_xml_tag_attributes(_, []) :-
		!.
	write_xml_tag_attributes(Stream, [Attribute-Value| Rest]) :-
		write(Stream, ' '),
		write(Stream, Attribute),
		write(Stream, '="'),
		write(Stream, Value),
		write(Stream, '"'),
		write_xml_tag_attributes(Stream, Rest).

	write_xml_close_tag(Stream, Tag) :-
		write(Stream, '</'),
		write(Stream, Tag),
		write(Stream, '>'),
		nl(Stream).

	pretty_print_vars(Stream, Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Stream, Term, [numbervars(true)])).

	pretty_print_vars_quoted(Stream, Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Stream, Term, [numbervars(true), quoted(true)])).

:- end_object.
