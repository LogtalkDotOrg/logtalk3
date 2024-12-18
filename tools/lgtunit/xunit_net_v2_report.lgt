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
	% are created (e.g., when running tests defined in a directory different
	% from the directory that contains the tests driver file)
	create_logtalk_flag(tests_report_directory, '', [type(atom), keep(true)]),
	% define a flag to allow the logtalk_tester script to pass the
	% base URL for generating links to test files
	create_logtalk_flag(tests_base_url, '', [type(atom), keep(true)])
)).


:- object(xunit_net_v2_report).

	:- info([
		version is 5:0:1,
		author is 'Paulo Moura',
		date is 2024-04-01,
		comment is 'Intercepts unit test execution messages and generates a ``xunit_report.xml`` file using the xUnit.net v2 XML format in the same directory as the tests object file.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(xunit_net_v2_report))``.'
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
		write_assemblies_element.

	write_report_header :-
		write(xunit_report, '<?xml version="1.0" encoding="UTF-8"?>'), nl(xunit_report).

	write_assemblies_element :-
		assemblies_start_rtf(StartRtf),
		assemblies_finish_rtf(FinishRtf),
		write_xml_open_tag(assemblies, [
			'start-rtf' - StartRtf,
			'finish-rtf' - FinishRtf
		]),
		write_assembly_elements,
		write_xml_close_tag(assemblies).

	write_assembly_elements :-
		message_cache_(running_tests_from_object_file(Object, _)),
		write_assembly_element(Object),
		fail.
	write_assembly_elements.

	write_assembly_element(Object) :-
		assembly_name(Object, Name),
		assembly_config_file(Object, ConfigFile),
		assembly_start_rtf(StartRtf),
		assembly_finish_rtf(FinishRtf),
		assembly_time(Object, Time),
		assembly_run_date(RunDate),
		assembly_run_time(RunTime),
		assembly_stats(Object, Total, Skipped, Passed, Failed),
		write_xml_open_tag(assembly,
			[	name-Name, 'config-file'-ConfigFile, 'test-framework'-lgtunit,
				'start-rtf' - StartRtf, 'finish-rtf' - FinishRtf,
				'run-date'-RunDate, 'run-time'-RunTime,
				time-Time,
				total-Total, passed-Passed, failed-Failed, skipped-Skipped, errors-0
			]
		),
		write_errors_element(Object),
		write_collection_element(Object),
		write_xml_close_tag(assembly).

	write_errors_element(Object) :-
		\+ message_cache_(broken_step(_, Object, _)),
		\+ message_cache_(failed_step(_, Object)),
		!.
	write_errors_element(Object) :-
		write_xml_open_tag(errors, []),
		message_cache_(broken_step(Step, Object, Error)),
		write_xml_open_tag(error, [type-error, name-Step]),
		write_xml_open_tag(failure, []),
		writeq_xml_cdata_element('stack-trace', [], Error),
		write_xml_close_tag(failure),
		write_xml_close_tag(error),
		fail.
	write_errors_element(Object) :-
		message_cache_(failed_step(Step, Object)),
		write_xml_open_tag(error, [type-failure, name-Step]),
		write_xml_empty_tag(failure, []),
		write_xml_close_tag(error),
		fail.
	write_errors_element(_) :-
		write_xml_close_tag(errors).

	write_collection_element(Object) :-
		collection_stats(Object, Total, Skipped, Passed, Failed),
		collection_name(Object, Name),
		collection_time(Object, Time),
		write_xml_open_tag(collection,
			[	name-Name, time-Time,
				total-Total, passed-Passed, failed-Failed, skipped-Skipped
			]
		),
		write_test_elements(Object),
		write_xml_close_tag(collection).

	write_test_elements(Object) :-
		message_cache_(tests_skipped(Object, File, Note)),
		Object::test(Name),
		write_test_element_tags(skipped_test(File, 0-0, Note), Name, Object),
		fail.
	write_test_elements(Object) :-
		message_cache_(test(Object, Name, Test)),
		write_test_element_tags(Test, Name, Object),
		fail.
	write_test_elements(_).

	write_test_element_tags(passed_test(File, Position, Note, _, WallTime), Name, Object) :-
		suppress_path_prefix(File, Short),
		write_xml_open_tag(test, [name-(Name::Object), type-(Short::Object), method-Name, time-WallTime, result-'Pass']),
		write_test_element_traits(Short, Position, Note),
		write_xml_close_tag(test).
	write_test_element_tags(failed_test(File, Position, Reason, Note, _, WallTime), Name, Object) :-
		suppress_path_prefix(File, Short),
		write_xml_open_tag(test, [name-(Name::Object), type-(Short::Object), method-Name, time-WallTime, result-'Fail']),
		write_test_element_traits(Short, Position, Note),
		write_xml_open_tag(failure, []),
		failed_test(Reason, Description, _, Error),
		write_xml_element(message, [], Description),
		(	Error == '' ->
			true
		;	writeq_xml_cdata_element('stack-trace', [], Error)
		),
		write_xml_close_tag(failure),
		write_xml_close_tag(test).
	write_test_element_tags(skipped_test(File, Position, Note), Name, Object) :-
		suppress_path_prefix(File, Short),
		write_xml_open_tag(test, [name-(Name::Object), type-(Short::Object), method-Name, time-0.0, result-'Skip']),
		write_test_element_traits(Short, Position, Note),
		write_xml_element(reason, [], 'Skipped test'),
		write_xml_close_tag(test).

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

	% "assemblies" tag attributes

	assemblies_start_rtf(StartRtf) :-
		message_cache_(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		integers_to_atoms([Year,Month,Day,Hours,Minutes,Seconds], [AYear,AMonth0,ADay0,AHours0,AMinutes0,ASeconds0]),
		pad_single_char_atoms([AMonth0,ADay0,AHours0,AMinutes0,ASeconds0], [AMonth,ADay,AHours,AMinutes,ASeconds]),
		atomic_list_concat([AYear,'-',AMonth,'-',ADay,'T',AHours,':',AMinutes,':',ASeconds], StartRtf).

	assemblies_finish_rtf(FinishRtf) :-
		message_cache_(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		integers_to_atoms([Year,Month,Day,Hours,Minutes,Seconds], [AYear,AMonth0,ADay0,AHours0,AMinutes0,ASeconds0]),
		pad_single_char_atoms([AMonth0,ADay0,AHours0,AMinutes0,ASeconds0], [AMonth,ADay,AHours,AMinutes,ASeconds]),
		atomic_list_concat([AYear,'-',AMonth,'-',ADay,'T',AHours,':',AMinutes,':',ASeconds], FinishRtf).

	% "assembly" tag attributes

	assembly_start_rtf(StartRtf) :-
		assemblies_start_rtf(StartRtf).

	assembly_finish_rtf(FinishRtf) :-
		assemblies_finish_rtf(FinishRtf).

	assembly_time(Object, Time) :-
		findall(
			WallTime,
			(	message_cache_(test(Object, _, passed_test(_, _, _, _, WallTime)))
			;	message_cache_(test(Object, _, failed_test(_, _, _, _, _, WallTime)))
			),
			WallTimes
		),
		sum(WallTimes, 0, Time).

	assembly_stats(Object, Total, Total, 0, 0) :-
		message_cache_(tests_skipped(Object, _File, _Note)),
		!,
		Object::number_of_tests(Total).
	assembly_stats(Object, Total, Skipped, Passed, Failed) :-
		message_cache_(tests_results_summary(Object, Total, Skipped, Passed, Failed, _Flaky, _Note)),
		!.

	assembly_config_file(Object, Name) :-
		once(message_cache_(running_tests_from_object_file(Object, File))),
		suppress_path_prefix(File, Name).

	assembly_name(Object, Name::Object) :-
		once(message_cache_(running_tests_from_object_file(Object, File))),
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, Name, File) ->
			true
		;	Name = File
		).

	assembly_run_date(RunDate) :-
		message_cache_(tests_start_date_time(Year, Month, Day, _, _, _)),
		integers_to_atoms([Year,Month,Day], [AYear,AMonth0,ADay0]),
		pad_single_char_atoms([AMonth0,ADay0], [AMonth,ADay]),
		atomic_list_concat([AYear,'-',AMonth,'-',ADay], RunDate).

	assembly_run_time(RunTime) :-
		message_cache_(tests_start_date_time(_, _, _, Hours, Minutes, Seconds)),
		integers_to_atoms([Hours,Minutes,Seconds], [AHours0,AMinutes0,ASeconds0]),
		pad_single_char_atoms([AHours0,AMinutes0,ASeconds0], [AHours,AMinutes,ASeconds]),
		atomic_list_concat([AHours,':',AMinutes,':',ASeconds], RunTime).

	% "collection" tag attributes

	collection_stats(Object, Total, Skipped, Passed, Failed) :-
		assembly_stats(Object, Total, Skipped, Passed, Failed).

	collection_name(Object, Name) :-
		assembly_name(Object, Name).

	collection_time(Object, Time) :-
		assembly_time(Object, Time).

	% date and time auxiliary predicates

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

	suppress_path_prefix(Path, ShortPath) :-
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, ShortPath, Path) ->
			true
		;	ShortPath = Path
		).

	% write test element auxiliary predicates

	write_test_element_traits(Short, Position, Note) :-
		write_xml_open_tag(traits, []),
		write_xml_empty_tag(trait, [name-file, value-Short]),
		write_xml_empty_tag(trait, [name-position, value-Position]),
		(	tests_url(Short, Position, URL) ->
			write_xml_empty_tag(trait, [name-url, value-URL])
		;	true
		),
		(	Note \== '' ->
			write_xml_empty_tag(property, [name-note, value-Note])
		;	true
		),
		write_xml_close_tag(traits).

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
