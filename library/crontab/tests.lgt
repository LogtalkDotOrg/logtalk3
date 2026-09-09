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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-09,
		comment is 'Tests for the "crontab" library.'
	]).

	cover(crontab).
	cover(crontab(_)).

	setup :-
		^^clean_file('test_files/output.crontab').

	test(crontab_parse_atom_default, deterministic(Entries == [
		comment(' jobs'),
		env('SHELL', '/bin/sh'),
		entry(time(0, 2, '*', '*', '*'), '/usr/local/bin/backup', none)
	])) :-
		crontab::parse(atom('# jobs\nSHELL=/bin/sh\n0 2 * * * /usr/local/bin/backup\n'), Entries).

	test(crontab_parse_chars_representation, deterministic(Entries == [
		entry(time(step('*', 15), '*', '*', '*', mon), [e,c,h,o], none)
	])) :-
		crontab(chars)::parse(atom('*/15 * * * MON echo\n'), Entries).

	test(crontab_parse_codes_representation, deterministic(Entries == [
		comment([32,106,111,98,115]),
		env([80,65,84,72], [47,98,105,110])
	])) :-
		crontab(codes)::parse(atom('# jobs\nPATH=/bin\n'), Entries).

	test(crontab_parse_system, deterministic(Entries == [
		user_entry(root, time(0, '*', '*', '*', sun), run, none)
	])) :-
		crontab::parse(atom('0 * * * SUN root run\n'), Entries, [format(system)]).

	test(crontab_parse_percent_input, deterministic(Entries == [
		entry(time('*', '*', '*', '*', '*'), 'mail % report', stdin('line one\nline two'))
	])) :-
		crontab::parse(atom('* * * * * mail \\% report%line one%line two\n'), Entries).

	test(crontab_parse_macro_alias, deterministic(Entries == [
		entry(special(daily), run, none)
	])) :-
		crontab::parse(atom('@MIDNIGHT run\n'), Entries).

	test(crontab_parse_system_macro_user, deterministic(Entries == [
		user_entry('backup-user', special(reboot), run, none)
	])) :-
		crontab::parse(atom('@reboot backup-user run\n'), Entries, [format(system)]).

	test(crontab_generate_atom, deterministic(Atom == '0 2 * * * /usr/local/bin/backup\n')) :-
		crontab::generate(atom(Atom), [
			entry(time(0, 2, '*', '*', '*'), '/usr/local/bin/backup', none)
		]).

	test(crontab_generate_chars_to_codes_sink, deterministic(Codes == [35,32,106,111,98,115,10])) :-
		crontab(chars)::generate(codes(Codes), [comment([' ',j,o,b,s])]).

	test(crontab_generate_macro, deterministic(Atom == '@daily run\n')) :-
		crontab::generate(atom(Atom), [entry(special(daily), run, none)]).

	test(crontab_generate_environment_boundary_spaces, deterministic(Atom == 'NAME=" value "\n')) :-
		crontab::generate(atom(Atom), [env('NAME', ' value ')]).

	test(crontab_environment_quotes_round_trip, deterministic(Parsed == [env('NAME', ' both " and '' quotes ')])) :-
		crontab::generate(atom(Atom), [env('NAME', ' both " and '' quotes ')]),
		crontab::parse(atom(Atom), Parsed).

	test(crontab_round_trip, deterministic(Parsed == Entries)) :-
		Entries = [
			blank,
			env('MAILTO', ''),
			entry(time(list([1, 3, 5]), range(9, 17), '*', jan, mon), 'echo 100%', stdin('a\nb'))
		],
		crontab::generate(atom(Atom), Entries),
		crontab::parse(atom(Atom), Parsed).

	test(crontab_parse_user_file, deterministic) :-
		^^file_path('test_files/user.crontab', Path),
		crontab::parse(file(Path), Entries),
		Entries = [
			comment(' User crontab'),
			env('SHELL', '/bin/sh'),
			env('MAILTO', ' ops@example.com '),
			blank,
			entry(time(step('*', 15), range(9, 17), '*', jan, range(mon, fri)), 'echo weekday', none),
			entry(special(daily), '/usr/local/bin/backup', none),
			entry(time('*', '*', '*', '*', '*'), 'mail % report', stdin('first line\nsecond line'))
		].

	test(crontab_parse_system_stream, deterministic) :-
		^^file_path('test_files/system.crontab', Path),
		parse_system_stream(Path, Entries),
		Entries = [
			comment(' System crontab'),
			env('PATH', '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin'),
			blank,
			user_entry(root, time(0, '*', '*', '*', '*'), 'run-parts /etc/cron.hourly', none),
			user_entry('backup-user', special(reboot), '/usr/local/bin/start-backup', none)
		].

	test(crontab_generate_file_round_trip, deterministic(Parsed == Entries)) :-
		^^file_path('test_files/output.crontab', Path),
		Entries = [env('PATH', '/bin'), entry(special(hourly), run, none)],
		crontab::generate(file(Path), Entries),
		crontab::parse(file(Path), Parsed).

	test(crontab_entry_valid, true) :-
		crontab(codes)::entry(entry(time(0, 0, 1, 1, 0), [114,117,110], none)).

	test(crontab_entry_wrong_representation, false) :-
		crontab(codes)::entry(comment(atom)).

	test(crontab_default_options, deterministic(Options == [format(user)])) :-
		crontab::default_options(Options).

	test(crontab_valid_option, true) :-
		crontab::valid_option(format(system)).

	test(crontab_invalid_options_variable, error(instantiation_error)) :-
		crontab::parse(atom(''), _, _).

	test(crontab_invalid_options_type, error(type_error(list, format(user)))) :-
		crontab::parse(atom(''), _, format(user)).

	test(crontab_invalid_option, error(domain_error(option, unknown(value)))) :-
		crontab::parse(atom(''), _, [unknown(value)]).

	test(crontab_invalid_field, error(syntax_error(crontab(1, invalid_line)))) :-
		crontab::parse(atom('60 * * * * run\n'), _).

	test(crontab_invalid_line_ending_number, error(syntax_error(crontab(2, invalid_line_ending)))) :-
		crontab::parse(codes([35,32,111,107,10,98,97,100,13,108,105,110,101]), _).

	test(crontab_invalid_representation, error(domain_error(crontab_representation, string))) :-
		crontab(string)::parse(atom(''), _).

	% auxiliary predicates

	parse_system_stream(Path, Entries) :-
		open(Path, read, Stream),
		catch(
			( crontab::parse(stream(Stream), Entries, [format(system)]), at_end_of_stream(Stream) ),
			Error,
			(catch(close(Stream), _, true), throw(Error))
		),
		close(Stream).

:- end_object.
