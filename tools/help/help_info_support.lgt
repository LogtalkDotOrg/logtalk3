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


:- category(help_info_support,
	complements(help)).

	:- info([
		version is 0:8:1,
		author is 'Paulo Moura',
		date is 2024-04-08,
		comment is 'Experimental help predicates for inline browsing of the Texinfo versions of the Handbook and APIs documentation. Currently requires Ciao Prolog, ECLiPSe, GNU Prolog, LVM, SICStus Prolog, SWI-Prolog, Trealla Prolog, XSB, or YAP as the backend running on a POSIX system.'
	]).

	:- public(handbook/0).
	:- mode(handbook, one).
	:- info(handbook/0, [
		comment is 'Opens inline the Texinfo version of the Handbook.'
	]).

	:- public(handbook/1).
	:- mode(handbook(+atom), one).
	:- mode(handbook(+predicate_indicator), one).
	:- mode(handbook(+non_terminal_indicator), one).
	:- info(handbook/1, [
		comment is 'Opens inline the Texinfo version of the Handbook at the given topic.',
		argnames is ['Topic']
	]).

	:- public(apis/0).
	:- mode(apis, one).
	:- info(apis/0, [
		comment is 'Opens inline the Texinfo version of the APIs documentation.'
	]).

	:- public(apis/1).
	:- mode(apis(+atom), one).
	:- mode(apis(+predicate_indicator), one).
	:- mode(apis(+non_terminal_indicator), one).
	:- info(apis/1, [
		comment is 'Opens inline the Texinfo version of the APIs documentation at the given topic.',
		argnames is ['Topic']
	]).

	:- public(man/1).
	:- mode(man(+atom), one).
	:- info(man/1, [
		comment is 'Opens inline the man page of the given script.',
		argnames is ['Script']
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	help :-
		@help,
		write('Experimental features: assuming the Handbook and APIs documentation Texinfo'), nl,
		write('files, the man files, and the info command are available, the following goals'), nl,
		write('can be used for inline browsing of the Texinfo and man files:'), nl, nl,
		write('    help::handbook.               help::apis.               help::man(Script).'), nl,
		write('    help::handbook(Topic).        help::apis(Topic).'), nl,
		write('    help::handbook(Name/Arity).   help::apis(Name/Arity).'), nl,
		write('    help::handbook(Name//Arity).  help::apis(Name//Arity).'), nl, nl.

	handbook :-
		info_executable(Info),
		handbook_file(File),
		process_create(Info, ['-f', File]).

	handbook(Topic) :-
		topic_to_atom(Topic, TopicAtom),
		info_executable(Info),
		handbook_file(File),
		process_create(Info, ['-f', File, '-n', TopicAtom]).

	apis :-
		info_executable(Info),
		apis_file(File),
		process_create(Info, ['-f', File]).

	apis(Topic) :-
		topic_to_atom(Topic, TopicAtom),
		info_executable(Info),
		apis_file(File),
		process_create(Info, ['-f', File, '-n', TopicAtom]).

	man(Script) :-
		info_executable(Info),
		process_create(Info, ['-a', Script]).

	topic_to_atom(Topic, TopicAtom) :-
		(	atom(Topic) ->
			TopicAtom = Topic
		;	Topic = Name/Arity, atom(Name), integer(Arity) ->
			atomic_list_concat([Name, '/', Arity], TopicAtom)
		;	Topic = Name//Arity, atom(Name), integer(Arity) ->
			atomic_list_concat([Name, '//', Arity], TopicAtom)
		;	write('Starting node must be an atom, a predicate indicator, or a non-terminal indicator!'), nl,
			fail
		).

	info_executable('/usr/bin/info') :-
		os::file_exists('/usr/bin/info'),
		!.
	info_executable('/usr/local/bin/info') :-
		os::file_exists('/usr/local/bin/info'),
		!.
	info_executable('/opt/local/bin/info') :-
		os::file_exists('/opt/local/bin/info'),
		!.
	info_executable(_) :-
		write('The info command-line executable was not found!'), nl,
		fail.

	handbook_file(File) :-
		this(This),
		object_property(This, file(_, Directory)),
		current_logtalk_flag(version_data, logtalk(Major,Minor,Patch,_)),
		atomic_list_concat([Directory, '../../manuals/TheLogtalkHandbook-',Major,'.',Minor,'.',Patch,'.info'], File).

	apis_file(File) :-
		this(This),
		object_property(This, file(_, Directory)),
		current_logtalk_flag(version_data, logtalk(Major,Minor,Patch,_)),
		atomic_list_concat([Directory, '../../docs/LogtalkAPIs-',Major,'.',Minor,'.',Patch,'.info'], File).

	:- if(current_logtalk_flag(prolog_dialect, ciao)).

		{:- use_module(library(process))}.
		process_create(Process, Arguments) :-
			{	current_output(Output),
				process_call(Process, Arguments, [stdout(stream(Output))])
			}.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		process_create(Process, Arguments) :-
			atomic_list_concat([Process| Arguments], ' ', Command),
			{exec(Command, [])}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		process_create(Process, Arguments) :-
			atomic_list_concat([Process| Arguments], ' ', Command),
			{exec(Command, user_input, user_output, user_error)}.

	:- elif(current_logtalk_flag(prolog_dialect, lvm)).

		process_create(Process, Arguments) :-
			{process_create(Process, Arguments, [stdout(std)])}.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		process_create(Process, Arguments) :-
			{process_create(Process, Arguments, [stdin(std), stdout(std), stderr(std), wait(_)])}.

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		process_create(Process, Arguments) :-
			{process_create(Process, Arguments, [stdout(std)])}.

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

		process_create(Process, Arguments) :-
			{process_create(Process, Arguments, [stdout(std)])}.

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		process_create(Process, Arguments) :-
			atomic_list_concat([Process| Arguments], ' ', Command),
			{	current_output(Output),
				shell(Command, none, Output, none, _)
			}.

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		process_create(Process, Arguments) :-
			atomic_list_concat([Process| Arguments], ' ', Command),
			{exec(Command, [std,std,std], _)}.

	:- endif.

:- end_category.
