%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(process).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-04,
		comment is 'Portable process handling predicates.',
		remarks is [
			'Supported backend Prolog systems' - 'ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog, Trealla Prolog, and XVM.'
		],
		see_also is [os]
	]).

	:- public(create/3).
	:- mode(create(+atom, +list(atom), +list(compound)), zero_or_one).
	:- info(create/3, [
		comment is 'Creates a new process from the given executable and list of arguments. Supported options are ``process(Pid)``, ``stdin(Stream)``, ``stdout(Stream)``, and ``stderr(Stream)``.',
		argnames is ['Executable', 'Arguments', 'Options']
	]).

	:- public(wait/2).
	:- mode(wait(+process_or_pid, -integer), zero_or_one).
	:- info(wait/2, [
		comment is 'Waits for a process to terminate and retrieves its exit status.',
		argnames is ['Process', 'Status']
	]).

	:- public(kill/2).
	:- mode(kill(+process_or_pid, +atom_or_integer), zero_or_one).
	:- info(kill/2, [
		comment is 'Kills the given process with the specified signal (an integer or one of the following atoms: ``sighup``, ``sigint``, ``sigkill``, or ``sigterm``).',
		argnames is ['Process', 'Signal']
	]).

	:- public(kill/1).
	:- mode(kill(+process_or_pid), zero_or_one).
	:- info(kill/1, [
		comment is 'Kills the given process using the default signal (``sigkill``).',
		argnames is ['Process']
	]).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

		:- uses(user, [
			member/2
		]).

		create(Executable, Arguments, Options) :-
			build_streams_list(Options, Streams),
			(	member(process(Pid), Options) ->
				{exec([Executable| Arguments], Streams, Pid)}
			;	{exec([Executable| Arguments], Streams, _)}
			),
			bind_streams(Options, Streams).

		build_streams_list(Options, [Stdin, Stdout, Stderr]) :-
			(member(stdin(_), Options) -> true ; Stdin = null),
			(member(stdout(_), Options) -> true ; Stdout = null),
			(member(stderr(_), Options) -> true ; Stderr = null).

		bind_streams([], _).
		bind_streams([Option| Options], [Stdin, Stdout, Stderr]) :-
			(	Option = stdin(Stdin) ->
				true
			;	Option = stdout(Stdout) ->
				true
			;	Option = stderr(Stderr) ->
				true
			;	true
			),
			bind_streams(Options, [Stdin, Stdout, Stderr]).

		wait(Pid, Status) :-
			{wait(Pid, Status)}.

		kill(Pid, Signal) :-
			(	atom(Signal) ->
				signal_number(Signal, Number),
				{kill(Pid, Number)}
			;	{kill(Pid, Signal)}
			).

		kill(Pid) :-
			{kill(Pid, 9)}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		:- uses(user, [
			atomic_list_concat/3, member/2
		]).

		create(Executable, Arguments, Options) :-
			atomic_list_concat([Executable| Arguments], ' ', Command),
			(	member(process(Pid), Options) ->
				{exec(Command, Stdin, Stdout, Stderr, Pid)}
			;	{exec(Command, Stdin, Stdout, Stderr)}
			),
			bind_streams_gnu(Options, Stdin, Stdout, Stderr).

		bind_streams_gnu([], _, _, _).
		bind_streams_gnu([Option| Options], Stdin, Stdout, Stderr) :-
			(	Option = stdin(Stdin) ->
				true
			;	Option = stdout(Stdout) ->
				true
			;	Option = stderr(Stderr) ->
				true
			;	true
			),
			bind_streams_gnu(Options, Stdin, Stdout, Stderr).

		wait(Pid, Status) :-
			{wait(Pid, Status)}.

		kill(Pid, Signal) :-
			(	atom(Signal) ->
				signal_number(Signal, Number),
				{send_signal(Pid, Number)}
			;	{send_signal(Pid, Signal)}
			).

		kill(Pid) :-
			{send_signal(Pid, 9)}.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		create(Executable, Arguments, Options) :-
			translate_options(Options, NativeOptions),
			{process:process_create(Executable, Arguments, NativeOptions)}.

		translate_options([], []).
		translate_options([Option| Options], [NativeOption| NativeOptions]) :-
			translate_option(Option, NativeOption),
			translate_options(Options, NativeOptions).

		translate_option(process(Pid), process(Pid)).
		translate_option(stdin(Stream), stdin(pipe(Stream))).
		translate_option(stdout(Stream), stdout(pipe(Stream))).
		translate_option(stderr(Stream), stderr(pipe(Stream))).

		wait(Process, Status) :-
			{process:process_wait(Process, Status)}.

		kill(Process, Signal) :-
			(	atom(Signal) ->
				signal_number(Signal, Number),
				{process:process_kill(Process, Number)}
			;	{process:process_kill(Process, Signal)}
			).

		kill(Process) :-
			{process:process_kill(Process)}.

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

		create(Executable, Arguments, Options) :-
			translate_options(Options, NativeOptions),
			{process_create(Executable, Arguments, NativeOptions)}.

		translate_options([], []).
		translate_options([Option| Options], [NativeOption| NativeOptions]) :-
			translate_option(Option, NativeOption),
			translate_options(Options, NativeOptions).

		translate_option(process(Pid), process(Pid)).
		translate_option(stdin(Stream), stdin(pipe(Stream))).
		translate_option(stdout(Stream), stdout(pipe(Stream))).
		translate_option(stderr(Stream), stderr(pipe(Stream))).

		% Trealla's process_wait/2 takes options as second argument (input),
		% not a status output. process_wait/1 just waits for the process.
		% Since Trealla doesn't provide exit status, we return exit(0)
		% assuming successful completion.
		wait(Pid, Status) :-
			{process_wait(Pid)},
			Status = exit(0).

		kill(Pid, Signal) :-
			(	atom(Signal) ->
				signal_number(Signal, Number),
				{process_kill(Pid, Number)}
			;	{process_kill(Pid, Signal)}
			).

		kill(Pid) :-
			{process_kill(Pid)}.

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		create(Executable, Arguments, Options) :-
			translate_options(Options, NativeOptions),
			{process:process_create(Executable, Arguments, NativeOptions)}.

		translate_options([], []).
		translate_options([Option| Options], [NativeOption| NativeOptions]) :-
			translate_option(Option, NativeOption),
			translate_options(Options, NativeOptions).

		translate_option(process(Pid), process(Pid)).
		translate_option(stdin(Stream), stdin(pipe(Stream))).
		translate_option(stdout(Stream), stdout(pipe(Stream))).
		translate_option(stderr(Stream), stderr(pipe(Stream))).

		wait(Pid, Status) :-
			{process:process_wait(Pid, Status)}.

		kill(Pid, Signal) :-
			(	atom(Signal) ->
				signal_number(Signal, Number),
				{process:process_kill(Pid, Number)}
			;	{process:process_kill(Pid, Signal)}
			).

		kill(Pid) :-
			{process:process_kill(Pid)}.

	:- elif(current_logtalk_flag(prolog_dialect, xvm)).

		create(Executable, Arguments, Options) :-
			translate_options(Options, NativeOptions),
			{process_create(Executable, Arguments, NativeOptions)}.

		translate_options([], []).
		translate_options([Option| Options], [NativeOption| NativeOptions]) :-
			translate_option(Option, NativeOption),
			translate_options(Options, NativeOptions).

		translate_option(process(Pid), process(Pid)).
		translate_option(stdin(Stream), stdin(pipe(Stream))).
		translate_option(stdout(Stream), stdout(pipe(Stream))).
		translate_option(stderr(Stream), stderr(pipe(Stream))).

		wait(Pid, Status) :-
			{process_wait(Pid, Status)}.

		kill(Pid, Signal) :-
			(	atom(Signal) ->
				signal_number(Signal, Number),
				{process_kill(Pid, Number)}
			;	{process_kill(Pid, Signal)}
			).

		kill(Pid) :-
			{process_kill(Pid)}.

	:- elif(current_logtalk_flag(prolog_dialect, ciao)).

		create(Executable, Arguments, Options) :-
			translate_options(Options, NativeOptions),
			{process:process_call(Executable, Arguments, NativeOptions)}.

		translate_options([], []).
		translate_options([Option| Options], [NativeOption| NativeOptions]) :-
			translate_option(Option, NativeOption),
			translate_options(Options, NativeOptions).

		translate_option(process(Pid), process(Pid)).
		translate_option(stdin(Stream), stdin(pipe(Stream))).
		translate_option(stdout(Stream), stdout(pipe(Stream))).
		translate_option(stderr(Stream), stderr(pipe(Stream))).

		wait(Pid, Status) :-
			{system:wait(Pid, Status)}.

		kill(Pid) :-
			{process:process_send_signal(Pid, 9)}.

		kill(Pid, Signal) :-
			(	atom(Signal) ->
				signal_number(Signal, Number),
				{process:process_send_signal(Pid, Number)}
			;	{process:process_send_signal(Pid, Signal)}
			).

	:- endif.

	signal_number(sighup,   1).
	signal_number(sigint,   2).
	signal_number(sigkill,  9).
	signal_number(sigterm, 15).

:- end_object.
