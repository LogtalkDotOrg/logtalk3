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
		version is 2:2:0,
		author is 'Paulo Moura',
		date is 2026-11-05,
		comment is 'Portable process handling predicates.',
		remarks is [
			'Supported backend Prolog systems' - 'ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog, Trealla Prolog, and XVM.',
			'Option ``type/1`` limitation' - 'This option is ignored on ECLiPSe, GNU Prolog, and XVM.'
		],
		see_also is [os]
	]).

	:- public(create/3).
	:- mode(create(+atom, +list(atom), +list(compound)), zero_or_one).
	:- info(create/3, [
		comment is 'Creates a new process from the given executable and list of arguments. Supported options are ``process(Pid)``, ``stdin(Stream)``, ``stdout(Stream)``, ``stderr(Stream)``, and ``type(Type)``.',
		argnames is ['Executable', 'Arguments', 'Options']
	]).

	:- public(wait/2).
	:- mode(wait(+process_or_pid, -integer_or_compound), one).
	:- info(wait/2, [
		comment is 'Waits for a process to terminate and retrieves its exit status. The value of ``Status`` depends on the backend. For a successfully terminated process, ``Status`` is either ``0`` or ``exit(0)``.',
		argnames is ['Process', 'Status']
	]).

	:- public(wait/3).
	:- mode(wait(+process_or_pid, -integer_or_compound, -compound), zero_or_one_or_error).
	:- info(wait/3, [
		comment is 'Waits once for a process to terminate, returning the backend-specific status in ``RawStatus`` and its normalized form in ``NormalizedStatus``.',
		argnames is ['Process', 'RawStatus', 'NormalizedStatus'],
		remarks is [
			'Normalized status' - 'Either ``exit(Code)``, ``killed(Signal)``, or ``unknown(RawStatus)`` when native information is insufficient.',
			'GNU Prolog limitation' - 'Nonzero POSIX integer statuses can be ambiguous. Native waiting errors, including those for exit codes 126 and 127, are propagated.'
		],
		fails_if is 'The backend wait predicate fails.',
		exceptions is [
			'Process is a variable' - instantiation_error,
			'Process is not an integer on a backend using integer process identifiers' - type_error(integer, 'Process'),
			'Process is not a valid backend process reference' - domain_error(process, 'Process'),
			'Process does not identify a child process' - existence_error(process, 'Process'),
			'Waiting for the process raises an operating system error' - system_error('Error')
		]
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

		create(Executable, Arguments, Options) :-
			build_streams_list(Options, Streams),
			(	{member(process(Pid), Options)} ->
				{exec([Executable| Arguments], Streams, Pid)}
			;	{exec([Executable| Arguments], Streams, _)}
			),
			bind_streams(Options, Streams).

		build_streams_list(Options, [Stdin, Stdout, Stderr]) :-
			({member(stdin(_), Options)} -> true ; Stdin = null),
			({member(stdout(_), Options)} -> true ; Stdout = null),
			({member(stderr(_), Options)} -> true ; Stderr = null).

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
			(	{getenv('COMSPEC', _)} ->
				{integer_atom(Pid, PidAtom)},
				atom_concat('taskkill /F /PID ', PidAtom, Command),
				ignore({sh(Command)})
			;	atom(Signal) ->
				signal_number(Signal, Number),
				{kill(Pid, Number)}
			;	{kill(Pid, Signal)}
			).

		kill(Pid) :-
			(	{getenv('COMSPEC', _)} ->
				{integer_atom(Pid, PidAtom)},
				atom_concat('taskkill /F /PID ', PidAtom, Command),
				ignore({sh(Command)})
			;	{kill(Pid, 9)}
			).

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
			(	{environ('COMSPEC', _)} ->
				{write_to_atom(PidAtom, Pid)},
				atom_concat('taskkill /F /PID ', PidAtom, Command),
				{shell(Command, _)}
			;	atom(Signal) ->
				signal_number(Signal, Number),
				{send_signal(Pid, Number)}
			;	{send_signal(Pid, Signal)}
			).

		kill(Pid) :-
			(	{environ('COMSPEC', _)} ->
				{write_to_atom(PidAtom, Pid)},
				atom_concat('taskkill /F /PID ', PidAtom, Command),
				{shell(Command, _)}
			;	{send_signal(Pid, 9)}
			).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		create(Executable, Arguments, Options) :-
			(	{absolute_file_name(Executable, Executable)} ->
				Path = Executable
			;	Path = path(Executable)
			),
			translate_options(Options, Options, NativeOptions),
			{process:process_create(Path, Arguments, NativeOptions)}.

		translate_options([], _, []).
		translate_options([Option| Options], AllOptions, NativeOptions) :-
			(	translate_option(Option, AllOptions, NativeOption) ->
				NativeOptions = [NativeOption| NativeOptionsTail]
			;	NativeOptions = NativeOptionsTail
			),
			translate_options(Options, AllOptions, NativeOptionsTail).

		translate_option(process(Pid), _, process(Pid)).
		translate_option(stdin(Stream), Options, NativeOption) :-
			(	{member(type(Type), Options)} ->
				NativeOption = stdin(pipe(Stream, [type(Type)]))
			;	NativeOption = stdin(pipe(Stream))
			).
		translate_option(stdout(Stream), Options, NativeOption) :-
			(	{member(type(Type), Options)} ->
				NativeOption = stdout(pipe(Stream, [type(Type)]))
			;	NativeOption = stdout(pipe(Stream))
			).
		translate_option(stderr(Stream), _, stderr(pipe(Stream))).

		wait(Process, Status) :-
			{process:process_wait(Process, Status)}.

		kill(Process, Signal) :-
			(	{environ('COMSPEC', _)} ->
				catch({process:process_kill(Process)}, _, true)
			;	atom(Signal) ->
				signal_number(Signal, Number),
				{process:process_kill(Process, Number)}
			;	{process:process_kill(Process, Signal)}
			).

		kill(Process) :-
			{process:process_kill(Process)}.

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		create(Executable, Arguments, Options) :-
			(	{is_absolute_file_name(Executable)} ->
				Path = Executable
			;	Path = path(Executable)
			),
			translate_options(Options, Options, NativeOptions),
			{process:process_create(Path, Arguments, NativeOptions)}.

		translate_options([], _, []).
		translate_options([Option| Options], AllOptions, NativeOptions) :-
			(	translate_option(Option, AllOptions, NativeOption) ->
				NativeOptions = [NativeOption| NativeOptionsTail]
			;	NativeOptions = NativeOptionsTail
			),
			translate_options(Options, AllOptions, NativeOptionsTail).

		translate_option(process(Pid), _, process(Pid)).
		translate_option(stdin(Stream), Options, NativeOption) :-
			(	{member(type(Type), Options)} ->
				NativeOption = stdin(pipe(Stream, [type(Type)]))
			;	NativeOption = stdin(pipe(Stream))
			).
		translate_option(stdout(Stream), Options, NativeOption) :-
			(	{member(type(Type), Options)} ->
				NativeOption = stdout(pipe(Stream, [type(Type)]))
			;	NativeOption = stdout(pipe(Stream))
			).
		translate_option(stderr(Stream), _, stderr(pipe(Stream))).

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

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

		create(Executable, Arguments, Options) :-
			translate_options(Options, Options, NativeOptions),
			{process_create(Executable, Arguments, NativeOptions)}.

		translate_options([], _, []).
		translate_options([Option| Options], AllOptions, NativeOptions) :-
			(	translate_option(Option, AllOptions, NativeOption) ->
				NativeOptions = [NativeOption| NativeOptionsTail]
			;	NativeOptions = NativeOptionsTail
			),
			translate_options(Options, AllOptions, NativeOptionsTail).

		translate_option(process(Pid), _, process(Pid)).
		translate_option(stdin(Stream), Options, NativeOption) :-
			(	{member(type(Type), Options)} ->
				NativeOption = stdin(pipe(Stream, [type(Type)]))
			;	NativeOption = stdin(pipe(Stream))
			).
		translate_option(stdout(Stream), Options, NativeOption) :-
			(	{member(type(Type), Options)} ->
				NativeOption = stdout(pipe(Stream, [type(Type)]))
			;	NativeOption = stdout(pipe(Stream))
			).
		translate_option(stderr(Stream), _, stderr(pipe(Stream))).

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

	:- elif(current_logtalk_flag(prolog_dialect, xvm)).

		create(Executable, Arguments, Options) :-
			translate_options(Options, NativeOptions),
			{process_create(Executable, Arguments, NativeOptions)}.

		translate_options([], []).
		translate_options([Option| Options], NativeOptions) :-
			(	translate_option(Option, NativeOption) ->
				NativeOptions = [NativeOption| NativeOptionsTail]
			;	NativeOptions = NativeOptionsTail
			),
			translate_options(Options, NativeOptionsTail).

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
		translate_options([Option| Options], NativeOptions) :-
			(	translate_option(Option, NativeOption) ->
				NativeOptions = [NativeOption| NativeOptionsTail]
			;	NativeOptions = NativeOptionsTail
			),
			translate_options(Options, NativeOptionsTail).

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

	wait(Process, RawStatus, NormalizedStatus) :-
		wait(Process, RawStatus),
		normalize_status(RawStatus, NormalizedStatus).

	normalize_status(exit(Code), ExitStatus) :-
		integer(Code),
		!,
		ExitStatus = exit(Code).
	normalize_status(killed(Signal), ExitStatus) :-
		ground(Signal),
		!,
		ExitStatus = killed(Signal).
	:- if(current_logtalk_flag(prolog_dialect, xvm)).
		normalize_status(signal(Signal), ExitStatus) :-
			integer(Signal),
			!,
			ExitStatus = killed(Signal).
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		normalize_status(Status, ExitStatus) :-
			integer(Status),
			Status >= 0,
			!,
			(	{getenv('COMSPEC', _)} ->
				ExitStatus = exit(Status)
			;	LowByte is Status mod 256,
				Signal is Status mod 128,
				(	LowByte =:= 0 ->
					Code is (Status // 256) mod 256,
					ExitStatus = exit(Code)
				;	Signal > 0,
					Signal < 127 ->
					ExitStatus = killed(Signal)
				;	ExitStatus = unknown(Status)
				)
			).
	:- elif(current_logtalk_flag(prolog_dialect, gnu)).
		normalize_status(Status, ExitStatus) :-
			integer(Status),
			Status >= 0,
			!,
			ExitStatus = exit(Status).
	:- endif.
	normalize_status(Status, unknown(Status)).

	signal_number(sighup,   1).
	signal_number(sigint,   2).
	signal_number(sigkill,  9).
	signal_number(sigterm, 15).

:- end_object.
