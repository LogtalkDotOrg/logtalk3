%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(problog_process).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-12,
		comment is 'Isolated operating-system shell transport for the ProbLog CLI.'
	]).

	:- public(quote/3).
	:- mode(quote(+atom, +atom, -atom), one_or_error).
	:- info(quote/3, [
		comment is 'Quotes an atom as one POSIX shell or Windows PowerShell literal, without interpolation.',
		argnames is ['Platform', 'Argument', 'Quoted'],
		exceptions is [
			'``Platform`` is a variable' - instantiation_error,
			'``Platform`` is neither a variable nor an atom' - type_error(atom, 'Platform'),
			'``Platform`` is an atom but neither ``posix`` nor ``windows``' - domain_error(problog_shell_platform, 'Platform'),
			'``Argument`` is a variable' - instantiation_error,
			'``Argument`` is neither a variable nor an atom' - type_error(atom, 'Argument'),
			'``Argument`` is an atom but contains a null character' - domain_error(problog_shell_argument, 'Argument')
		]
	]).

	:- public(run/7).
	:- mode(run(+atom, +list(atom), +compound, -list(code), -list(code), -term, -compound), one_or_error).
	:- info(run/7, [
		comment is 'Runs the ProbLog CLI with an owned temporary source and separate UTF-8 output captures, returning only after cleanup.',
		argnames is [
			'Executable', 'Arguments', 'ProgramSource', 'Codes', 'Diagnostics', 'RawStatus',
			'NormalizedStatus'
		],
		exceptions is [
			'``Executable`` is a variable' - instantiation_error,
			'``Executable`` is neither a variable nor an atom' - type_error(atom, 'Executable'),
			'``Arguments`` is a partial list or a list with an element ``Argument`` which is a variable' - instantiation_error,
			'``Arguments`` is neither a variable nor a list' - type_error(list(atom), 'Arguments'),
			'An element ``Argument`` of the list ``Arguments`` is neither a variable nor an atom' - type_error(atom, 'Argument'),
			'``Executable`` or an element ``Argument`` of the list ``Arguments`` contains a null character' - domain_error(problog_shell_argument, 'Argument'),
			'``ProgramSource`` is not a ground term' - instantiation_error,
			'``ProgramSource`` is neither ``file(Path)`` nor ``codes(Codes)``' - domain_error(problog_process_source, 'ProgramSource'),
			'A launcher dependency does not exist' - existence_error(command, 'Command'),
			'A source or capture file does not exist' - existence_error(source_sink, 'Path'),
			'A source or capture file cannot be opened' - permission_error(open, source_sink, 'Path'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - problog_transport('Stage', 'Status', 'Details')
		]
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(list, [
		member/2, append/3
	]).

	:- uses(os, [
		absolute_file_name/2, delete_directory_and_contents/1, directory_exists/1, file_exists/1,
		operating_system_type/1, resolve_command_path/2, temporary_directory/1, working_directory/1
	]).

	:- uses(process, [
		create/3, kill/1, wait/2, wait/3
	]).

	:- uses(reader, [
		stream_to_bytes/2, stream_to_codes/2, file_to_bytes/2
	]).

	:- uses(utf_8, [
		bytes_to_codes/2, codes_to_bytes/2
	]).

	run(Executable, Arguments, ProgramSource, Codes, Diagnostics, RawStatus, NormalizedStatus) :-
		context(Context),
		check(ground, Executable-Arguments-ProgramSource, Context),
		check(atom, Executable, Context),
		check(list(atom), Arguments, Context),
		quote(posix, Executable, _),
		forall(member(Argument, Arguments), quote(posix, Argument, _)),
		check_program_source(ProgramSource),
		platform(Platform),
		create_workspace(Platform, Directory),
		catch(
			(	run_in_workspace(Platform, Directory, Executable, Arguments, ProgramSource, Codes, Diagnostics, RawStatus, NormalizedStatus) ->
					Outcome = success
			;	transport_error(execution, unknown, [])
			),
			Error,
			Outcome = error(Error)
		),
		catch(remove_workspace(Directory), CleanupError, Cleanup = error(CleanupError)),
		finish_run(Outcome, Cleanup, Codes, Diagnostics, RawStatus, NormalizedStatus).

	check_program_source(file(Path)) :-
		!,
		context(Context),
		check(atom, Path, Context),
		quote(posix, Path, _).
	check_program_source(codes(Codes)) :-
		!,
		context(Context),
		check(codes, Codes, Context).
	check_program_source(ProgramSource) :-
		domain_error(problog_process_source, ProgramSource).

	finish_run(success, Cleanup, _, _, _, _) :-
		(	var(Cleanup) ->
			true
		;	Cleanup = error(Error),
			transport_error(cleanup, unknown, Error)
		).
	finish_run(error(Error), Cleanup, _, _, _, _) :-
		(	var(Cleanup) ->
			throw(Error)
		;	Cleanup = error(CleanupError),
			transport_error(cleanup, unknown, operation_error(Error, CleanupError))
		).

	platform(Platform) :-
		(	operating_system_type(windows) ->
			Platform = windows
		;	Platform = posix
		).

	create_workspace(Platform, Directory) :-
		temporary_directory(Temporary),
		absolute_file_name(Temporary, Base),
		allocation_command(Platform, Base, Shell, Arguments),
		control_process(Shell, Arguments, Bytes, Errors, _, Status),
		(	Status == exit(0),
			Errors == [] ->
			utf8_codes(Bytes, Codes),
			path_line(Codes, PathCodes),
			atom_codes(Directory, PathCodes),
			(	Directory \== '', directory_exists(Directory) ->
				true
			;	transport_error(allocation, Status, Codes)
			)
		;	transport_error(allocation, Status, Bytes-Errors)
		).

	allocation_command(posix, Base, '/bin/sh', Arguments) :-
		resolve_command(mktemp, Mktemp),
		script_path('scripts/problog_process.sh', Script),
		Arguments = [Script, allocate, Mktemp, Base].
	allocation_command(windows, Base, Shell, Arguments) :-
		powershell_command(allocate, [Base], Shell, Arguments).

	run_in_workspace(Platform, Directory, Executable, Arguments, ProgramSource, Codes, Diagnostics, RawStatus, NormalizedStatus) :-
		program_path(ProgramSource, Directory, ProgramPath),
		atom_concat(Directory, '/stdout.txt', OutputPath),
		atom_concat(Directory, '/stderr.txt', ErrorPath),
		append(Arguments, ['--', ProgramPath], SolverArguments),
		execution_command(Platform, Executable, SolverArguments, OutputPath, ErrorPath, Shell, ShellArguments),
		control_process(Shell, ShellArguments, LauncherOutput, LauncherError, RawStatus, NormalizedStatus),
		(	LauncherOutput == [],
			LauncherError == [] ->
			read_utf8(OutputPath, Codes),
			read_utf8(ErrorPath, Diagnostics)
		;	transport_error(launcher, NormalizedStatus, LauncherOutput-LauncherError)
		).

	program_path(file(Path), _, Absolute) :-
		(	absolute_file_name(Path, Absolute) ->
			true
		;	existence_error(source_sink, Path)
		),
		(	file_exists(Absolute) ->
			true
		;	existence_error(source_sink, Absolute)
		).
	program_path(codes(Codes), Directory, Path) :-
		atom_concat(Directory, '/program.pl', Path),
		write_codes_file(Path, Codes).

	execution_command(posix, Executable, Arguments, Output, Error, '/bin/sh', ShellArguments) :-
		script_path('scripts/problog_process.sh', Script),
		ShellArguments = [Script, run, Output, Error, Executable| Arguments].
	execution_command(windows, Executable, Arguments, Output, Error, Shell, ShellArguments) :-
		working_directory(Working),
		Parameters = [Output, Error, Working, Executable| Arguments],
		powershell_command(run, Parameters, Shell, ShellArguments).

	powershell_command(Operation, Parameters, Shell, Arguments) :-
		resolve_command(pwsh, Shell),
		script_path('scripts/problog_process.ps1', Script),
		Arguments = ['-NoProfile', '-NonInteractive', '-ExecutionPolicy', 'Bypass', '-File', Script, Operation| Parameters].

	control_process(Shell, Arguments, OutputBytes, ErrorBytes, RawStatus, Status) :-
		process_arguments(Shell, Arguments, ProcessShell, ProcessArguments),
		(	create(ProcessShell, ProcessArguments, [process(Pid), stdin(Input), stdout(Output), stderr(Error), type(binary)]) ->
			true
		;	transport_error(spawn, unknown, [])
		),
		catch(
			(	close(Input),
				control_bytes(Output, OutputBytes),
				close(Output),
				control_bytes(Error, ErrorBytes),
				close(Error)
			),
			Exception,
			(	catch(close(Input), _, true),
				catch(close(Output), _, true),
				catch(close(Error), _, true),
				catch(terminate_owned_process(Pid), _, true),
				catch(wait(Pid, _), _, true),
				throw(Exception)
			)
		),
		wait(Pid, RawStatus, Status).

	terminate_owned_process(Pid) :-
		(	operating_system_type(windows) ->
			resolve_command_path(taskkill, Taskkill),
			number_codes(Pid, PidCodes),
			atom_codes(PidAtom, PidCodes),
			process_arguments(Taskkill, ['/F', '/T', '/PID', PidAtom], ProcessTaskkill, Arguments),
			create(ProcessTaskkill, Arguments, [process(Killer), stdin(Input), stdout(Output), stderr(Error)]),
			catch(
				(	close(Input),
					control_bytes(Output, _),
					close(Output),
					control_bytes(Error, _),
					close(Error)
				),
				Exception,
				(	catch(close(Input), _, true),
					catch(close(Output), _, true),
					catch(close(Error), _, true),
					catch(ignore(kill(Killer)), _, true),
					catch(wait(Killer, _), _, true),
					throw(Exception)
				)
			),
			wait(Killer, _)
		;	ignore(kill(Pid))
		).

	control_bytes(Stream, Bytes) :-
		(	stream_property(Stream, type(binary)) ->
			stream_to_bytes(Stream, Bytes)
		;	stream_to_codes(Stream, Codes),
			(	codes_to_bytes(Codes, Bytes) ->
				true
			;	representation_error(utf_8)
			)
		).

	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		process_arguments(Shell, Arguments, QuotedShell, QuotedArguments) :-
			platform(Platform),
			(	Platform == posix ->
				quote(posix, Shell, QuotedShell),
				posix_arguments(Arguments, QuotedArguments)
			;	atom_concat('"', Shell, Prefix),
				atom_concat(Prefix, '"', QuotedShell),
				QuotedArguments = Arguments
			).

		posix_arguments([], []).
		posix_arguments([Argument| Arguments], [Quoted| QuotedArguments]) :-
			quote(posix, Argument, Quoted),
			posix_arguments(Arguments, QuotedArguments).
	:- else.
		process_arguments(Shell, Arguments, Shell, Arguments).
	:- endif.

	script_path(Name, Path) :-
		object_property(problog_process, file(_, Directory)),
		atom_concat(Directory, Name, Path).

	resolve_command(Command, Path) :-
		(	resolve_command_path(Command, Path) ->
			true
		;	existence_error(command, Command)
		).

	write_codes_file(Path, Codes) :-
		(	codes_to_bytes(Codes, Bytes) ->
			true
		;	representation_error(utf_8)
		),
		open(Path, write, Stream, [type(binary)]),
		catch(
			(	write_bytes(Bytes, Stream),
				close(Stream)
			),
			Error,
			(	catch(close(Stream), _, true),
				throw(Error)
			)
		).

	read_utf8(Path, Codes) :-
		file_to_bytes(Path, Bytes),
		utf8_codes(Bytes, Codes).

	utf8_codes(Bytes, Codes) :-
		(	bytes_to_codes(Bytes, Codes) ->
			true
		;	representation_error(utf_8)
		).

	path_line(Codes, Path) :-
		(	append(Path, [13,10], Codes) ->
			true
		;	append(Path, [10], Codes) ->
			true
		;	Path = Codes
		).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	remove_workspace(Directory) :-
		(	directory_exists(Directory) ->
			delete_directory_and_contents(Directory)
		;	existence_error(directory, Directory)
		).

	transport_error(Stage, Status, Details) :-
		context(Context),
		throw(error(problog_transport(Stage, Status, Details), Context)).

	quote(Platform, Argument, Quoted) :-
		context(Context),
		check(atom, Platform, Context),
		check(atom, Argument, Context),
		(	Platform == posix ->
			true
		;	Platform == windows ->
			true
		;	domain_error(problog_shell_platform, Platform)
		),
		atom_codes(Argument, Codes),
		(	member(0, Codes) ->
			domain_error(problog_shell_argument, Argument)
		;	true
		),
		phrase(quoted(Platform, Codes), QuotedCodes),
		atom_codes(Quoted, QuotedCodes).

	quoted(Platform, Codes) -->
		[39],
		quoted_codes(Codes, Platform),
		[39].

	quoted_codes([], _) -->
		[].
	quoted_codes([39| Codes], posix) -->
		!,
		[39,34,39,34,39],
		quoted_codes(Codes, posix).
	quoted_codes([39| Codes], windows) -->
		!,
		[39,39],
		quoted_codes(Codes, windows).
	quoted_codes([Code| Codes], Platform) -->
		[Code],
		quoted_codes(Codes, Platform).

:- end_object.
