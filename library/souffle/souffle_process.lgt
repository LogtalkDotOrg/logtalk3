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


:- object(souffle_process).

	% Keep shell construction isolated here; all child communication uses process.
	% Souffle writes relations to files, so stdout/stderr capture and the owned
	% facts/output directories cannot deadlock on full pipes.

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Isolated operating-system shell transport for the Souffle CLI, including owned facts and output directories.'
	]).

	:- public(quote/3).
	:- mode(quote(+atom, +atom, -atom), one_or_error).
	:- info(quote/3, [
		comment is 'Quotes an atom as one POSIX shell or Windows PowerShell literal, without interpolation.',
		argnames is ['Platform', 'Argument', 'Quoted'],
		exceptions is [
			'``Platform`` is a variable' - instantiation_error,
			'``Argument`` is a variable' - instantiation_error,
			'``Platform`` is neither a variable nor an atom' - type_error(atom, 'Platform'),
			'``Argument`` is neither a variable nor an atom' - type_error(atom, 'Argument'),
			'``Platform`` is an atom but neither ``posix`` nor ``windows``' - domain_error(souffle_shell_platform, 'Platform'),
			'``Argument`` is an atom but contains a null character' - domain_error(souffle_shell_argument, 'Argument')
		]
	]).

	:- public(run/10).
	:- mode(run(+atom, +list(atom), +compound, +compound, +list(atom), -list(integer), -list(integer), -list(compound), -term, -compound), one_or_error).
	:- info(run/10, [
		comment is 'Runs the CLI using an owned directory and separate UTF-8 capture files, materializing EDB facts and reading back requested ``.output`` relation files before cleanup, returning only after cleanup.',
		argnames is [
			'Executable', 'Arguments', 'ProgramSource', 'FactsSpec', 'OutputRelations', 'Codes',
			'Diagnostics', 'RelationCodes', 'RawStatus', 'NormalizedStatus'
		],
		exceptions is [
			'``Executable``, ``Arguments``, ``ProgramSource``, ``FactsSpec``, or ``OutputRelations`` is a variable' - instantiation_error,
			'An element of the list ``Arguments`` or ``OutputRelations`` is a variable' - instantiation_error,
			'``Executable`` is neither a variable nor an atom' - type_error(atom, 'Executable'),
			'``Arguments`` is neither a variable nor a list' - type_error(list, 'Arguments'),
			'An element of the list ``Arguments`` is neither a variable nor an atom' - type_error(atom, 'Argument'),
			'An element of the list ``OutputRelations`` is neither a variable nor an atom' - type_error(atom, 'Name'),
			'``ProgramSource`` is neither ``file(Path)`` nor ``codes(Codes)``' - domain_error(souffle_process_source, 'ProgramSource'),
			'``FactsSpec`` is neither ``none``, ``external(Path)``, nor ``materialize(Pairs)``' - domain_error(souffle_facts_spec, 'FactsSpec'),
			'A materialized facts row has a symbol column value containing a tab or newline character, or a value that is not an integer, float, or atom' - domain_error(souffle_facts_value, 'Value'),
			'An argument contains a null character' - domain_error(souffle_shell_argument, 'Argument'),
			'A launcher dependency does not exist' - existence_error(command, 'Command'),
			'A source or capture file does not exist' - existence_error(source_sink, 'Path'),
			'A source or capture file cannot be opened' - permission_error(open, source_sink, 'Path'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - souffle_transport('Stage', 'Status', 'Details')
		]
	]).

	:- uses(list, [
		member/2, append/3
	]).

	:- uses(reader, [
		stream_to_bytes/2, stream_to_codes/2, file_to_bytes/2
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	run(Executable, Arguments, ProgramSource, FactsSpec, OutputRelations, Codes, Diagnostics, RelationCodes, RawStatus, NormalizedStatus) :-
		context(Context),
		check(ground, Executable-Arguments-ProgramSource-FactsSpec-OutputRelations, Context),
		check(atom, Executable, Context),
		check(list(atom), Arguments, Context),
		check(list(atom), OutputRelations, Context),
		quote(posix, Executable, _),
		forall(member(Argument, Arguments), quote(posix, Argument, _)),
		check_program_source(ProgramSource),
		check_facts_spec(FactsSpec),
		owns_facts(FactsSpec, OwnsFacts),
		platform(Platform),
		create_workspace(Platform, Directory),
		catch(
			(	run_in_workspace(Platform, Directory, Executable, Arguments, ProgramSource, FactsSpec, OutputRelations, Codes, Diagnostics, RelationCodes, RawStatus, NormalizedStatus) ->
				remove_workspace(Directory, OwnsFacts)
			;	remove_workspace(Directory, OwnsFacts),
				transport_error(execution, unknown, [])
			),
			Error,
			(	catch(remove_workspace(Directory, OwnsFacts), _, true),
				throw(Error)
			)
		).

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
		domain_error(souffle_process_source, ProgramSource).

	check_facts_spec(none) :-
		!.
	check_facts_spec(external(Path)) :-
		!,
		context(Context),
		check(atom, Path, Context),
		quote(posix, Path, _).
	check_facts_spec(materialize(Pairs)) :-
		!,
		context(Context),
		check(list, Pairs, Context).
	check_facts_spec(FactsSpec) :-
		domain_error(souffle_facts_spec, FactsSpec).

	owns_facts(external(_), false) :-
		!.
	owns_facts(_, true).

	platform(Platform) :-
		(	os::operating_system_type(windows) ->
			Platform = windows
		;	Platform = posix
		).

	% The OS predicate selects the base; the launcher atomically reserves ownership.
	create_workspace(Platform, Directory) :-
		os::temporary_directory(Temporary),
		os::absolute_file_name(Temporary, Base),
		allocation_command(Platform, Base, Shell, Arguments),
		control_process(Shell, Arguments, Bytes, Errors, _, Status),
		(	Status == exit(0),
			Errors == [] ->
			utf8_codes(Bytes, Codes),
			path_line(Codes, PathCodes),
			atom_codes(Directory, PathCodes),
			(	Directory \== '', os::directory_exists(Directory) ->
				true
			;	transport_error(allocation, Status, Codes)
			)
		;	transport_error(allocation, Status, Bytes-Errors)
		).

	% Pass paths as positional data, with a restrictive umask for the owned directory.
	allocation_command(posix, Base, '/bin/sh', Arguments) :-
		resolve_command(mktemp, Mktemp),
		script_path('scripts/souffle_process.sh', Script),
		Arguments = [Script, allocate, Mktemp, Base].
	allocation_command(windows, Base, Shell, Arguments) :-
		powershell_command(allocate, [Base], Shell, Arguments).

	% Control output is separate from solver captures; launcher failures are not CSV.
	run_in_workspace(Platform, Directory, Executable, Arguments, ProgramSource, FactsSpec, OutputRelations, Codes, Diagnostics, RelationCodes, RawStatus, NormalizedStatus) :-
		program_path(ProgramSource, Directory, ProgramPath),
		facts_directory(FactsSpec, Directory, FactsDir),
		atom_concat(Directory, '/output', OutputDir),
		os::make_directory(OutputDir),
		atom_concat(Directory, '/stdout.txt', StdoutPath),
		atom_concat(Directory, '/stderr.txt', StderrPath),
		append(Arguments, ['-F', FactsDir, '-D', OutputDir, ProgramPath], SolverArguments),
		execution_command(Platform, Executable, SolverArguments, StdoutPath, StderrPath, Shell, ShellArguments),
		control_process(Shell, ShellArguments, LauncherOutput, LauncherError, RawStatus, NormalizedStatus),
		(	LauncherOutput == [],
			LauncherError == [] ->
			read_utf8(StdoutPath, Codes),
			read_utf8(StderrPath, Diagnostics),
			read_relation_files(OutputRelations, OutputDir, RelationCodes)
		;	transport_error(launcher, NormalizedStatus, LauncherOutput-LauncherError)
		).

	program_path(file(Path), _, Absolute) :-
		os::absolute_file_name(Path, Absolute).
	program_path(codes(Codes), Directory, Path) :-
		atom_concat(Directory, '/program.dl', Path),
		write_codes_file(Path, Codes).

	facts_directory(none, Directory, FactsDir) :-
		!,
		atom_concat(Directory, '/facts', FactsDir),
		os::make_directory(FactsDir).
	facts_directory(external(Path), _, Path) :-
		!.
	facts_directory(materialize(Pairs), Directory, FactsDir) :-
		atom_concat(Directory, '/facts', FactsDir),
		os::make_directory(FactsDir),
		write_facts_files(Pairs, FactsDir).

	write_facts_files([], _).
	write_facts_files([Name-Rows| Pairs], FactsDir) :-
		atomic_list_concat([FactsDir, '/', Name, '.facts'], Path),
		rows_codes(Rows, Codes),
		write_codes_file(Path, Codes),
		write_facts_files(Pairs, FactsDir).

	rows_codes([], []).
	rows_codes([Row| Rows], Codes) :-
		row_codes(Row, RowCodes),
		rows_codes(Rows, RestCodes),
		append(RowCodes, RestCodes, Codes).

	row_codes(Row, Codes) :-
		column_codes_list(Row, ColumnCodesList),
		join_tab(ColumnCodesList, JoinedCodes),
		append(JoinedCodes, [10], Codes).

	column_codes_list([], []).
	column_codes_list([Value| Values], [Codes| ColumnCodesList]) :-
		column_value_codes(Value, Codes),
		column_codes_list(Values, ColumnCodesList).

	join_tab([Codes], Codes) :-
		!.
	join_tab([Codes| ColumnCodesList], Joined) :-
		append(Codes, [9| Rest], Joined),
		join_tab(ColumnCodesList, Rest).

	column_value_codes(Value, Codes) :-
		integer(Value),
		!,
		number_codes(Value, Codes).
	column_value_codes(Value, Codes) :-
		float(Value),
		!,
		number_codes(Value, Codes).
	column_value_codes(Value, Codes) :-
		atom(Value),
		!,
		atom_codes(Value, RawCodes),
		(	(member(9, RawCodes); member(10, RawCodes)) ->
			domain_error(souffle_facts_value, Value)
		;	Codes = RawCodes
		).
	column_value_codes(Value, _) :-
		domain_error(souffle_facts_value, Value).

	read_relation_files([], _, []).
	read_relation_files([Name| Names], OutputDir, [Name-Content| RelationCodes]) :-
		atomic_list_concat([OutputDir, '/', Name, '.csv'], Path),
		(	os::file_exists(Path) ->
			read_utf8(Path, FileCodes),
			Content = codes(FileCodes)
		;	Content = missing
		),
		read_relation_files(Names, OutputDir, RelationCodes).

	% exec preserves process ownership for termination; arguments remain positional data.
	execution_command(posix, Executable, Arguments, Output, Error, '/bin/sh', ShellArguments) :-
		script_path('scripts/souffle_process.sh', Script),
		ShellArguments = [Script, run, Output, Error, Executable| Arguments].
	% ArgumentList preserves native argument boundaries. Concurrent raw-byte copying
	% avoids PowerShell text redirection and its encoding changes, without Logtalk threads.
	% Script-level failures kill the solver; control-stream failures terminate the tree below.
	execution_command(windows, Executable, Arguments, Output, Error, Shell, ShellArguments) :-
		os::working_directory(Working),
		Parameters = [Output, Error, Working, Executable| Arguments],
		powershell_command(run, Parameters, Shell, ShellArguments).

	powershell_command(Operation, Parameters, Shell, Arguments) :-
		resolve_command(pwsh, Shell),
		script_path('scripts/souffle_process.ps1', Script),
		Arguments = ['-NoProfile', '-NonInteractive', '-ExecutionPolicy', 'Bypass', '-File', Script, Operation| Parameters].

	control_process(Shell, Arguments, OutputBytes, ErrorBytes, RawStatus, Status) :-
		process_arguments(Shell, Arguments, ProcessShell, ProcessArguments),
		(	process::create(ProcessShell, ProcessArguments, [process(Pid), stdin(Input), stdout(Output), stderr(Error), type(binary)]) ->
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
				catch(process::wait(Pid, _), _, true),
				throw(Exception)
			)
		),
		process::wait(Pid, RawStatus, Status).

	% Windows capture failures require tree termination; POSIX exec leaves only Souffle.
	terminate_owned_process(Pid) :-
		(	os::operating_system_type(windows) ->
			resolve_command(taskkill, Taskkill),
			number_codes(Pid, PidCodes),
			atom_codes(PidAtom, PidCodes),
			process_arguments(Taskkill, ['/F', '/T', '/PID', PidAtom], ProcessTaskkill, Arguments),
			process::create(ProcessTaskkill, Arguments, [process(Killer), stdin(Input), stdout(Output), stderr(Error)]),
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
					catch(ignore(process::kill(Killer)), _, true),
					catch(process::wait(Killer, _), _, true),
					throw(Exception)
				)
			),
			process::wait(Killer, _)
		;	ignore(process::kill(Pid))
		).

	control_bytes(Stream, Bytes) :-
		(	stream_property(Stream, type(binary)) ->
			stream_to_bytes(Stream, Bytes)
		;	stream_to_codes(Stream, Codes),
			(	utf_8::codes_to_bytes(Codes, Bytes) ->
				true
			;	representation_error(utf_8)
			)
		).

	% GNU's process boundary builds a command string, so quote its POSIX arguments here.
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
		object_property(souffle_process, file(_, Directory)),
		atom_concat(Directory, Name, Path).

	resolve_command(Command, Path) :-
		(	os::resolve_command_path(Command, Path) ->
			true
		;	existence_error(command, Command)
		).

	write_codes_file(Path, Codes) :-
		(	utf_8::codes_to_bytes(Codes, Bytes) ->
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
		(	utf_8::bytes_to_codes(Bytes, Codes) ->
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

	% Remove only known owned files and directories; an external facts directory
	% and a user-supplied native program file are never touched.
	remove_workspace(Directory, OwnsFacts) :-
		remove_capture_files(['program.dl', 'stdout.txt', 'stderr.txt'], Directory),
		(	OwnsFacts == true ->
			remove_owned_subdirectory(Directory, '/facts')
		;	true
		),
		remove_owned_subdirectory(Directory, '/output'),
		os::delete_directory(Directory).

	remove_capture_files([], _).
	remove_capture_files([Name| Names], Directory) :-
		atomic_list_concat([Directory, '/', Name], Path),
		( os::file_exists(Path) -> os::delete_file(Path) ; true ),
		remove_capture_files(Names, Directory).

	remove_owned_subdirectory(Directory, Name) :-
		atom_concat(Directory, Name, Path),
		( os::directory_exists(Path) -> os::delete_directory_and_contents(Path) ; true ).

	transport_error(Stage, Status, Details) :-
		context(Context),
		throw(error(souffle_transport(Stage, Status, Details), Context)).

	quote(Platform, Argument, Quoted) :-
		context(Context),
		check(atom, Platform, Context),
		check(atom, Argument, Context),
		(	Platform == posix ->
			true
		;	Platform == windows ->
			true
		;	domain_error(souffle_shell_platform, Platform)
		),
		atom_codes(Argument, Codes),
		(	member(0, Codes) ->
			domain_error(souffle_shell_argument, Argument)
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
