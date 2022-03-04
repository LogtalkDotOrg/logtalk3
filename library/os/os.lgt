%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- if(current_logtalk_flag(prolog_dialect, ciao)).
	:- use_module(library(system)).
	:- use_module(engine(runtime_control)).
:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
	:- use_module(library(calendar)).
:- elif(current_logtalk_flag(prolog_dialect, quintus)).
	:- [library(date)].
:- elif(current_logtalk_flag(prolog_dialect, sicstus)).
	:- if((current_logtalk_flag(prolog_version, v(Major,Minor,Patch)), v(Major,Minor,Patch) @< v(4,7,1))).
		:- use_module(library(system3), [shell/2]).
	:- else.
		:- use_module(library(process), [process_create/3]).
	:- endif.
:- elif(current_logtalk_flag(prolog_dialect, scryer)).
	:- use_module(library(lists)).
	:- use_module(library(files)).
	:- use_module(library(os)).
	:- use_module(library(time)).
:- elif(current_logtalk_flag(prolog_dialect, swi)).
	:- use_module(library(filesex)).
:- elif(current_logtalk_flag(prolog_dialect, tau)).
	:- use_module(library(lists)).
	:- use_module(library(statistics)).
:- elif(current_logtalk_flag(prolog_dialect, xsb)).
	:- import(from(/(datime,1), standard)).
	:- import(from(/(expand_atom,2), standard)).
	:- import(from(/(xsb_configuration,2), xsb_configuration)).
	:- import(from(/(sys_pid,1), shell)).
	:- import(from(/(list_directory,2), shell)).
:- endif.


:- object(os,
	implements(osp)).

	:- info([
		version is 1:88:0,
		author is 'Paulo Moura',
		date is 2022-03-04,
		comment is 'Portable operating-system access predicates.',
		remarks is [
			'File path expansion' - 'To ensure portability, all file paths are expanded before being handed to the backend Prolog system.',
			'Exception terms' - 'Currently, there is no standardization of the exception terms thrown by the different backend Prolog systems.',
			'B-Prolog portability' - 'The ``wall_time/1`` predicate is not supported.',
			'JIProlog portability' - 'The ``file_permission/2`` and ``command_line_arguments/1`` predicates are not supported.',
			'Qu-Prolog portability' - 'The ``directory_files/2`` predicate is not supported.',
			'Quintus Prolog' - 'The ``pid/1`` and ``shell/2`` predicates are not supported.',
			'Scryer Prolog' - 'The ``file_permission/2``, ``cpu_time/1``, and ``wall_time/1`` predicates are not supported.',
			'XSB portability' - 'The ``command_line_arguments/1`` predicate is not supported.'
		],
		see_also is [os_types]
	]).

	:- alias(osp, [
		absolute_file_name/2 as expand_path/2
	]).

	expand_path(Path, ExpandedPath) :-
		absolute_file_name(Path, ExpandedPath).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		pid(PID) :-
			current_prolog_flag(pid, PID).

		shell(Command, Status) :-
			(	current_prolog_flag(windows, true) ->
				atom_concat('cmd.exe /C ', Command, ExtendedCommand)
			;	ExtendedCommand = Command
			),
			{shell(ExtendedCommand, Status)}.

		shell(Command) :-
			shell(Command, 0).

		is_absolute_file_name(Path) :-
			{is_absolute_file_name(Path)}.

		absolute_file_name(Path, ExpandedPath) :-
			catch(
				expand_path_(Path, ExpandedPath),
				error(existence_error(variable,_), _),
				expand_only_directory_part(Path, ExpandedPath)
			).

		expand_path_(Path, ExpandedPath) :-
			{working_directory(Current, Current)},
			(	{absolute_file_name(Path, [expand(true), relative_to(Current), file_errors(fail)], ExpandedPath)} ->
				true
			;	{absolute_file_name(Path, [expand(true), relative_to(Current), file_type(directory), file_errors(fail)], ExpandedPath)}
			).

		% workaround to expand file names such as Sample$Config.class where
		% SWI-Prolog will try to expand $Config as an environment variable
		expand_only_directory_part(Path, ExpandedPath) :-
			decompose_file_name(Path, Directory, Name, Extension),
			expand_path_(Directory, ExpandedDirectory),
			{atomic_list_concat([ExpandedDirectory, Name, Extension], ExpandedPath)}.

		internal_os_path(InternalPath, OSPath) :-
			{prolog_to_os_filename(InternalPath, OSPath)}.

		make_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{exists_directory(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{make_directory_path(ExpandedPath)}.

		delete_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{delete_directory(ExpandedPath)}.

		change_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{working_directory(_, ExpandedPath)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_files(Directory, Files) :-
			absolute_file_name(Directory, Path),
			{directory_files(Path, Files)}.

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{exists_directory(ExpandedPath)}.

		file_exists(File) :-
			absolute_file_name(File, ExpandedPath),
			{exists_file(ExpandedPath)}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, ExpandedPath),
			{time_file(ExpandedPath, Time)}.

		file_size(File, Size) :-
			absolute_file_name(File, ExpandedPath),
			{size_file(ExpandedPath, Size)}.

		file_permission(File, Permission) :-
			absolute_file_name(File, ExpandedPath),
			{access_file(ExpandedPath, Permission)}.

		copy_file(File, Copy) :-
			absolute_file_name(File, FilePath),
			absolute_file_name(Copy, CopyPath),
			{copy_file(FilePath, CopyPath)}.

		rename_file(Old, New) :-
			absolute_file_name(Old, OldExpandedPath),
			absolute_file_name(New, NewExpandedPath),
			{rename_file(OldExpandedPath, NewExpandedPath)}.

		delete_file(File) :-
			absolute_file_name(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds) :-
			{	get_time(TimeStamp),
				stamp_date_time(TimeStamp, date(Year,Month,Day,Hours,Minutes,Seconds0,_,_,_), local),
				Seconds is truncate(float_integer_part(Seconds0)),
				Milliseconds is round(float_fractional_part(Seconds0)*1000)
			}.

		cpu_time(Seconds) :-
			{statistics(cputime, Seconds)}.

		wall_time(Seconds) :-
			{statistics(walltime, [Milliseconds, _])},
			Seconds is Milliseconds / 1000.

		operating_system_type(Type) :-
			(	current_prolog_flag(windows, true) ->
				Type = windows
			;	current_prolog_flag(unix, true) ->
				Type = unix
			;	Type = unknown
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, Arguments).

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		pid(PID) :-
			{pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		is_absolute_file_name(Path) :-
			{is_absolute_file_name(Path)}.

		:- if((current_prolog_flag(version_data, yap(Major,Minor,_,_)), (Major,Minor) @< (6,3))).
		absolute_file_name(Path, ExpandedPath) :-
			{	working_directory(Current, Current),
				absolute_file_name(Path, [access(none), file_type(txt), relative_to(Current)], ExpandedPath)
			}.
		:- else.
		absolute_file_name(Path, ExpandedPath) :-
			{working_directory(Current, Current)},
			(	{absolute_file_name(Path, [expand(true), relative_to(Current), file_errors(fail)], ExpandedPath)} ->
				true
			;	{absolute_file_name(Path, [expand(true), relative_to(Current), file_type(directory), file_errors(fail)], ExpandedPath)}
			).
		:- endif.

		internal_os_path(InternalPath, OSPath) :-
			{prolog_to_os_filename(InternalPath, OSPath)}.

		make_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{file_exists(ExpandedPath), file_property(ExpandedPath, type(directory))} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{delete_file(ExpandedPath)}.

		change_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{cd(ExpandedPath)}.

		working_directory(Directory) :-
			{getcwd(Directory)}.

		directory_files(Directory, Files) :-
			absolute_file_name(Directory, Path),
			{directory_files(Path, Files)}.

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{	file_exists(ExpandedPath),
				file_property(ExpandedPath, type(directory))
			}.

		file_exists(File) :-
			absolute_file_name(File, ExpandedPath),
			{	file_exists(ExpandedPath),
				file_property(ExpandedPath, type(regular))
			}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, ExpandedPath),
			{file_property(ExpandedPath, mod_time(Time))}.

		file_size(File, Size) :-
			absolute_file_name(File, ExpandedPath),
			{file_property(ExpandedPath, size(Size))}.

		file_permission(File, Permission) :-
			absolute_file_name(File, ExpandedPath),
			{access_file(ExpandedPath, Permission)}.

		delete_file(File) :-
			absolute_file_name(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		copy_file(File, Copy) :-
			absolute_file_name(File, FilePath),
			absolute_file_name(Copy, CopyPath),
			{copy_file(FilePath, CopyPath)}.

		rename_file(Old, New) :-
			absolute_file_name(Old, OldExpandedPath),
			absolute_file_name(New, NewExpandedPath),
			{rename_file(OldExpandedPath, NewExpandedPath)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{datime(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{datime(datime(Year, Month, Day, Hours, Minutes, Seconds))}.

		cpu_time(Seconds) :-
			{statistics(runtime, [Milliseconds| _])},
			Seconds is Milliseconds / 1000.

		wall_time(Seconds) :-
			{statistics(walltime, [Milliseconds, _])},
			Seconds is Milliseconds / 1000.

		operating_system_type(Type) :-
			(	current_prolog_flag(windows, true) ->
				Type = windows
			;	current_prolog_flag(unix, true) ->
				Type = unix
			;	Type = unknown
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, Arguments).

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		pid(PID) :-
			{sys_pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command, Status)},
			Status =:= 0.

		is_absolute_file_name(Path) :-
			{	expand_atom(Path, EnvVarExpandedPath),
				path_sysop(isabsolute, EnvVarExpandedPath)
			}.

		absolute_file_name(Path, ExpandedPath) :-
			{	expand_atom(Path, EnvVarExpandedPath),
				path_sysop(expand, EnvVarExpandedPath, ExpandedPath0)
			},
			ExpandedPath = ExpandedPath0.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath), path_sysop(isdir, ExpandedPath)} ->
				true
			;	{path_sysop(mkdir, ExpandedPath)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath)} ->
				{path_sysop(rmdir, ExpandedPath)}
			;	existence_error(directory, Directory)
			).

		change_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath)} ->
				{path_sysop(chdir, ExpandedPath)}
			;	existence_error(directory, Directory)
			).

		working_directory(Directory) :-
			{path_sysop(cwd, Directory)}.

		directory_files(Directory, Files) :-
			absolute_file_name(Directory, Path),
			(	{path_sysop(exists, Path)} ->
				{findall(File, list_directory(Path, File), Files)}
			;	existence_error(directory, Directory)
			).

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{	path_sysop(exists, ExpandedPath),
				path_sysop(isdir, ExpandedPath)
			}.

		file_exists(File) :-
			absolute_file_name(File, ExpandedPath),
			{	path_sysop(exists, ExpandedPath),
				path_sysop(isplain, ExpandedPath)
			}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath)} ->
				{path_sysop(modtime, ExpandedPath, Time)}
			;	existence_error(file, File)
			).

		file_size(File, Size) :-
			absolute_file_name(File, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath)} ->
				{path_sysop(size, ExpandedPath, Size)}
			;	existence_error(file, File)
			).

		file_permission(File, read) :-
			absolute_file_name(File, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath)} ->
				{path_sysop(readable, ExpandedPath)}
			;	existence_error(file, File)
			).

		file_permission(File, write) :-
			absolute_file_name(File, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath)} ->
				{path_sysop(writable, ExpandedPath)}
			;	existence_error(file, File)
			).

		file_permission(File, execute) :-
			absolute_file_name(File, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath)} ->
				{path_sysop(executable, ExpandedPath)}
			;	existence_error(file, File)
			).

		delete_file(File) :-
			absolute_file_name(File, ExpandedPath),
			(	{path_sysop(exists, ExpandedPath)} ->
				{path_sysop(rm, ExpandedPath)}
			;	existence_error(file, File)
			).

		copy_file(File, Copy) :-
			absolute_file_name(File, FilePath),
			absolute_file_name(Copy, CopyPath),
			(	{path_sysop(exists, FilePath)} ->
				{path_sysop(copy, FilePath, CopyPath)}
			;	existence_error(file, FilePath)
			).

		rename_file(Old, New) :-
			absolute_file_name(Old, OldPath),
			absolute_file_name(New, NewPath),
			(	{path_sysop(exists, OldPath)} ->
				{path_sysop(rename, OldPath, NewPath)}
			;	existence_error(file, Old)
			).

		environment_variable(Variable, Value) :-
			atom_concat('$', Variable, DollarVariable),
			{expand_atom(DollarVariable, Value)}.

		time_stamp(Time) :-
			{datime(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{datime(datime(Year, Month, Day, Hours, Minutes, Seconds))}.

		cpu_time(Time) :-
			{cputime(Time)}.

		wall_time(Time) :-
			{walltime(Time)}.

		operating_system_type(Type) :-
			(	{xsb_configuration(os_type, windows)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(_) :-
			throw(not_available(command_line_arguments/1)).

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		pid(PID) :-
			{prolog_pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		is_absolute_file_name(Path) :-
			{is_absolute_file_name(Path)}.

		absolute_file_name(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			{absolute_file_name(Directory, ExpandedPath)},
			(	directory_exists(ExpandedPath) ->
				true
			;	{make_directory(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				delete_directory(ExpandedPath)
			}.

		change_directory(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				change_directory(ExpandedPath)
			}.

		working_directory(Directory) :-
			{	working_directory(Directory0),
				absolute_file_name(Directory0, Directory)
			}.

		directory_files(Directory, Files) :-
			{	absolute_file_name(Directory, Path),
				directory_files(Path, Files)
			}.

		directory_exists(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				file_exists(ExpandedPath),
				file_property(ExpandedPath, type(directory))
			}.

		file_exists(File) :-
			{	absolute_file_name(File, ExpandedPath),
				file_exists(ExpandedPath),
				file_property(ExpandedPath, type(regular))
			}.

		file_modification_time(File, Time) :-
			{	absolute_file_name(File, ExpandedPath),
				file_property(ExpandedPath, last_modification(Time))
			}.

		file_size(File, Size) :-
			{	absolute_file_name(File, ExpandedPath),
				file_property(ExpandedPath, size(Size))
			}.

		file_permission(File, Permission) :-
			{	absolute_file_name(File, ExpandedPath),
				file_permission(ExpandedPath, Permission)
			}.

		delete_file(File) :-
			{	absolute_file_name(File, ExpandedPath),
				delete_file(ExpandedPath)
			}.

		rename_file(Old, New) :-
			{	absolute_file_name(Old, OldPath),
				absolute_file_name(New, NewPath),
				rename_file(OldPath, NewPath)
			}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{date_time(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{date_time(dt(Year, Month, Day, Hours, Minutes, Seconds))}.

		cpu_time(Seconds) :-
			{cpu_time(Milliseconds)},
			Seconds is Milliseconds / 1000.

		wall_time(Seconds) :-
			{real_time(Milliseconds)},
			Seconds is Milliseconds / 1000.

		operating_system_type(Type) :-
			(	{environ('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{argument_list(Arguments)}.

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, b)).

		pid(PID) :-
			{getpid(PID)}.

		shell(Command, Status) :-
			{system(Command, Status)}.

		shell(Command) :-
			{system(Command)}.

		is_absolute_file_name(Path) :-
			absolute_file_name(Path, Path).

		absolute_file_name(Path, ExpandedPath) :-
			{	expand_environment(Path, ExpandedPath0),
				(	(	sub_atom(ExpandedPath0, 0, 1, _, '/')
					;	sub_atom(ExpandedPath0, 1, 1, _, ':')
					) ->
					ExpandedPath = ExpandedPath0
				;	working_directory(Current),
					atom_concat(Current, '/', Directory),
					atom_concat(Directory, ExpandedPath0, ExpandedPath)
				)
			}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	directory_exists(ExpandedPath) ->
				true
			;	{make_directory(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{delete_directory(ExpandedPath)}.

		change_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{chdir(ExpandedPath)}.

		working_directory(Directory) :-
			{working_directory(Directory)}.

		directory_files(Directory, Files) :-
			absolute_file_name(Directory, Path),
			{directory_files(Path, Files)}.

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{	file_exists(ExpandedPath),
				(	% workaround B-Prolog bug on Windows where file_exists/1
					% succeeds but file_property/2 throws a file existence error
					catch(file_property(ExpandedPath, type(directory)), _, fail) ->
					true
				;	% workaround B-Prolog bug on Windows where
					% drives are not recognized as directories
					environ('COMSPEC', _),
					sub_atom(ExpandedPath, 1, 1, 0, ':')
				)
			}.

		file_exists(File) :-
			absolute_file_name(File, ExpandedPath),
			{	file_exists(ExpandedPath),
				file_property(ExpandedPath, type(regular))
			}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, ExpandedPath),
			{file_property(ExpandedPath, modification_time(Time))}.

		file_size(File, Size) :-
			absolute_file_name(File, ExpandedPath),
			{file_property(ExpandedPath, size(Size))}.

		file_permission(File, Permission) :-
			absolute_file_name(File, ExpandedPath),
			{file_property(ExpandedPath, permission(Permission))}.

		delete_file(File) :-
			absolute_file_name(File, ExpandedPath),
			(	{file_exists(ExpandedPath)} ->
				{delete_file(ExpandedPath)}
			;	existence_error(file, File)
			).

		copy_file(File, Copy) :-
			absolute_file_name(File, FilePath),
			absolute_file_name(Copy, CopyPath),
			{copy_file(FilePath, CopyPath)}.

		rename_file(Old, New) :-
			absolute_file_name(Old, OldPath),
			absolute_file_name(New, NewPath),
			{rename_file(OldPath, NewPath)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(ts(Year, Month, Day, Hours, Minutes, Seconds)) :-
			{date(Year, Month, Day), time(Hours, Minutes, Seconds)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{date(Year, Month, Day), time(Hours, Minutes, Seconds)}.

		cpu_time(Seconds) :-
			{statistics(runtime, [Milliseconds| _])},
			Seconds is Milliseconds / 1000.

		wall_time(_) :-
			throw(not_available(wall_time/1)).

		operating_system_type(Type) :-
			(	{environ('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{get_main_args(Arguments0)},
			find_arguments(Arguments0, Arguments).

		find_arguments([], []).
		find_arguments(['--'| Arguments], Arguments) :-
			!.
		find_arguments([_| Arguments0], Arguments) :-
			find_arguments(Arguments0, Arguments).

		sleep(Seconds) :-
			number_codes(Seconds, Codes),
			atom_codes(SecondsAtom, Codes),
			atom_concat('sleep ', SecondsAtom, Command),
			{system(Command)}.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		pid(PID) :-
			{pid(PID)}.

		:- if((current_logtalk_flag(prolog_version, v(Major,Minor,Patch)), v(Major,Minor,Patch) @< v(4,7,1))).

			shell(Command, Status) :-
				{shell(Command, Status)},
				!.	% workaround SICStus Prolog bug in shell/2

			shell(Command) :-
				{shell(Command, Status)},
				!,	% workaround SICStus Prolog bug in shell/2
				Status =:= 0.

		:- else.

			shell(Command, Status) :-
				{current_directory(Directory, Directory)},
				(	{environ('COMSPEC', _)} ->
					% assume windows
					atom_concat('cmd.exe /Q /C ', Command, Command1),
					{process_create(Command1, [], [commandline(true), cwd(Directory), wait(Exit)])}
				;	% assume POSIX
					{atomic_list_concat(['/bin/sh -c \'', Command, '\''], Command1)},
					{process_create(Command1, [], [commandline(true), cwd(Directory), wait(Exit)])}
				),
				Exit = exit(Status).

			shell(Command) :-
				shell(Command, 0).

		:- endif.

		is_absolute_file_name(Path) :-
			operating_system_type(Type),
			(	Type == windows ->
				sub_atom(Path, 1, 1, _, ':'),
				sub_atom(Path, 0, 1, _, Drive),
				(	a @=< Drive, Drive @=< z ->
					true
				;	'A' @=< Drive, Drive @=< 'Z'
				)
			;	% assume POSIX or POSIX compatible operating-system
				sub_atom(Path, 0, 1, _, '/')
			).

		absolute_file_name(Path, ExpandedPath) :-
			{	current_directory(Directory),
				absolute_file_name(Path, ExpandedPath, [relative_to(Directory)])
			}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			absolute_file_name(Directory, Path),
			(	{directory_exists(Path)} ->
				true
			;	{make_directory(Path)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			absolute_file_name(Directory, Path),
			{delete_directory(Path)}.

		change_directory(Directory) :-
			{current_directory(_, Directory)}.

		working_directory(Directory) :-
			{current_directory(Directory, Directory)}.

		directory_files(Directory, Files) :-
			absolute_file_name(Directory, Path),
			% ensure an existence error when the directory does not exist
			{	current_directory(Current, Path),
				current_directory(_, Current),
				findall(File1, file_member_of_directory(Path, File1, _), Files1),
				findall(Directory1, directory_member_of_directory(Directory, Directory1, _), Directories1),
				append(['.', '..'| Directories1], Files1, Files)
			}.

		directory_exists(Directory) :-
			% workaround issue with directories like "c:" not
			% being recognized when running on Windows
			atom_concat(Directory, '/', DirectorySlash),
			absolute_file_name(DirectorySlash, Path),
			{directory_exists(Path)}.

		file_exists(File) :-
			absolute_file_name(File, Path),
			{file_exists(Path)}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, Path),
			{file_property(Path, modify_timestamp, Time)}.

		file_size(File, Size) :-
			absolute_file_name(File, Path),
			{file_property(Path, size_in_bytes, Size)}.

		file_permission(File, Permission) :-
			absolute_file_name(File, Path),
			map_file_permission(Permission, Property),
			(	{directory_exists(Path)} ->
				{directory_property(Path, Property, true)}
			;	{file_property(Path, Property, true)}
			).

		map_file_permission(read,    readable).
		map_file_permission(write,   writable).
		map_file_permission(execute, executable).

		delete_file(File) :-
			absolute_file_name(File, Path),
			{delete_file(Path)}.

		rename_file(Old, New) :-
			absolute_file_name(Old, OldPath),
			absolute_file_name(New, NewPath),
			{rename_file(OldPath, NewPath)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{datime(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{datime(datime(Year, Month, Day, Hours, Minutes, Seconds))}.

		cpu_time(Seconds) :-
			{statistics(runtime, [Milliseconds| _])},
			Seconds is Milliseconds / 1000.

		wall_time(Seconds) :-
			{statistics(walltime, [Milliseconds, _])},
			Seconds is Milliseconds / 1000.

		operating_system_type(Type) :-
			(	{environ('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, Arguments).

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		pid(PID) :-
			{get_flag(pid, PID)}.

		shell(Command, Status) :-
			{	(	getenv('SHELL', Shell) ->
					exec([Shell, '-c', Command], [], Pid),
					wait(Pid, Code),
					Status is Code >> 8 /\ 255
				;	getenv('COMSPEC', _),
					sh(Command) ->
					Status is 0
				;	Status is 1
				)
			}.

		shell(Command) :-
			shell(Command, 0).

		is_absolute_file_name(Path) :-
			absolute_file_name(Path, Path).

		absolute_file_name(Path, ExpandedPath) :-
			{	os_file_name(InternalPath, Path),
				canonical_path_name(InternalPath, ExpandedPath)
			}.

		internal_os_path(InternalPath, OSPath) :-
			{os_file_name(InternalPath, OSPath)}.

		make_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{exists(ExpandedPath), get_file_info(ExpandedPath, type, directory)} ->
				true
			;	{mkdir(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			context(Context),
			absolute_file_name(Directory, ExpandedPath),
			(	{exists(ExpandedPath)} ->
				{delete(ExpandedPath)}
			;	throw(error(existence_error(directory,Directory), logtalk(delete_directory(Directory),Context)))
			).

		change_directory(Directory) :-
			context(Context),
			absolute_file_name(Directory, ExpandedPath),
			(	{exists(ExpandedPath)} ->
				{cd(ExpandedPath)}
			;	throw(error(existence_error(directory,Directory), logtalk(change_directory(Directory),Context)))
			).

		working_directory(Directory) :-
			{	getcwd(DirectoryString),
				atom_string(Directory, DirectoryString)
			}.

		directory_files(Directory, Files) :-
			context(Context),
			absolute_file_name(Directory, ExpandedPath),
			{	(	exists(ExpandedPath) ->
					read_directory(ExpandedPath, '*', Directories0, Files0),
					findall(File1, (member(File0, Files0), atom_string(File1, File0)), Files1),
					findall(Directory1, (member(Directory0, Directories0), atom_string(Directory1, Directory0)), Directories1),
					append(['.', '..'| Directories1], Files1, Files)
				;	throw(error(existence_error(directory,Directory), logtalk(directory_files(Directory,Files),Context)))
				)
			}.

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{	exists(ExpandedPath),
				get_file_info(ExpandedPath, type, directory)
			}.

		file_exists(File) :-
			absolute_file_name(File, ExpandedPath),
			{	exists(ExpandedPath),
				get_file_info(ExpandedPath, type, file)
			}.

		file_modification_time(File, Time) :-
			context(Context),
			(	absolute_file_name(File, ExpandedPath),
				{exists(ExpandedPath)} ->
				{get_file_info(ExpandedPath, mtime, Time)}
			;	throw(error(existence_error(file,File), logtalk(file_modification_time(File,Time),Context)))
			).

		file_size(File, Size) :-
			context(Context),
			(	absolute_file_name(File, ExpandedPath),
				{exists(ExpandedPath)} ->
				{get_file_info(ExpandedPath, size, Size)}
			;	throw(error(existence_error(file,File), logtalk(file_size(File,Size),Context)))
			).

		file_permission(File, read) :-
			context(Context),
			(	absolute_file_name(File, ExpandedPath),
				{exists(ExpandedPath)} ->
				{get_file_info(ExpandedPath, readable, on)}
			;	throw(error(existence_error(file,File), logtalk(file_permission(File,read),Context)))
			).

		file_permission(File, write) :-
			context(Context),
			(	absolute_file_name(File, ExpandedPath),
				{exists(ExpandedPath)} ->
				{get_file_info(ExpandedPath, writable, on)}
			;	throw(error(existence_error(file,File), logtalk(file_permission(File,read),Context)))
			).

		file_permission(File, execute) :-
			context(Context),
			(	absolute_file_name(File, ExpandedPath),
				{exists(ExpandedPath)} ->
				{get_file_info(ExpandedPath, executable, on)}
			;	throw(error(existence_error(file,File), logtalk(file_permission(File,execute),Context)))
			).

		delete_file(File) :-
			context(Context),
			absolute_file_name(File, ExpandedPath),
			(	{exists(ExpandedPath)} ->
				{delete(ExpandedPath)}
			;	throw(error(existence_error(file,File), logtalk(delete_file(File),Context)))
			).

		rename_file(Old, New) :-
			context(Context),
			absolute_file_name(Old, OldPath),
			(	{exists(OldPath)} ->
				absolute_file_name(New, NewPath),
				{rename(OldPath, NewPath)}
			;	throw(error(existence_error(file,Old), logtalk(rename_file(Old,New),Context)))
			).

		environment_variable(Variable, Value) :-
			{	getenv(Variable, ValueString),
				atom_string(Value, ValueString)
			}.

		time_stamp(Time) :-
			{get_flag(unix_time, Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{	get_flag(unix_time, Time),
				local_time(Year, Month, Day, Hours, Minutes, Seconds, _, Time)
			}.

		cpu_time(Time) :-
			{cputime(Time)}.

		wall_time(Time) :-
			{statistics(times, [_, _, Time])}.

		operating_system_type(Type) :-
			{get_flag(hostarch, HostArch)},
			(	{(atom_string(i386_nt, HostArch); atom_string(x86_64_nt, HostArch))} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{argv(all, Arguments0),
			findall(Argument, (member(Argument0, Arguments0), atom_string(Argument, Argument0)), [_| Arguments])}.

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, ciao)).

		pid(PID) :-
			{get_pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		is_absolute_file_name(Path) :-
			absolute_file_name(Path, Path).

		absolute_file_name(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			{absolute_file_name(Directory, ExpandedPath)},
			(	{file_exists(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				delete_directory(ExpandedPath)
			}.

		change_directory(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				cd(ExpandedPath)
			}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_files(Directory, Files) :-
			{	absolute_file_name(Directory, Path),
				directory_files(Path, Files)
			}.

		directory_exists(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				file_exists(ExpandedPath),
				file_property(ExpandedPath, type(directory))
			}.

		file_exists(File) :-
			{	absolute_file_name(File, ExpandedPath),
				file_exists(ExpandedPath),
				file_property(ExpandedPath, type(regular))
			}.

		file_modification_time(File, Time) :-
			{	absolute_file_name(File, ExpandedPath),
				file_property(ExpandedPath, mod_time(Time))
			}.

		file_size(File, Size) :-
			{	absolute_file_name(File, ExpandedPath),
				file_property(ExpandedPath, size(Size))
			}.

		file_permission(File, read) :-
			{	absolute_file_name(File, ExpandedPath),
				file_exists(ExpandedPath, 4)
			}.

		file_permission(File, write) :-
			{	absolute_file_name(File, ExpandedPath),
				file_exists(ExpandedPath, 2)
			}.

		file_permission(File, execute) :-
			{	absolute_file_name(File, ExpandedPath),
				file_exists(ExpandedPath, 1)
			}.

		delete_file(File) :-
			{	absolute_file_name(File, ExpandedPath),
				delete_file(ExpandedPath)
			}.

		rename_file(Old, New) :-
			{	absolute_file_name(Old, OldPath),
				absolute_file_name(New, NewPath),
				rename_file(OldPath, NewPath)
			}.

		environment_variable(Variable, Value) :-
			{getenvstr(Variable, String)},
			atom_codes(Value, String).

		time_stamp(Time) :-
			{time(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{datime(_, Year, Month, Day, Hours, Minutes, Seconds, _, _)}.

		cpu_time(Seconds) :-
			{statistics(runtime, [Milliseconds| _])},
			Seconds is Milliseconds / 1000.

		wall_time(Seconds) :-
			{statistics(walltime, [Milliseconds| _])},
			Seconds is Milliseconds / 1000.

		operating_system_type(Type) :-
			(	{getenvstr('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, Arguments).

		sleep(Seconds) :-
			{pause(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, cx)).

		pid(PID) :-
			{os_pid(PID)}.

		shell(Command, Status) :-
			(	{os_run(Command)} ->
				Status = 0
			;	Status = 1
			).

		shell(Command) :-
			{os_run(Command)}.

		is_absolute_file_name(Path) :-
			absolute_file_name(Path, Path).

		absolute_file_name(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			{absolute_file_name(Directory, ExpandedPath)},
			(	{fs_exists_dir(ExpandedPath)} ->
				true
			;	{fs_mkdir(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				fs_delete(ExpandedPath)
			}.

		change_directory(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				fs_cwd(_, ExpandedPath)
			}.

		working_directory(Directory) :-
			{fs_cwd(Directory)}.

		directory_files(Directory, Files) :-
			{	absolute_file_name(Directory, Path),
				fs_cwd(CurrentDirectory, Path),
				fs_files(Files),
				fs_cwd(_, CurrentDirectory)
			}.

		directory_exists(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				fs_exists_dir(ExpandedPath)
			}.

		file_exists(File) :-
			{	absolute_file_name(File, ExpandedPath),
				fs_exists_file(ExpandedPath)
			}.

		file_modification_time(File, Time) :-
			{	absolute_file_name(File, ExpandedPath),
				fs_property(ExpandedPath, time, [_, Time])
			}.

		file_size(File, Size) :-
			{	absolute_file_name(File, ExpandedPath),
				fs_property(ExpandedPath, size, Size)
			}.

		file_permission(File, read) :-
			(	{absolute_file_name(File, ExpandedPath)},
				{(fs_exists_file(ExpandedPath); fs_exists_dir(ExpandedPath))} ->
				{fs_property(ExpandedPath, readable, true)}
			;	existence_error(file, File)
			).

		file_permission(File, write) :-
			(	{absolute_file_name(File, ExpandedPath)},
				{(fs_exists_file(ExpandedPath); fs_exists_dir(ExpandedPath))} ->
				{fs_property(ExpandedPath, writable, true)}
			;	existence_error(file, File)
			).

		delete_file(File) :-
			{	absolute_file_name(File, ExpandedPath),
				fs_delete(ExpandedPath)
			}.

		copy_file(File, Copy) :-
			{	absolute_file_name(File, FilePath),
				absolute_file_name(Copy, CopyPath),
				buffer_new(Buffer),
				open(FilePath, read,  Input),  get_block(Input,  Buffer), close(Input),
				open(CopyPath, write, Output), put_block(Output, Buffer), close(Output),
				buffer_delete(Buffer)
			}.

		rename_file(Old, New) :-
			{	absolute_file_name(Old, OldPath),
				absolute_file_name(New, NewPath),
				fs_rename(OldPath, NewPath)
			}.

		environment_variable(Variable, Value) :-
			{os_env(Variable, Value)}.

		time_stamp(TimeStamp) :-
			TimeStamp is currtime.

		date_time(0, 0, 0, 0, 0, 0, 0).

		cpu_time(Seconds) :-
			Seconds is cputime.

		wall_time(Seconds) :-
			Seconds is currtime.

		operating_system_type(Type) :-
			{os_name(Name)},
			(	Name = win32 ->
				Type = windows
			;	Name = unix ->
				Type = unix
			;	Type = unknown
			).

		command_line_arguments(Arguments) :-
			{os_args(Arguments0)},
			find_arguments(Arguments0, Arguments).

		find_arguments([], []).
		find_arguments(['--'| Arguments], Arguments) :-
			!.
		find_arguments([_| Arguments0], Arguments) :-
			find_arguments(Arguments0, Arguments).

		sleep(Seconds) :-
			number_codes(Seconds, Codes),
			atom_codes(SecondsAtom, Codes),
			atom_concat('sleep ', SecondsAtom, Command),
			{os_run(Command)}.

	:- elif(current_logtalk_flag(prolog_dialect, qp)).

		pid(PID) :-
			process_pid(PID).

		shell(Command, Status) :-
			{os(system(Command, Status))}.

		shell(Command) :-
			{os(system(Command))}.

		is_absolute_file_name(Path) :-
			absolute_file_name(Path, Path).

		absolute_file_name(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			{absolute_file_name(Directory, ExpandedPath)},
			(	{access(ExpandedPath, 4, 0)} ->
				true
			;	atom_concat('mkdir "', ExpandedPath, Command0),
				atom_concat(Command0, '"', Command),
				{os(system(Command))}
			).

		make_directory_path(Directory) :-
			{absolute_file_name(Directory, ExpandedPath)},
			(	{access(ExpandedPath, 4, 0)} ->
				true
			;	atom_concat('mkdir -p "', ExpandedPath, Command0),
				atom_concat(Command0, '"', Command),
				{os(system(Command))}
			).

		delete_directory(Directory) :-
			{absolute_file_name(Directory, ExpandedPath)},
			atom_concat('rmdir "', ExpandedPath, Command0),
			atom_concat(Command0, '"', Command),
			{os(system(Command))}.

		change_directory(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				chdir(ExpandedPath)
			}.

		working_directory(Directory) :-
			{getcwd(Directory)}.

		directory_files(_, _) :-
			throw(not_available(directory_files/2)).

		directory_exists(Directory) :-
			{	absolute_file_name(Directory, ExpandedPath),
				access(ExpandedPath, 4, 0)
			}.

		file_exists(File) :-
			{	absolute_file_name(File, ExpandedPath),
				access(ExpandedPath, 4, 0)
			}.

		file_modification_time(File, Time) :-
			{	absolute_file_name(File, ExpandedPath),
				stat(ExpandedPath, stat(Time, _))
			}.

		file_size(File, Size) :-
			{	absolute_file_name(File, ExpandedPath),
				stat(ExpandedPath, stat(_, Size))
			}.

		file_permission(File, read) :-
			{	absolute_file_name(File, ExpandedPath),
				access(ExpandedPath, 4, 0)
			}.

		file_permission(File, write) :-
			{	absolute_file_name(File, ExpandedPath),
				access(ExpandedPath, 2, 0)
			}.

		file_permission(File, execute) :-
			{	absolute_file_name(File, ExpandedPath),
				access(ExpandedPath, 1, 0)
			}.

		delete_file(File) :-
			{	absolute_file_name(File, ExpandedPath),
				access(ExpandedPath, 4, 0)
			},
			atom_concat('rm "', ExpandedPath, Command0),
			atom_concat(Command0, '"', Command),
			{os(system(Command))}.

		rename_file(Old, New) :-
			{	absolute_file_name(Old, OldPath),
				absolute_file_name(New, NewPath)
			},
			atom_concat('mv "', OldPath, Command0),
			atom_concat(Command0, '" "', Command1),
			atom_concat(Command1, NewPath, Command2),
			atom_concat(Command2, '"', Command),
			{os(system(Command))}.

		environment_variable(Variable, Value) :-
			(	{predicate_property(getenv(_, _), built_in)} ->
				{getenv(Variable, Value)}
			;	{predicate_property(env_getenv(_, _), built_in)},
				{env_getenv(Variable, Value)}
			).

		time_stamp(Time) :-
			{realtime(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{realtime(Time)},
			{localtime(Time, time(Year2, Month2, Day, Hours, Minutes, Seconds, _))},
			Year is 1900 + Year2,
			Month is Month2 + 1.

		cpu_time(Seconds) :-
			{statistics(runtime, [Milliseconds,_])},
			Seconds is Milliseconds / 1000.

		wall_time(Seconds) :-
			{gettimeofday(Seconds)}.

		operating_system_type(Type) :-
			(	environment_variable('COMSPEC', _) ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{get_args(Arguments)}.

		sleep(Seconds) :-
			{thread_sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, quintus)).

		pid(_) :-
			throw(not_available(pid/1)).

		shell(_, _) :-
			throw(not_available(shell/2)).

		shell(Command) :-
			{unix(shell(Command))}.

		is_absolute_file_name(Path) :-
			absolute_file_name(Path, Path).

		absolute_file_name(Path, ExpandedPath) :-
			{expanded_file_name(Path, ExpandedPath0)},
			expand_path_(ExpandedPath0, ExpandedPath).

		expand_path_(Path, ExpandedPath) :-
			atom_codes(Path, PathCodes),
			expand_path_reverse_slashes(PathCodes, ConvertedCodes),
			atom_codes(ConvertedPath, ConvertedCodes),
			(	{absolute_file_name(ConvertedPath, [file_errors(fail)], ExpandedPath)} ->
				true
			;	{absolute_file_name(ConvertedPath, [file_type(directory), file_errors(fail)], ExpandedPath)}
			).

		expand_path_reverse_slashes([], []).
		expand_path_reverse_slashes([Code| Codes], [ConvertedCode| ConvertedCodes]) :-
			(	char_code('\\', Code) ->
				char_code('/', ConvertedCode)
			;	ConvertedCode = Code
			),
			expand_path_reverse_slashes(Codes, ConvertedCodes).

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			absolute_file_name(Directory, Path),
			(	{absolute_file_name(Path, [access(exist), file_type(directory), file_errors(fail)], _)} ->
				true
			;	atom_concat('mkdir ', Path, Command),
				{unix(system(Command))}
			).

		make_directory_path(Directory) :-
			absolute_file_name(Directory, Path),
			(	{absolute_file_name(Path, [access(exist), file_type(directory), file_errors(fail)], _)} ->
				true
			;	atom_concat('mkdir -p ', Path, Command),
				{unix(system(Command))}
			).

		delete_directory(Directory) :-
			absolute_file_name(Directory, Path),
			(	{absolute_file_name(Path, [access(exist), file_type(directory), file_errors(fail)], _)} ->
				atom_concat('rmdir ', Path, Command),
				{unix(system(Command))}
			;	true
			).

		change_directory(Directory) :-
			{unix(cd(Directory))}.

		working_directory(Directory) :-
			{absolute_file_name('.', Directory)}.

		directory_files(Directory, Files) :-
			absolute_file_name(Directory, Path),
			{	findall(File1, file_member_of_directory(Path, File1, _), Files1),
				findall(Directory1, directory_member_of_directory(Directory, Directory1, _), Directories1),
				append(['.', '..'| Directories1], Files1, Files)
			}.

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{absolute_file_name(ExpandedPath, [access(exist), file_type(directory), file_errors(fail)], _)}.

		file_exists(File) :-
			absolute_file_name(File, Path),
			{	file_exists(Path),
				file_property(Path, size_in_bytes, _)
			}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, Path),
			{file_property(Path, modify_time, Time)}.

		file_size(File, Size) :-
			absolute_file_name(File, Path),
			{file_property(Path, size_in_bytes, Size)}.

		file_permission(File, Permission) :-
			absolute_file_name(File, Path),
			map_file_permission(Permission, Property),
			{file_property(Path, Property, true)}.

		map_file_permission(read,    readable).
		map_file_permission(write,   writable).
		map_file_permission(execute, executable).

		delete_file(File) :-
			absolute_file_name(File, Path),
			{delete_file(Path)}.

		rename_file(Old, New) :-
			absolute_file_name(Old, OldPath),
			absolute_file_name(New, NewPath),
			{rename_file(OldPath, NewPath)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{now(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{date(date(Year0, Month0, Day))},
			Year is 1900 + Year0,
			Month is Month0 + 1,
			{time(time(Hours, Minutes, Seconds))}.

		cpu_time(Seconds) :-
			{statistics(runtime, [Milliseconds| _])},
			Seconds is Milliseconds / 1000.

		wall_time(Seconds) :-
			{statistics(real_time, [Milliseconds, _])},
			Seconds is Milliseconds / 1000.

		operating_system_type(Type) :-
			(	{environ('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{unix(argv(Arguments0))},
			find_arguments(Arguments0, Arguments).

		find_arguments([], []).
		find_arguments(['--'| Arguments], Arguments) :-
			!.
		find_arguments([_| Arguments0], Arguments) :-
			find_arguments(Arguments0, Arguments).

		sleep(Seconds) :-
			number_chars(Seconds, Chars),
			atom_chars(Chars, SecondsAtom),
			atom_concat('sleep ', SecondsAtom, Command),
			{unix(shell(Command))}.

	:- elif(current_logtalk_flag(prolog_dialect, ji)).

		pid(PID) :-
			{pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		is_absolute_file_name(Path) :-
			absolute_file_name(Path, Path).

		absolute_file_name(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath0)},
			convert_file_path(ExpandedPath0, ExpandedPath).

		convert_file_path(File, Converted) :-
			atom_codes(File, FileCodes),
			char_code('\\', Backslash),
			char_code('/', Slash),
			reverse_slashes(FileCodes, Backslash, Slash, ConvertedCodes),
			atom_codes(Converted, ConvertedCodes).

		reverse_slashes([], _, _, []).
		reverse_slashes([Code| Codes], Backslash, Slash, [ConvertedCode| ConvertedCodes]) :-
			(	Code =:= Backslash ->
				ConvertedCode = Slash
			;	ConvertedCode = Code
			),
			reverse_slashes(Codes, Backslash, Slash, ConvertedCodes).

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			(	{exists_directory(Directory)} ->
				true
			;	{make_directory(Directory)}
			).

		make_directory_path(Directory) :-
			make_directory(Directory).

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{chdir(Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory0, Directory0)},
			convert_file_path(Directory0, Directory).

		directory_files(Directory, Files) :-
			(	{exists_directory(Directory)} ->
				{directory_files(Directory, Files)}
			;	existence_error(directory, Directory)
			).

		directory_exists(Directory) :-
			{exists_directory(Directory)}.

		file_exists(File) :-
			{exists_file(File), \+ exists_directory(File)}.

		file_modification_time(File, Time) :-
			(	{exists_file(File)} ->
				{file_attributes(File, _, _, _, _, _, Time)}
			;	existence_error(file, File)
			).

		file_size(File, Size) :-
			(	{exists_file(File)} ->
				{file_attributes(File, _, _, _, _, Size, _)}
			;	existence_error(file, File)
			).

		file_permission(_, _) :-
			throw(not_available(file_permission/2)).

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		delete_file(File) :-
			{delete_file(File)}.

		environment_variable(Variable, Value) :-
			(	{invoke('java.lang.System', getenv('java.lang.String'), [Variable], Value)},
				Value \== [] ->
				true
			;	% check if the environment variable value is passed as a property
				{invoke('java.lang.System', getProperty('java.lang.String'), [Variable], Value)},
				Value \== []
			).

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds) :-
			{time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds)}.

		cpu_time(Seconds) :-
			Milliseconds is cputime,
			Seconds is Milliseconds / 1000.

		wall_time(Seconds) :-
			{get_time(Milliseconds)},
			Seconds is Milliseconds / 1000.

		operating_system_type(Type) :-
			{invoke('java.lang.System', getProperty('java.lang.String'), ['os.name'], Name)},
			(	sub_atom(Name, 0, _, _, 'Windows') ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(_) :-
			throw(not_available(command_line_arguments/1)).

		sleep(Seconds) :-
			Milliseconds is Seconds * 1000,
			{sleep(Milliseconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, tau)).

		pid(PID) :-
			{pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		is_absolute_file_name(Path) :-
			{is_absolute_file_name(Path)}.

		absolute_file_name(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{exists_directory(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			make_directory_path_portable(Directory).

		delete_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{delete_directory(ExpandedPath)}.

		change_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{working_directory(_, ExpandedPath)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_files(Directory, Files) :-
			absolute_file_name(Directory, ExpandedPath),
			{directory_files(ExpandedPath, Files)}.

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{exists_directory(ExpandedPath)}.

		file_exists(File) :-
			absolute_file_name(File, ExpandedPath),
			{exists_file(ExpandedPath)}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, ExpandedPath),
			{time_file(ExpandedPath, Time)}.

		file_size(File, Size) :-
			absolute_file_name(File, ExpandedPath),
			{size_file(ExpandedPath, Size)}.

		file_permission(File, Permission) :-
			absolute_file_name(File, ExpandedPath),
			{file_permission(ExpandedPath, Permission)}.

		rename_file(Old, New) :-
			absolute_file_name(Old, OldExpandedPath),
			absolute_file_name(New, NewExpandedPath),
			{rename_file(OldExpandedPath, NewExpandedPath)}.

		delete_file(File) :-
			absolute_file_name(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds) :-
			{	get_time(Time),
				time_property(Time, year(Year)),
				time_property(Time, month(Month0)), Month is Month0 + 1,
				time_property(Time, day(Day)),
				time_property(Time, hours(Hours)),
				time_property(Time, minutes(Minutes)),
				time_property(Time, seconds(Seconds)),
				time_property(Time, milliseconds(Milliseconds))
			}.

		cpu_time(Seconds) :-
			{statistics(runtime, [Milliseconds| _])},
			Seconds is Milliseconds / 1000.

		wall_time(Seconds) :-
			{statistics(walltime, Milliseconds)},
			Seconds is Milliseconds / 1000.

		operating_system_type(Type) :-
			(	{getenv('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, AllArguments),
			{append(_, ['--'| Arguments], AllArguments)}, !.

		sleep(Seconds) :-
			number_codes(Seconds, Codes),
			atom_codes(SecondsAtom, Codes),
			atom_concat('sleep ', SecondsAtom, Command),
			{shell(Command)}.

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

		pid(PID) :-
			{pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		is_absolute_file_name(Path) :-
			absolute_file_name(Path, Path).

		absolute_file_name(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath, [expand(true)])}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{exists_directory(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{make_directory_path(ExpandedPath)}.

		delete_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{delete_file(ExpandedPath)}.

		change_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{working_directory(_, ExpandedPath)}.

		working_directory(Directory) :-
			{working_directory(Directory0, Directory0)},
			atom_chars(Directory, Directory0).

		directory_files(Directory, Files) :-
			absolute_file_name(Directory, ExpandedPath),
			{directory_files(ExpandedPath, Files)}.

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{exists_directory(ExpandedPath)}.

		file_exists(File) :-
			absolute_file_name(File, ExpandedPath),
			{exists_file(ExpandedPath)}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, ExpandedPath),
			{time_file(ExpandedPath, Time)}.

		file_size(File, Size) :-
			absolute_file_name(File, ExpandedPath),
			{size_file(ExpandedPath, Size)}.

		file_permission(File, Permission) :-
			absolute_file_name(File, ExpandedPath),
			{access_file(ExpandedPath, Permission)}.

		rename_file(Old, New) :-
			absolute_file_name(Old, OldExpandedPath),
			absolute_file_name(New, NewExpandedPath),
			{rename_file(OldExpandedPath, NewExpandedPath)}.

		delete_file(File) :-
			absolute_file_name(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds) :-
			{date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds)}.

		cpu_time(Seconds) :-
			{get_time(Seconds)}.

		wall_time(Seconds) :-
			{wall_time(Seconds)}.

		operating_system_type(Type) :-
			(	{getenv('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, Arguments).

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, lvm)).

		pid(PID) :-
			{pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		is_absolute_file_name(Path) :-
			{is_absolute_file_name(Path)}.

		absolute_file_name(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			(	{directory_exists(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		make_directory_path(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{make_directory_path(ExpandedPath)}.

		delete_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{delete_directory(ExpandedPath)}.

		change_directory(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{change_directory(ExpandedPath)}.

		working_directory(Directory) :-
			{current_directory(Directory)}.

		directory_files(Directory, ['.', '..'| Files]) :-
			absolute_file_name(Directory, ExpandedPath),
			{directory_files(ExpandedPath, Files)}.

		directory_exists(Directory) :-
			absolute_file_name(Directory, ExpandedPath),
			{directory_exists(ExpandedPath)}.

		file_exists(File) :-
			absolute_file_name(File, ExpandedPath),
			{file_exists(ExpandedPath)}.

		file_modification_time(File, Time) :-
			absolute_file_name(File, ExpandedPath),
			{file_modification_time(ExpandedPath, Time)}.

		file_size(File, Size) :-
			absolute_file_name(File, ExpandedPath),
			{file_size(ExpandedPath, Size)}.

		file_permission(File, Permission) :-
			absolute_file_name(File, ExpandedPath),
			{file_permission(ExpandedPath, Permission)}.

		copy_file(File, Copy) :-
			absolute_file_name(File, FilePath),
			absolute_file_name(Copy, CopyPath),
			{copy_file(FilePath, CopyPath)}.

		rename_file(Old, New) :-
			absolute_file_name(Old, OldExpandedPath),
			absolute_file_name(New, NewExpandedPath),
			{rename_file(OldExpandedPath, NewExpandedPath)}.

		delete_file(File) :-
			absolute_file_name(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{time_stamp(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{date_time(Year, Month, Day, Hours, Minutes, Seconds)}.

		cpu_time(Seconds) :-
			{cpu_time(Seconds)}.

		wall_time(Seconds) :-
			{wall_time(Seconds)}.

		operating_system_type(Type) :-
			(	{getenv('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, Arguments).

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- elif(current_logtalk_flag(prolog_dialect, scryer)).

		expand_path_chars(Path, ExpandedPathChars) :-
			atom_chars(Path, PathChars),
			{path_segments(PathChars, [Segment| Segments])},
			(	['$'| EnvVar] = Segment ->
				{getenv(EnvVar, Value)},
				{path_segments(PathCharsExpanded, [Value| Segments])}
			;	PathCharsExpanded = PathChars
			),
			(	{path_canonical(PathCharsExpanded, ExpandedPathChars)} ->
				true
			;	PathCharsExpanded = ['/'| _] ->
				ExpandedPathChars = PathCharsExpanded
			;	{working_directory(Current, Current)},
				{append(Current, ['/'| PathCharsExpanded], ExpandedPathChars)}
			).

		pid(PID) :-
			{pid(PID)}.

		shell(Command, Status) :-
			atom_chars(Command, Chars),
			{shell(Chars, Status)}.

		shell(Command) :-
			atom_chars(Command, Chars),
			{shell(Chars)}.

		is_absolute_file_name(Path) :-
			atom_chars(Path, PathChars),
			{path_canonical(PathChars, PathChars)}.

		absolute_file_name(Path, ExpandedPath) :-
			expand_path_chars(Path, ExpandedPathChars),
			atom_chars(ExpandedPath, ExpandedPathChars).

		internal_os_path(Path, OSPath) :-
			internal_os_path_portable(Path, OSPath).

		make_directory(Directory) :-
			expand_path_chars(Directory, ExpandedPathChars),
			(	{directory_exists(ExpandedPathChars)} ->
				true
			;	{make_directory(ExpandedPathChars)}
			).

		make_directory_path(Directory) :-
			expand_path_chars(Directory, ExpandedPathChars),
			{make_directory_path(ExpandedPathChars)}.

		delete_directory(Directory) :-
			expand_path_chars(Directory, ExpandedPathChars),
			{delete_directory(ExpandedPathChars)}.

		change_directory(Directory) :-
			expand_path_chars(Directory, ExpandedPathChars),
			{working_directory(_, ExpandedPathChars)}.

		working_directory(Directory) :-
			{working_directory(DirectoryChars, DirectoryChars)},
			atom_chars(Directory, DirectoryChars).

		directory_files(Directory, ['.', '..'| Files]) :-
			expand_path_chars(Directory, ExpandedPathChars),
			{directory_files(ExpandedPathChars, FilesChars)},
			chars_to_atom(FilesChars, Files).

		chars_to_atom([], []).
		chars_to_atom([FileChars| FilesChars], [File| Files]) :-
			atom_chars(File, FileChars),
			chars_to_atom(FilesChars, Files).

		directory_exists(Directory) :-
			expand_path_chars(Directory, ExpandedPathChars),
			{directory_exists(ExpandedPathChars)}.

		file_exists(File) :-
			expand_path_chars(File, ExpandedPathChars),
			{file_exists(ExpandedPathChars)}.

		file_modification_time(File, Time) :-
			expand_path_chars(File, ExpandedPathChars),
			{file_modification_time(ExpandedPathChars, Time)}.

		file_size(File, Size) :-
			expand_path_chars(File, ExpandedPathChars),
			{file_size(ExpandedPathChars, Size)}.

		file_permission(_, _) :-
			throw(not_available(file_permission/2)).

		rename_file(Old, New) :-
			expand_path_chars(Old, OldExpandedPathChars),
			expand_path_chars(New, NewExpandedPathChars),
			{rename_file(OldExpandedPathChars, NewExpandedPathChars)}.

		delete_file(File) :-
			expand_path_chars(File, ExpandedPathChars),
			{delete_file(ExpandedPathChars)}.

		environment_variable(Variable, Value) :-
			atom_chars(Variable, VariableChars),
			{getenv(VariableChars, ValueChars)},
			atom_chars(Value, ValueChars).

		time_stamp(Time) :-
			{current_time(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0) :-
			{
				current_time(Time),
				memberchk('Y'=YearChars,    Time), number_chars(Year, YearChars),
				memberchk('m'=MonthChars,   Time), number_chars(Month, MonthChars),
				memberchk('d'=DayChars,     Time), number_chars(Day, DayChars),
				memberchk('H'=HoursChars,   Time), number_chars(Hours, HoursChars),
				memberchk('M'=MinutesChars, Time), number_chars(Minutes, MinutesChars),
				memberchk('S'=SecondsChars, Time), number_chars(Seconds, SecondsChars)
			}.

		cpu_time(0.0).

		wall_time(0.0).

		operating_system_type(Type) :-
			(	{getenv("COMSPEC", _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{argv(Arguments)}.

		sleep(Seconds) :-
			{sleep(Seconds)}.

	:- else.

		:- initialization((write('WARNING: backend Prolog compiler not supported!'), nl)).

	:- endif.

	path_concat(Prefix, Suffix, Path) :-
		(	Suffix == '' ->
			(	sub_atom(Prefix, _, 1, 0, '/') ->
				Path = Prefix
			;	atom_concat(Prefix, '/', Path)
			)
		;	absolute_file_name(Suffix, Suffix) ->
			Path = Suffix
		;	sub_atom(Prefix, _, 1, 0, '/') ->
			atom_concat(Prefix, Suffix, Path)
		;	atom_concat(Prefix, '/', Path0),
			atom_concat(Path0, Suffix, Path)
		).

	temporary_directory(Directory) :-
		(	operating_system_type(windows) ->
			temporary_directory_windows(Directory0)
		;	temporary_directory_unix(Directory0)
		),
		absolute_file_name(Directory0, Directory).

	temporary_directory_windows(Directory) :-
		(	environment_variable('TEMP', Directory)
		;	environment_variable('TMP', Directory)
		;	directory_exists('c:\\windows\\temp'), Directory = 'c:\\windows\\temp'
		;	working_directory(Directory)
		),
		!.

	temporary_directory_unix(Directory) :-
		(	environment_variable('TMPDIR', Directory)
		;	environment_variable('TMP', Directory)
		;	environment_variable('TEMP', Directory)
		;	environment_variable('TEMPDIR', Directory)
		;	directory_exists('/tmp'), Directory = '/tmp'
		;	directory_exists('/var/tmp'), Directory = '/var/tmp'
		;	directory_exists('/usr/tmp'), Directory = '/usr/tmp'
		;	working_directory(Directory)
		),
		!.

	null_device_path(Path) :-
		(	operating_system_type(windows) ->
			Path = nul
		;	Path = '/dev/null'
		).

	ensure_directory(Directory) :-
		(	directory_exists(Directory) ->
			true
		;	make_directory_path(Directory)
		).

	ensure_file(File) :-
		(	file_exists(File) ->
			true
		;	decompose_file_name(File, Directory, _),
			make_directory_path(Directory),
			open(File, append, Stream),
			close(Stream)
		).

	decompose_file_name(File, Directory, Basename) :-
		atom_codes(File, FileCodes),
		char_code('/', SlashCode),
		(	strrch(FileCodes, SlashCode, [_Slash| BasenameCodes]) ->
			atom_codes(Basename, BasenameCodes),
			atom_concat(Directory, Basename, File)
		;	Directory = './',
			Basename = File
		).

	decompose_file_name(File, Directory, Name, Extension) :-
		atom_codes(File, FileCodes),
		char_code('/', SlashCode),
		(	strrch(FileCodes, SlashCode, [_Slash| BasenameCodes]) ->
			atom_codes(Basename, BasenameCodes),
			atom_concat(Directory, Basename, File)
		;	Directory = './',
			Basename = File,
			BasenameCodes = FileCodes
		),
		char_code('.', DotCode),
		(	strrch(BasenameCodes, DotCode, ExtensionCodes) ->
			atom_codes(Extension, ExtensionCodes),
			atom_concat(Name, Extension, Basename)
		;	Name = Basename,
			Extension = ''
		).

	% the following auxiliary predicate is simplified version of code
	% written by Per Mildner and is used here with permission
	strrch(Xs, G, Ys) :-
		Xs = [X| Xs1],
		(	X == G ->
			strrch1(Xs1, G, Xs, Ys)
		;	strrch(Xs1, G, Ys)
		).

	strrch1([], _G, Ys, Ys).
	strrch1([X| Xs1], G, Prev, Ys) :-
		(	X == G ->
			strrch1(Xs1, G, [X| Xs1], Ys)
		;	strrch1(Xs1, G, Prev, Ys)
		).

	directory_files(Directory0, Files, Options) :-
		absolute_file_name(Directory0, Directory1),
		directory_files(Directory1, Files0),
		(	sub_atom(Directory1, _, 1, 0, '/') ->
			Directory = Directory1
		;	atom_concat(Directory1, '/', Directory)
		),
		(	list::member(type(Type), Options) ->
			(	Type == regular ->
				filter_regular(Files0, Directory, Files1)
			;	Type == directory ->
				filter_directories(Files0, Directory, Files1)
			;	Files1 = Files0
			)
		;	Files1 = Files0
		),
		(	list::member(extensions([Extension| Extensions]), Options),
			ground([Extension| Extensions]) ->
			filter_extensions(Files1, [Extension| Extensions], Files2)
		;	Files2 = Files1
		),
		(	list::member(prefixes([Prefix| Prefixes]), Options),
			ground([Prefix| Prefixes]) ->
			filter_prefixes(Files2, [Prefix| Prefixes], Files3)
		;	Files3 = Files2
		),
		(	list::member(suffixes([Suffix| Suffixes]), Options),
			ground([Suffix| Suffixes]) ->
			filter_suffixes(Files3, [Suffix| Suffixes], Files4)
		;	Files4 = Files3
		),
		(	list::member(dot_files(Boolean), Options),
			Boolean == false ->
			filter_dot_files(Files4, Files5)
		;	Files5 = Files4
		),
		(	list::member(paths(Paths), Options) ->
			(	Paths == absolute ->
				expand_relative_paths(Files5, Directory, Files)
			;	Files = Files5
			)
		;	Files = Files5
		).

	filter_regular([], _, []).
	filter_regular([File0| Files0], Directory, Files) :-
		atom_concat(Directory, File0, Path0),
		(	file_exists(Path0) ->
			Files = [File0| Rest],
			filter_regular(Files0, Directory, Rest)
		;	filter_regular(Files0, Directory, Files)
		).

	filter_directories([], _, []).
	filter_directories([File0| Files0], Directory, Files) :-
		atom_concat(Directory, File0, Path0),
		(	directory_exists(Path0) ->
			Files = [File0| Rest],
			filter_directories(Files0, Directory, Rest)
		;	filter_directories(Files0, Directory, Files)
		).

	filter_extensions([], _, []).
	filter_extensions([File0| Files0], Extensions, Files) :-
		decompose_file_name(File0, _, _, Extension),
		(	list::member(Extension, Extensions) ->
			Files = [File0| Rest],
			filter_extensions(Files0, Extensions, Rest)
		;	filter_extensions(Files0, Extensions, Files)
		).

	filter_prefixes([], _, []).
	filter_prefixes([File0| Files0], Prefixes, Files) :-
		(	list::member(Prefix, Prefixes),
			sub_atom(File0, 0, _, _, Prefix) ->
			Files = [File0| Rest],
			filter_prefixes(Files0, Prefixes, Rest)
		;	filter_prefixes(Files0, Prefixes, Files)
		).

	filter_suffixes([], _, []).
	filter_suffixes([File0| Files0], Suffixes, Files) :-
		(	list::member(Suffix, Suffixes),
			sub_atom(File0, _, _, 0, Suffix) ->
			Files = [File0| Rest],
			filter_suffixes(Files0, Suffixes, Rest)
		;	filter_suffixes(Files0, Suffixes, Files)
		).

	filter_dot_files([], []).
	filter_dot_files([File0| Files0], Files) :-
		(	sub_atom(File0, 0, 1, _, '.') ->
			filter_dot_files(Files0, Files)
		;	Files = [File0| Rest],
			filter_dot_files(Files0, Rest)
		).

	expand_relative_paths([], _, []).
	expand_relative_paths([File0| Files0], Directory, [File1| Files1]) :-
		atom_concat(Directory, File0, File1),
		expand_relative_paths(Files0, Directory, Files1).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		Dialect \== swi, Dialect \== qp, Dialect \== quintus, Dialect \== trealla, Dialect \== lvm
	)).

		make_directory_path_portable(Path) :-
			absolute_file_name(Path, ExpandedPath),
			(	directory_exists(ExpandedPath) ->
				Parts = []
			;	sub_atom(ExpandedPath, _, 1, 0, '/') ->
				sub_atom(ExpandedPath, 0, _, 1, ExpandedPath1),
				path_parts(ExpandedPath1, [], Parts)
			;	path_parts(ExpandedPath, [], Parts)
			),
			make_parts(Parts).

		path_parts(Path, Parts0, Parts) :-
			decompose_file_name(Path, Directory, Name, Extension),
			(	Path == Directory ->
				atom_concat(Name, Extension, Part0),
				atom_concat('/', Part0, Part),
				Parts = [Part| Parts0]
			;	Directory == ('/') ->
				Parts = [Path| Parts0]
			;	Directory = './' ->
				atom_concat(Name, Extension, Part),
				Parts = [Part| Parts0]
			;	atom_concat(Name, Extension, Part0),
				atom_concat('/', Part0, Part),
				sub_atom(Directory, 0, _, 1, Directory1),
				path_parts(Directory1, [Part| Parts0], Parts)
			).

		make_parts([]).
		make_parts([Part| Parts]) :-
			make_parts_(Parts, Part).

		make_parts_([], Part) :-
			(	directory_exists(Part) ->
				true
			;	make_directory(Part)
			).
		make_parts_([Part| Parts], Root) :-
			(	directory_exists(Root) ->
				true
			;	make_directory(Root)
			),
			atom_concat(Root, Part, NewRoot),
			make_parts_(Parts, NewRoot).

	:- endif.

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		Dialect \== eclipse, Dialect \== swi, Dialect \== yap
	)).

		internal_os_path_portable(Path, OSPath) :-
			(	environment_variable('COMSPEC', _) ->
				(	atom(Path) ->
					atom_chars(Path, PathChars),
					slashes_to_backslashes(PathChars, OSPathChars),
					atom_chars(OSPath, OSPathChars)
				;	atom(OSPath),
					atom_chars(OSPath, OSPathChars),
					backslashes_to_slashes(OSPathChars, PathChars),
					atom_chars(Path, PathChars)
				)
			;	OSPath = Path
			).

		slashes_to_backslashes([], []).
		slashes_to_backslashes(['/'| PathChars], ['\\'| OSPathChars]) :-
			!,
			slashes_to_backslashes(PathChars, OSPathChars).
		slashes_to_backslashes([Char| PathChars], [Char| OSPathChars]) :-
			slashes_to_backslashes(PathChars, OSPathChars).

		backslashes_to_slashes([], []).
		backslashes_to_slashes(['\\'| OSPathChars], ['/'| PathChars]) :-
			!,
			backslashes_to_slashes(OSPathChars, PathChars).
		backslashes_to_slashes([Char| OSPathChars], [Char| PathChars]) :-
			backslashes_to_slashes(OSPathChars, PathChars).

	:- endif.


	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(	Dialect == ciao; Dialect == eclipse; Dialect == gnu; Dialect == ji;
			Dialect == quintus; Dialect == scryer; Dialect == sicstus; Dialect == tau;
			Dialect == trealla
		)
	)).

		copy_file(File, Copy) :-
			absolute_file_name(File, FilePath),
			absolute_file_name(Copy, CopyPath),
			open(FilePath, read, Input, [type(binary)]),
			open(CopyPath, write, Output, [type(binary)]),
			get_byte(Input, Byte),
			copy_file_bytes(Byte, Input, Output),
			close(Input),
			close(Output).

		copy_file_bytes(-1, _, _) :-
			!.
		copy_file_bytes(Byte, Input, Output) :-
			put_byte(Output, Byte),
			get_byte(Input, Next),
			copy_file_bytes(Next, Input, Output).

	:- endif.

:- end_object.
