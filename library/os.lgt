%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- if(current_logtalk_flag(prolog_dialect, ciao)).
	:- use_module(library(system)).
:- elif(current_logtalk_flag(prolog_dialect, xsb)).
	:- import(from(/(expand_atom,2), standard)).
	:- import(from(/(xsb_configuration,2), xsb_configuration)).
	:- import(from(/(sys_pid,1), shell)).
:- endif.


:- object(os,
	implements(osp)).

	:- info([
		version is 1.9,
		author is 'Paulo Moura',
		date is 2014/09/28,
		comment is 'Simple example of using conditional compilation to implement a portable operating-system interface for selected back-end Prolog compilers.'
	]).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		pid(PID) :-
			{current_prolog_flag(pid, PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			{working_directory(Current, Current)},
			(	{absolute_file_name(Path, [expand(true), relative_to(Current), file_errors(fail)], ExpandedPath)} ->
				true
			;	{absolute_file_name(Path, [expand(true), relative_to(Current), file_type(directory), file_errors(fail)], ExpandedPath)}
			).

		make_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			(	{exists_directory(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		delete_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			{delete_directory(ExpandedPath)}.

		change_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			{working_directory(_, ExpandedPath)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			expand_path(Directory, ExpandedPath),
			{exists_directory(ExpandedPath)}.

		file_exists(File) :-
			expand_path(File, ExpandedPath),
			{exists_file(ExpandedPath)}.

		file_modification_time(File, Time) :-
			expand_path(File, ExpandedPath),
			{time_file(ExpandedPath, Time)}.

		file_size(File, Size) :-
			expand_path(File, ExpandedPath),
			{size_file(ExpandedPath, Size)}.

		file_permission(File, Permission) :-
			expand_path(File, ExpandedPath),
			{access_file(ExpandedPath, Permission)}.

		rename_file(Old, New) :-
			expand_path(Old, OldExpandedPath),
			expand_path(New, NewExpandedPath),
			{rename_file(OldExpandedPath, NewExpandedPath)}.

		delete_file(File) :-
			expand_path(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
			{get_time(Time),
			 convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs)}.

		cpu_time(Time) :-
			{statistics(cputime, Time)}.

		wall_time(Time) :-
			{statistics(walltime, [Time, _])}.

		operating_system_type(Type) :-
			(	{current_prolog_flag(windows, true)} ->
				Type = windows
			;	{current_prolog_flag(unix, true)} ->
				Type = unix
			;	Type = unknown
			).

		command_line_arguments(Arguments) :-
			{current_prolog_flag(argv, Arguments0)},
			find_arguments(Arguments0, Arguments).

		find_arguments([], []).
		find_arguments(['--'| Arguments], Arguments) :-
			!.
		find_arguments([_| Arguments0], Arguments) :-
			find_arguments(Arguments0, Arguments).

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		pid(PID) :-
			{pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		:- if((current_prolog_flag(version_data, yap(Major,Minor,_,_)), (Major,Minor) @< (6,3))).
		expand_path(Path, ExpandedPath) :-
			{working_directory(Current, Current),
			 absolute_file_name(Path, [access(none), file_type(txt), relative_to(Current)], ExpandedPath)}.
		:- else.
		expand_path(Path, ExpandedPath) :-
			{working_directory(Current, Current)},
			(	{absolute_file_name(Path, [expand(true), relative_to(Current), file_errors(fail)], ExpandedPath)} ->
				true
			;	{absolute_file_name(Path, [expand(true), relative_to(Current), file_type(directory), file_errors(fail)], ExpandedPath)}
			).
		:- endif.

		make_directory(Directory) :-
			(	directory_exists(Directory) ->
				true
			;	{make_directory(Directory)}
			).

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{cd(Directory)}.

		working_directory(Directory) :-
			{getcwd(Directory)}.

		directory_exists(Directory) :-
			{file_exists(Directory),
			 file_property(Directory, type(directory))}.

		file_exists(File) :-
			{file_exists(File)}.

		file_modification_time(File, Time) :-
			{file_property(File, mod_time(Time))}.

		file_size(File, Size) :-
			{file_property(File, size(Size))}.

		file_permission(File, Permission) :-
			{file_exists(File, Permission)}.

		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename(Old, New)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{datime(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.

		cpu_time(Time) :-
			{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.

		wall_time(Time) :-
			{statistics(walltime, [Time, _])}.

		operating_system_type(Type) :-
			(	{current_prolog_flag(windows, true)} ->
				Type = windows
			;	{current_prolog_flag(unix, true)} ->
				Type = unix
			;	Type = unknown
			).

		command_line_arguments(Arguments) :-
			{current_prolog_flag(argv, Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		pid(PID) :-
			{sys_pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			{expand_atom(Path, ExpandedPath)}.

		make_directory(Directory) :-
			{expand_atom(Directory, Expanded),
			 path_sysop(mkdir, Expanded)}.

		delete_directory(Directory) :-
			{expand_atom(Directory, Expanded),
			 path_sysop(rmdir, Expanded)}.

		change_directory(Directory) :-
			{expand_atom(Directory, Expanded),
			 path_sysop(chdir, Expanded)}.

		working_directory(Directory) :-
			{path_sysop(cwd, Directory)}.

		directory_exists(Directory) :-
			{expand_atom(Directory, Expanded),
			 path_sysop(exists, Expanded),
			 path_sysop(isdir, Expanded)}.

		file_exists(File) :-
			{expand_atom(File, Expanded),
			 path_sysop(exists, Expanded),
			 path_sysop(isplain, Expanded)}.

		file_modification_time(File, Time) :-
			{path_sysop(modtime, File, [High, Low]),
			 Time is Low + High * 2 ** 24}.

		file_size(File, Size) :-
			{path_sysop(size, File, Size)}.

		file_permission(File, read) :-
			{path_sysop(readable, File)}.

		file_permission(File, write) :-
			{path_sysop(writable, File)}.

		file_permission(File, execute) :-
			{path_sysop(executable, File)}.

		delete_file(File) :-
			{path_sysop(rm, File)}.

		rename_file(Old, New) :-
			{path_sysop(rename, Old, New)}.

		environment_variable(Variable, Value) :-
			{expand_atom(Variable, Value)}.

		time_stamp(Time) :-
			{standard:datime(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{standard:datime(datime(Year, Month, Day, Hours, Mins, Secs))}.

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

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		pid(PID) :-
			{prolog_pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		make_directory(Directory) :-
			(	directory_exists(Directory) ->
				true
			;	{make_directory(Directory)}
			).

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{change_directory(Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory)}.

		directory_exists(Directory) :-
			{file_exists(Directory),
			 file_property(Directory, type(directory))}.

		file_exists(File) :-
			{file_exists(File),
			 file_property(File, type(regular))}.

		file_modification_time(File, Time) :-
			{file_property(File, last_modification(Time))}.

		file_size(File, Size) :-
			{file_property(File, size(Size))}.

		file_permission(File, Permission) :-
			{file_permission(File, Permission)}.

		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{date_time(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{date_time(dt(Year, Month, Day, Hours, Mins, Secs))}.

		cpu_time(Time) :-
			{cpu_time(Miliseconds), Time is Miliseconds/1000}.

		wall_time(Time) :-
			{real_time(Miliseconds), Time is Miliseconds/1000}.

		operating_system_type(Type) :-
			(	{os_version(windows)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{argument_list(Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, b)).

		:- if(predicate_property(getpid(_), _)).
			pid(PID) :-
				{getpid(PID)}.
		:- else.
			pid(_) :-
				throw(not_available(pid/1)).
		:- endif.

		shell(Command, Status) :-
			{system(Command, Status)}.

		shell(Command) :-
			{system(Command)}.

		expand_path(Path, ExpandedPath) :-
			{expand_environment(Path, ExpandedPath0),
			 (	(	sub_atom(ExpandedPath0, 0, 1, _, '/')
			 	;	sub_atom(ExpandedPath0, 1, 1, _, ':')
			 	) ->
			 	ExpandedPath = ExpandedPath0
			 ;	working_directory(Current),
			 	atom_concat(Current, '/', Directory),
			 	atom_concat(Directory, ExpandedPath0, ExpandedPath)
			 )}.

		make_directory(Directory) :-
			(	directory_exists(Directory) ->
				true
			;	{make_directory(Directory)}
			).

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{chdir(Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory)}.

		directory_exists(Directory) :-
			{file_exists(Directory),
			 file_property(Directory, type(directory))}.

		file_exists(File) :-
			{file_exists(File)}.

		file_modification_time(File, Time) :-
			{file_property(File, modification_time(Time))}.

		file_size(File, Size) :-
			{file_property(File, size(Size))}.

		file_permission(File, Permission) :-
			{file_property(File, permission(Permission))}.

		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(ts(Year, Month, Day, Hours, Mins, Secs)) :-
			{date(Year, Month, Day), time(Hours, Mins, Secs)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{date(Year, Month, Day), time(Hours, Mins, Secs)}.

		cpu_time(Time) :-
			{cputime(Miliseconds), Time is Miliseconds/1000}.

		wall_time(_) :-
			throw(not_available(wall_time/1)).

		operating_system_type(Type) :-
			(	{environ('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{get_main_args(Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		pid(PID) :-
			{pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			{current_directory(Directory),
			 absolute_file_name(Path, ExpandedPath, [relative_to(Directory)])}.

		make_directory(Directory) :-
			expand_path(Directory, Path),
			(	{directory_exists(Path)} ->
				true
			;	{make_directory(Path)}
			).

		delete_directory(Directory) :-
			expand_path(Directory, Path),
			{delete_directory(Path)}.

		change_directory(Directory) :-
			{current_directory(_, Directory)}.

		working_directory(Directory) :-
			{current_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			expand_path(Directory, Path),
			{directory_exists(Path)}.

		file_exists(File) :-
			expand_path(File, Path),
			{file_exists(Path)}.

		file_modification_time(File, Time) :-
			expand_path(File, Path),
			{file_property(Path, modify_timestamp, Time)}.

		file_size(File, Size) :-
			expand_path(File, Path),
			{file_property(Path, size_in_bytes, Size)}.

		file_permission(File, Permission) :-
			expand_path(File, Path),
			{file_exists(Path, Permission)}.

		delete_file(File) :-
			expand_path(File, Path),
			{delete_file(Path)}.

		rename_file(Old, New) :-
			expand_path(Old, OldPath),
			expand_path(New, NewPath),
			{rename_file(OldPath, NewPath)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{datime(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.

		cpu_time(Time) :-
			{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.

		wall_time(Time) :-
			{statistics(walltime, [Time, _])}.

		operating_system_type(Type) :-
			(	{environ('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{current_prolog_flag(argv, Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		pid(PID) :-
			{get_flag(pid, PID)}.

		shell(Command, Status) :-	% for UNIX anyway...
			{getenv('SHELL', Shell),
			 exec([Shell,'-c',Command], [], Pid),
			 wait(Pid, Code),
			 Status is Code >> 8 /\ 255}.

		shell(Command) :-
			{system(Command)}.

		expand_path(Path, ExpandedPath) :-
			{canonical_path_name(Path, ExpandedPath)}.	% works with strings and atoms

		make_directory(Directory) :-
			(	{exists(Directory)} ->
				true
			;	{mkdir(Directory)}
			).

		delete_directory(Directory) :-
			{delete(Directory)}.

		change_directory(Directory) :-
			{cd(Directory)}.

		working_directory(Directory) :-
			{getcwd(DirectoryString),
			 atom_string(Directory, DirectoryString)}.

		directory_exists(Directory) :-
			{exists(Directory)}.

		file_exists(File) :-
			{exists(File)}.

		file_modification_time(File, Time) :-
			{get_file_info(File, mtime, Time)}.

		file_size(File, Size) :-
			{get_file_info(File, size, Size)}.

		delete_file(File) :-
			{delete(File)}.

		rename_file(Old, New) :-
			{rename(Old, New)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, ValueString),
			 atom_string(Value, ValueString)}.

		time_stamp(Time) :-
			{get_flag(unix_time, Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{get_flag(unix_time, Time),
			 local_time(Year, Month, Day, Hours, Mins, Secs, _, Time)}.

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

	:- elif(current_logtalk_flag(prolog_dialect, ciao)).

		pid(PID) :-
			{get_pid(PID)}.

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		make_directory(Directory) :-
			(	{file_exists(Directory)} ->
				true
			;	{make_directory(Directory)}
			).

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{cd(Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			{file_exists(Directory),
			 file_property(Directory, type(directory))}.

		file_exists(File) :-
			{file_exists(File)}.

		file_modification_time(File, Time) :-
			{file_property(File, mod_time(Time))}.

		file_size(File, Size) :-
			{file_property(File, size(Size))}.

		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		environment_variable(Variable, Value) :-
			{getenvstr(Variable, String)},
			atom_codes(Value, String).

		time_stamp(Time) :-
			{time(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{datime(_, Year, Month, Day, Hours, Mins, Secs, _, _)}.

		cpu_time(Time) :-
			{statistics(runtime, [Miliseconds| _]), Time is Miliseconds / 1000}.

		wall_time(Time) :-
			{statistics(walltime, [Time, _])}.

		operating_system_type(Type) :-
			(	{getenvstr('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(_) :-
			throw(not_available(command_line_arguments/1)).

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

		expand_path(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		make_directory(Directory) :-
			(	{fs_exists_dir(Directory)} ->
				true
			;	{fs_mkdir(Directory)}
			).

		delete_directory(_) :-
			throw(not_available(delete_directory/1)).

		change_directory(Directory) :-
			{fs_cwd(_, Directory)}.

		working_directory(Directory) :-
			{fs_cwd(Directory)}.

		directory_exists(Directory) :-
			{fs_exists_dir(Directory)}.

		file_exists(File) :-
			{fs_exists_file(File)}.

		file_modification_time(File, Time) :-
			{fs_property(File, time, [_, Time])}.

		file_size(File, Size) :-
			{fs_property(File, size, Size)}.

		delete_file(File) :-
			{fs_delete(File)}.

		rename_file(Old, New) :-
			{fs_rename(Old, New)}.

		environment_variable(Variable, Value) :-
			{os_env(Variable, Value)}.

		time_stamp(_) :-
			throw(not_available(time_stamp/1)).

		date_time(0, 0, 0, 0, 0, 0, 0).

		cpu_time(Time) :-
			{Time is cputime}.

		wall_time(_) :-
			throw(not_available(wall_time/1)).

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

	:- elif(current_logtalk_flag(prolog_dialect, qp)).

		pid(PID) :-
			process_pid(PID).

		shell(Command, Status) :-
			{os(system(Command, Status))}.

		shell(Command) :-
			{os(system(Command))}.

		expand_path(Path, ExpandedPath) :-
			(	{predicate_property(absolute_file_name(_, _), built_in)} ->
				{absolute_file_name(Path, ExpandedPath)}
			;	throw(not_available(expand_path/2))
			).

		make_directory(Directory) :-
			(	{access(Directory, 4, 0)} ->
				true
			;	atom_concat('mkdir "', Directory, Command0),
				atom_concat(Command0, '"', Command),
				{os(system(Command))}
			).

		delete_directory(Directory) :-
			atom_concat('rmdir "', Directory, Command0),
			atom_concat(Command0, '"', Command),
			{os(system(Command))}.

		change_directory(Directory) :-
			{chdir(Directory)}.

		working_directory(Directory) :-
			{getcwd(Directory)}.

		directory_exists(Directory) :-
			{access(Directory, 4, 0)}.

		file_exists(File) :-
			{access(File, 4, 0)}.

		file_modification_time(File, Time) :-
			{stat(File, stat(Time, _))}.

		file_size(File, Size) :-
			{stat(File, stat(_, Size))}.

		delete_file(File) :-
			{access(File, 4, 0)},
			atom_concat('rm "', File, Command0),
			atom_concat(Command0, '"', Command),
			{os(system(Command))}.

		rename_file(Old, New) :-
			atom_concat('mv "', Old, Command0),
			atom_concat(Command0, '" "', Command1),
			atom_concat(Command1, New, Command2),
			atom_concat(Command2, '"', Command),
			{os(system(Command))}.

		environment_variable(Variable, Value) :-
			(	{predicate_property(getenv(_, _), built_in)} ->
				{getenv(Variable, Value)}
			;	{predicate_property(env_getenv(_, _), built_in)} ->
				{env_getenv(Variable, Value)}
			).

		time_stamp(Time) :-
			{realtime(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{realtime(Time)},
			{localtime(Time, time(Year2, Month2, Day, Hours, Mins, Secs, _))},
			Year is 1900 + Year2,
			Month is Month2 + 1.

		cpu_time(Time) :-
			{statistics(runtime, [Miliseconds,_]), Time is Miliseconds/1000}.

		wall_time(_) :-
			throw(not_available(wall_time/1)).

		operating_system_type(Type) :-
			(	environment_variable('COMSPEC', _) ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{get_args(Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, lean)).

		pid(_) :-
			throw(not_available(pid/1)).

		shell(Command, Status) :-
			{split_string(Command, ' ', List),
			 system('.', List, _, Status)}.

		shell(Command) :-
			{split_string(Command, ' ', List),
			 system(List, _)}.

		expand_path(Path, ExpandedPath) :-
			(	\+ atom_concat('/', _, Path),
				\+ atom_concat('$', _, Path),
				{working_directory(Current, Current)},
				atom_concat(Current, '/', ExpandedPath0),
				atom_concat(ExpandedPath0, Path, ExpandedPath1),
				{absolute_file_name(ExpandedPath1, ExpandedPath)}
			;	{absolute_file_name(Path, ExpandedPath)}
			).

		make_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			(	{exists_dir(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		delete_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			{delete_file(ExpandedPath)}.

		change_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			{working_directory(_, ExpandedPath)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			expand_path(Directory, ExpandedPath),
			{exists_dir(ExpandedPath)}.

		file_exists(File) :-
			expand_path(File, ExpandedPath),
			{exists_file(ExpandedPath)}.

		file_modification_time(File, Time) :-
			expand_path(File, ExpandedPath),
			{new_java_object('java.io.File'(ExpandedPath), Object),
			 invoke_java_method(Object, lastModified, Time)}.

		file_size(File, Size) :-
			expand_path(File, ExpandedPath),
			{new_java_object('java.io.File'(ExpandedPath), Object),
			 invoke_java_method(Object, length, Size0)},
			atom_codes(Size0, Codes),
			number_codes(Size, Codes).

		delete_file(File) :-
			expand_path(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{get_date_time(Time),
			 split_string(Time, '/: ', [YearAtom, MonthAtom, DayAtom, HoursAtom, MinsAtom, SecsAtom]),
			 maplist(to_number, [YearAtom, MonthAtom, DayAtom, HoursAtom, MinsAtom, SecsAtom], [Year, Month, Day, Hours, Mins, Secs])}.

		cpu_time(Time) :-
			{cputime(Time)}.

		wall_time(Time) :-
			{cputime(Time)}.

		operating_system_type(Type) :-
			(	{getenv('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{get_cmd_line_args(Arguments)}.

	:- elif(current_logtalk_flag(prolog_dialect, quintus)).

		pid(_) :-
			throw(not_available(pid/1)).

		shell(_, _) :-
			throw(not_available(shell/2)).

		shell(Command) :-
			{unix(shell(Command))}.

		expand_path(Path, ExpandedPath) :-
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

		make_directory(Directory) :-
			expand_path(Directory, Path),
			(	{absolute_file_name(Path, [access(exist), file_type(directory), file_errors(fail)], _)} ->
				true
			;	atom_concat('mkdir ', Path, Command),
				{unix(system(Command))}
			).

		delete_directory(Directory) :-
			expand_path(Directory, Path),
			(	{absolute_file_name(Path, [access(exist), file_type(directory), file_errors(fail)], _)} ->
				atom_concat('rmdir ', Path, Command),
				{unix(system(Command))}
			;	true
			).

		change_directory(Directory) :-
			{unix(cd(Directory))}.

		working_directory(Directory) :-
			{absolute_file_name('.', Directory)}.

		directory_exists(Directory) :-
			expand_path(Directory, ExpandedPath),
			{absolute_file_name(ExpandedPath, [access(exist), file_type(directory), file_errors(fail)], _)}.

		file_exists(File) :-
			expand_path(File, Path),
			{file_exists(Path)}.

		file_modification_time(File, Time) :-
			expand_path(File, Path),
			{file_property(Path, modify_time, Time)}.

		file_size(File, Size) :-
			expand_path(File, Path),
			{file_property(Path, size_in_bytes, Size)}.

		file_permission(File, Permission) :-
			expand_path(File, Path),
			{file_exists(Path, Permission)}.

		delete_file(File) :-
			expand_path(File, Path),
			{delete_file(Path)}.

		rename_file(Old, New) :-
			expand_path(Old, OldPath),
			expand_path(New, NewPath),
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

		cpu_time(Time) :-
			{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.

		wall_time(Time) :-
			{statistics(real_time, [Time, _])}.

		operating_system_type(Type) :-
			(	{environ('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			{unix(argv(Arguments))}.

	:- elif(current_logtalk_flag(prolog_dialect, ji)).

		pid(_) :-
			throw(not_available(pid/1)).

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			% first expand any environment variable
			expand_environment(Path, ExpandedPath0),
			(	(	sub_atom(ExpandedPath0, 0, 1, _, '/')
					% assume POSIX full path 
				;	sub_atom(ExpandedPath0, 1, 1, _, ':')
					% assume Windows full Path starting with a drive letter followed by ":"
				) ->
				% assume full path
				ExpandedPath = ExpandedPath0
			;	% assume path relative to the current directory
				working_directory(Current, Current),
				atom_concat(Current, '/', Directory),
				atom_concat(Directory, ExpandedPath0, ExpandedPath)
			).

		expand_environment(Path, ExpandedPath) :-
			(	sub_atom(Path, 0, 1, _, '$'),
				sub_atom(Path, Before, _, _, '/') ->
				End is Before - 1,
				sub_atom(Path, 1, End, _, Variable),
				sub_atom(Path, Before, _, 0, Rest),
				environment_variable(Variable, Value),
				atom_concat(Value, Rest, ExpandedPath)
			;	Path = ExpandedPath
			).

		make_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			(	{exists_directory(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		delete_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			{delete_directory(ExpandedPath)}.

		change_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			{chdir(ExpandedPath)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			expand_path(Directory, ExpandedPath),
			{exists_directory(ExpandedPath)}.

		file_exists(File) :-
			expand_path(File, ExpandedPath),
			{exists_file(ExpandedPath)}.

		file_modification_time(File, Time) :-
			expand_path(File, ExpandedPath),
			{file_attributes(ExpandedPath, _, _, _, _, _, Time)}.

		file_size(File, Size) :-
			expand_path(File, ExpandedPath),
			{file_attributes(ExpandedPath, _, _, _, _, Size, _)}.

		file_permission(_, _) :-
			throw(not_available(file_permission/2)).

		rename_file(Old, New) :-
			expand_path(Old, OldExpandedPath),
			expand_path(New, NewExpandedPath),
			{rename_file(OldExpandedPath, NewExpandedPath)}.

		delete_file(File) :-
			expand_path(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		environment_variable(Variable, Value) :-
			{(	invoke('java.lang.System', getenv('java.lang.String'), [Variable], Value),
				Value \== [] ->
				true
			;	% check if the environment variable value is passed as a property
			 	invoke('java.lang.System', getProperty('java.lang.String'), [Variable], Value),
				Value \== []
			)}.

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Minutes, Seconds, 0).
			{time(Year, Month, Day, Hours, Minutes, Seconds, _)}.

		cpu_time(Seconds) :-
			{Seconds is cputime}.

		wall_time(_) :-
			throw(not_available(wall_time/1)).

		operating_system_type(Type) :-
			(	environment_variable('COMSPEC', _) ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(_) :-
			throw(not_available(command_line_arguments/1)).

	:- elif(current_logtalk_flag(prolog_dialect, jekejeke)).

		pid(_) :-
			throw(not_available(pid/1)).

		shell(_, _) :-
			throw(not_available(shell/2)).

		shell(_) :-
			throw(not_available(shell/1)).

		expand_path(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		make_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			(	{exists_directory(ExpandedPath)} ->
				true
			;	{make_directory(ExpandedPath)}
			).

		delete_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			{delete_directory(ExpandedPath)}.

		change_directory(Directory) :-
			expand_path(Directory, ExpandedPath),
			set_prolog_flag(base_url, ExpandedPath).

		working_directory(Directory) :-
			current_prolog_flag(base_url, Directory).

		directory_exists(Directory) :-
			expand_path(Directory, ExpandedPath),
			{exists_directory(ExpandedPath)}.

		file_exists(File) :-
			expand_path(File, ExpandedPath),
			{exists_file(ExpandedPath)}.

		file_modification_time(File, Time) :-
			expand_path(File, ExpandedPath),
			{get_time_file(ExpandedPath, Time)}.

		file_size(_, _) :-
			throw(not_available(file_size/2)).

		file_permission(_, _) :-
			throw(not_available(file_permission/2)).

		rename_file(Old, New) :-
			expand_path(Old, OldExpandedPath),
			expand_path(New, NewExpandedPath),
			{rename_file(OldExpandedPath, NewExpandedPath)}.

		delete_file(File) :-
			expand_path(File, ExpandedPath),
			{delete_file(ExpandedPath)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(_) :-
			throw(not_available(time_stamp/1)).

		date_time(0, 0, 0, 0, 0, 0, 0).

		cpu_time(Seconds) :-
			{statistics(time, Miliseconds)},
			Seconds is Miliseconds / 1000 .

		wall_time(Seconds) :-
			{statistics(uptime, Miliseconds)},
			Seconds is Miliseconds / 1000 .

		operating_system_type(Type) :-
			(	{getenv('COMSPEC', _)} ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(_) :-
			throw(not_available(command_line_arguments/1)).

	:- else.

		:- initialization((write('WARNING: back-end Prolog compiler not supported!'), nl)).

	:- endif.

:- end_object.
