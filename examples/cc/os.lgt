%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(current_logtalk_flag(prolog_dialect, ciao)).
	:- use_module(library(system)).
:- elif(current_logtalk_flag(prolog_dialect, xsb)).
	:- import(from(/(expand_atom,2), standard)).
	:- import(from(/(xsb_configuration,2), xsb_configuration)).
:- endif.


:- object(os,
	implements(osp)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2012/05/01,
		comment is 'Simple example of using conditional compilation to implement a portable operating-system interface for selected back-end Prolog compilers.']).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			{working_directory(Current, Current),
			 (	absolute_file_name(Path, [expand(true), relative_to(Current), file_errors(fail)], ExpandedPath) ->
				true
			 ;	absolute_file_name(Path, [expand(true), relative_to(Current), file_type(directory), file_errors(fail)], ExpandedPath)
			 )}.

		make_directory(Directory) :-
			(	{exists_directory(Directory)} ->
				true
			;	{make_directory(Directory)}
			).

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{working_directory(_, Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			{exists_directory(Directory)}.

		file_exists(File) :-
			{exists_file(File)}.

		file_modification_time(File, Time) :-
			{time_file(File, Time)}.

		file_size(File, Size) :-
			{size_file(File, Size)}.

		file_permission(File, Permission) :-
			{access_file(File, Permission)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		delete_file(File) :-
			{delete_file(File)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
			{get_time(Time),
			 convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs)}.

		cpu_time(Time) :-
			{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.

		wall_time(Time) :-
			{statistics(walltime, [Time, _])}.

		operating_system_type(Type) :-
			(	current_prolog_flag(windows, true) ->
				Type = windows
			;	current_prolog_flag(unix, true) ->
				Type = unix
			;	Type = unknown
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, Arguments0),
			find_arguments(Arguments0, Arguments).

		find_arguments([], []).
		find_arguments(['--'| Arguments], Arguments) :-
			!.
		find_arguments([_| Arguments0], Arguments) :-
			find_arguments(Arguments0, Arguments).

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

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
			{working_directory(Current, Current),
			 (	absolute_file_name(Path, [expand(true), relative_to(Current), file_errors(fail)], ExpandedPath) ->
				true
			 ;	absolute_file_name(Path, [expand(true), relative_to(Current), file_type(directory), file_errors(fail)], ExpandedPath)
			 )}.
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
			(	current_prolog_flag(windows, true) ->
				Type = windows
			;	current_prolog_flag(unix, true) ->
				Type = unix
			;	Type = unknown
			).

		command_line_arguments(Arguments) :-
			current_prolog_flag(argv, Arguments).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

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

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			{absolute_file_name(Path, ExpandedPath)}.

		make_directory(Directory) :-
			{make_directory(Directory)}.

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
			{file_exists(File)}.

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
			(	os_version(windows) ->
				Type = windows
			;	Type = unix
			).

		command_line_arguments(Arguments) :-
			argument_list(Arguments).

	:- elif(current_logtalk_flag(prolog_dialect, b)).

		shell(Command, Status) :-
			{system(Command, Status)}.

		shell(Command) :-
			{system(Command)}.

		expand_path(Path, ExpandedPath) :-
			{expand_environment(Path, ExpandedPath)}.

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
			get_main_args(Arguments).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		shell(Command, Status) :-
			{shell(Command, Status)}.

		shell(Command) :-
			{shell(Command)}.

		expand_path(Path, ExpandedPath) :-
			{current_directory(Directory),
			 absolute_file_name(Path, ExpandedPath, [relative_to(Directory)])}.

		make_directory(Directory) :-
			(	{directory_exists(Directory)} ->
				true
			;	{make_directory(Directory)}
			).

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{current_directory(_, Directory)}.

		working_directory(Directory) :-
			{current_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			{directory_exists(Directory)}.

		file_exists(File) :-
			{file_exists(File)}.

		file_modification_time(File, Time) :-
			{file_property(File, modify_timestamp, Time)}.

		file_size(File, Size) :-
			{file_property(File, size_in_bytes, Size)}.

		file_permission(File, Permission) :-
			{file_exists(File, Permission)}.
 
		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

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
			current_prolog_flag(argv, Arguments).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

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
			argv(all, Arguments0),
			findall(Argument, (member(Argument0, Arguments0), atom_string(Argument, Argument0)), [_| Arguments]).

	:- elif(current_logtalk_flag(prolog_dialect, ciao)).

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

		shell(Command, Status) :-
			(	{os_run(Command)} ->
				Status = 0
			;	Status = 1
			).

		shell(Command) :-
			{os_run(Command)}.

		expand_path(_, _) :-
			throw(not_available(expand_path/2)).

		make_directory(Directory) :-
			(	{fs_exists_dir(Directory)} ->
				true
			;	{fs_mkdir(Directory)}
			).

		delete_directory(_) :-
			throw(not_available(delete_directory/2)).

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
			{file_property(File, size, Size)}.

		delete_file(File) :-
			{fs_delete(File)}.

		rename_file(Old, New) :-
			{fs_rename(Old, New)}.

		environment_variable(Variable, Value) :-
			{os_env(Variable, Value)}.

		time_stamp(_) :-
			throw(not_available(time_stamp/1)).

		date_time(_, _, _, _, _, _, _) :-
			throw(not_available(date_time/7)).

		cpu_time(Time) :-
			{Time is cputime}.

		wall_time(_) :-
			throw(not_available(wall_time/1)).

		operating_system_type(Type) :-
			os_name(Name),
			(	Name = win32 ->
				Type = windows
			;	Name = unix ->
				Type = unix
			;	Type = unknown
			).

		command_line_arguments(Arguments) :-
			os_args(Arguments0),
			find_arguments(Arguments0, Arguments).

		find_arguments([], []).
		find_arguments(['--'| Arguments], Arguments) :-
			!.
		find_arguments([_| Arguments0], Arguments) :-
			find_arguments(Arguments0, Arguments).

	:- elif(current_logtalk_flag(prolog_dialect, qp)).

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

		time_stamp(_) :-
			throw(not_available(time_stamp/1)).

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
			get_args(Arguments).

	:- elif(current_logtalk_flag(prolog_dialect, lean)).

		shell(Command, Status) :-
			{split_string(Command, ' ', List),
			 system('.', List, _, Status)}.

		shell(Command) :-
			{split_string(Command, ' ', List),
			 system(List, _)}.

		expand_path(Path, ExpandedPath) :-
			{(	\+ atom_concat('/', _, Path),
				\+ atom_concat('$', _, Path),
				working_directory(Current, Current),
				atom_concat(Current, '/', ExpandedPath0),
				atom_concat(ExpandedPath0, Path, ExpandedPath)
			;	absolute_file_name(Path, ExpandedPath)
			)}.

		make_directory(Directory) :-
			{(	exists_dir(Directory) ->
				true
			;	make_directory(Directory)
			)}.

		delete_directory(_) :-
			throw(not_available(delete_directory/1)).

		change_directory(Directory) :-
			{absolute_file_name(Directory, Path),
			 working_directory(_, Path)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			{exists_dir(Directory)}.

		file_exists(File) :-
			{working_directory(Path0, Path0),
			 atom_concat(Path0, '/', Path1),
			 atom_concat(Path1, File, Path),
			 exists_file(Path)}.

		file_modification_time(_, _) :-
			throw(not_available(file_modification_time/2)).

		file_size(_, _) :-
			throw(not_available(file_size/2)).

		delete_file(File) :-
			{working_directory(Path0, Path0),
			 atom_concat(Path0, '/', Path1),
			 atom_concat(Path1, File, Path),
			 delete_file(Path)}.

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

	:- else.

		:- initialization((write('WARNING: back-end Prolog compiler not supported!'), nl)).

	:- endif.

:- end_object.
