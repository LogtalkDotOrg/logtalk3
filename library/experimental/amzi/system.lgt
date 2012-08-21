
:- use_module(library(system)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/6/5,
		comment is 'Operating system interface for Amzi! Prolog.']).


	make_directory(Directory) :-
		{mkdir(Directory)}.


	delete_directory(Directory) :-
		{rmdir(Directory, 0)}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{curdir(Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{directory_files(Directory, Files)}.


	absolute_file_name(File) :-
		{fail}.


	absolute_file_name(File, Full) :-
		{fail}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modification_time(File, Time) :-
		{stat(File, _, _, Time, _, _, _, _, _)}.


	file_size(File, Size) :-
		{stat(File, _, _, _, Size, _, _, _, _)}.


	file_permission(File, Permission) :-
		{stat(File, _, _, _, _, Permission, _, _, _)}.


	delete_file(File) :-
		{delfile(File, 0)}.


	rename_file(Old, New) :-
		{rename(Old, New)}.


	symbolic_link(File, Target) :-
		{fail}.


	environment_variable(Variable, Value) :-
		{get_env_var(Variable, Value)}.


	set_environment_variable(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
		{date(Year, Month, Day), time(Hours, Mins, Secs)}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
		{fail}.


	cpu_time(Time) :-
		{Time is cputime}.


	host_name(Name) :-
		{current_host(Name)}.


:- end_object.
