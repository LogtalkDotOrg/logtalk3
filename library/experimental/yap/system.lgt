
:- use_module(library(system)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/6/5,
		comment is 'Operating system interface for YAP.']).


	make_directory(Directory) :-
		{make_directory(Directory)}.


	delete_directory(Directory) :-
		{delete_file(Directory)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{getcwd(Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{directory_files(Directory, Files)}.


	absolute_file_name(File) :-
		{absolute_file_name(File, File)}.


	absolute_file_name(File, Full) :-
		{absolute_file_name(File, Full)}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modification_time(File, Time) :-
		{file_property(File, mod_time(Time))}.


	file_size(File, Size) :-
		{file_property(File, size(Size))}.


	file_type(File, Type) :-
		{file_property(File, type(Type))}.


	file_permission(File, Permission) :-
		{file_exists(File, Permission)}.
 

	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename(Old, New)}.


	symbolic_link(File, Target) :-
		{fail}.


	environment_variable(Variable, Value) :-
		{environ(Variable, Value)}.


	set_environment_variable(Variable, Value) :-
		{putenv(Variable, Value)}.


	date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
		{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
		{fail}.


	cpu_time(Time) :-
		{Time is cputime}.


	host_name(Name) :-
		{host_name(Name)}.


:- end_object.
