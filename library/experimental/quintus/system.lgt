
:- [library(files)].
:- [library(directory)].
:- [library(date)].


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/6/5,
		comment is 'Operating system interface for Quintus Prolog.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), unix(Command)}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), unix(Command)}.


	change_directory(Directory) :-
		{unix(cd(Directory))}.


	working_directory(Directory) :-
		{absolute_file_name('.', Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{file_members_of_directory(Directory, '*', Files)}.


	absolute_file_name(File) :-
		{absolute_file_name(File, File)}.


	absolute_file_name(File, Full) :-
		{absolute_file_name(File, Full)}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modification_time(File, Time) :-
		{fail}.


	file_size(File, Size) :-
		{file_property(File, size, Size)}.


	file_type(File, Type) :-
		{file_property(File, type, Type)}.


	file_permission(File, Permission) :-
		{file_exists(File, Permission)}.
 

	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	environment_variable(Variable, Value) :-
		{fail}.


	set_environment_variable(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
		{date(date(Day, Month, Year)), time(time(Hours, Mins, Secs))}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
		{fail}.


	cpu_time(Time) :-
		{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.


	host_name(Name) :-
		{fail}.


:- end_object.
