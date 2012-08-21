
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/6/5,
		comment is 'Operating system interface for K-Prolog.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), system(Command)}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), system(Command)}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{fail}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{dir(Directory, Files, _)}.


	absolute_file_name(File) :-
		{fail}.


	absolute_file_name(File, Full) :-
		{fail}.


	file_exists(File) :-
		{fail}.


	file_modification_time(File, Time) :-
		{fail}.


	file_size(File, Size) :-
		{fail}.


	file_type(File, Type) :-
		{fail}.


	file_permission(File, Permission) :-
		{fail}.
 

	delete_file(File) :-
		{atom_concat('rm ', File, Command),
		 system(Command)}.


	rename_file(Old, New) :-
		{atom_concat('mv ', Old, Temp),
		 atom_concat(Temp, ' ', Temp2),
		 atom_concat(Temp2, New, Command),
		 system(Command)}.


	symbolic_link(File, Target) :-
		{fail}.


	environment_variable(Variable, Value) :-
		{getenv(Variable, Value)}.


	set_environment_variable(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
		{time(Secs, Mins, Hours, Day, Month, Year, _, _, _)}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
		{fail}.


	cpu_time(Time) :-
		{statistics(_, _, _, _, Time, _, _)}.


	host_name(Name) :-
		{fail}.


:- end_object.
