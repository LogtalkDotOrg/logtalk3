
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/12/1,
		comment is 'Operating system interface for Qu-Prolog.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), os(system(Command))}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), os(system(Command))}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{getcwd(Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{fail}.


	absolute_file_name(File) :-
		{fail}.


	absolute_file_name(File, Full) :-
		{fail}.


	file_exists(File) :-
		{access(File, 0, 0)}.


	file_modification_time(File, Time) :-
		{fail}.


	file_size(File, Size) :-
		{fail}.


	file_type(File, Type) :-
		{fail}.


	file_permission(File, read) :-
		{access(File, 4, 0)}.

	file_permission(File, write) :-
		{access(File, 2, 0)}.

	file_permission(File, execute) :-
		{access(File, 1, 0)}.
 

	delete_file(File) :-
		{atom_concat('rm ', File, Command), os(system(Command))}.


	rename_file(Old, New) :-
		{concat_atom([mv, Old, New], ' ', Command), os(system(Command))}.


	symbolic_link(File, Target) :-
		{fail}.


	environment_variable(Variable, Value) :-
		{fail}.


	set_environment_variable(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
		{realtime(RT), localtime(RT, LT), LT = local_time(Year, Month, Day, Hours, Mins, Secs)}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs, _) :-
		{localtime(Time, LT), LT = local_time(Year, Month, Day, Hours, Mins, Secs)}.


	cpu_time(Time) :-
		{statistics(runtime, [Start,_]), Time is Start/1000}.


	host_name(Name) :-
		{fail}.


:- end_object.
