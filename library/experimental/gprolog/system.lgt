
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/6/5,
		comment is 'Operating system interface for GNU Prolog.']).


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
		 file_property(File, type(directory))}.


	directory_files(Directory, Files) :-
		{directory_files(Directory, Files)}.


	absolute_file_name(File) :-
		{absolute_file_name(File, File)}.


	absolute_file_name(File, Full) :-
		{absolute_file_name(File, Full)}.


	file_base_name(File, Base) :-
		{decompose_file_name(File, _, Base, _)}.


	file_name_extension(File, Extension) :-
		{decompose_file_name(File, _, _, Extension)}.


	file_name_directory(File, Directory) :-
		{decompose_file_name(File, Directory, _, _)}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modification_time(File, Time) :-
		{fail}.


	file_size(File, Size) :-
		{file_property(File, size(Size))}.


	file_type(File, Type) :-
		{file_property(File, type(Type))}.


	file_permission(File, Permission) :-
		{file_permission(File, Permission)}.
 

	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	symbolic_link(File, Target) :-
		{file_property(File, real_file_name(Target))}.


	environment_variable(Variable, Value) :-
		{environ(Variable, Value)}.


	set_environment_variable(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
		{date_time(dt(Year, Month, Day, Hours, Mins, Secs))}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
		{fail}.


	cpu_time(Time) :-
		{cpu_time(Miliseconds),
		 Time is Miliseconds/1000}.


	host_name(Name) :-
		{host_name(Name)}.


:- end_object.
