
:- use_module(library(system)).
:- use_module(library(filenames)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/6/5,
		comment is 'Operating system interface for Ciao.']).


	make_directory(Directory) :-
		{make_directory(Directory)}.


	delete_directory(Directory) :-
		{delete_directory(Directory)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{working_directory(Directory, Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{directory_files(Directory, Files)}.


	absolute_file_name(File) :-
		{absolute_file_name(File, File)}.


	absolute_file_name(File, Full) :-
		{absolute_file_name(File, Full)}.


	file_base_name(File, Base) :-
		{no_path_file_name_(File, Name),
		 file_name_extension(Name, Base, _)}.


	file_name_extension(File, Extension) :-
		{no_path_file_name_(File, Name),
		 file_name_extension(Name, _, Extension)}.


	file_name_directory(File, Directory) :-
		{no_path_file_name_(File, Name),
		 absolute_file_name(File, Full),
		 atom_concat(Directory, Name, Full)}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modification_time(File, Time) :-
		{modif_time(File, Time)}.


	file_size(File, Size) :-
		{file_property(File, size(Size))}.


	file_type(File, Type) :-
		{file_property(File, type(Type))}.


	file_permission(File, Permission) :-
		{file_exists(File, Permission)}.
 

	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	symbolic_link(File, Target) :-
		{file_property(File, linkto(Target))}.


	environment_variable(Variable, Value) :-
		{getenvstr(Variable, Codes),
		 atom_codes(Value, Codes)}.


	set_environment_variable(Variable, Value) :-
		{atom_codes(Value, Codes),
		 setenvstr(Variable, Codes)}.


	date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
		{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
		{datime(Time, Year, Month, Day, Hours, Mins, Secs, _, _)}.


	cpu_time(Time) :-
		{statistics(runtime, [Miliseconds| _]),
		 Time is Miliseconds/1000}.


	host_name(Name) :-
		{current_host(Name)}.


:- end_object.
