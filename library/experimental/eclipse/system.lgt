
:-  use_module(library(calendar)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/6/5,
		comment is 'Operating system interface for ECLiPSe.'
	]).


	make_directory(Directory) :-
		{mkdir(Directory)}.


	delete_directory(Directory) :-
		{delete(Directory)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{getcwd(Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{read_directory(Directory, "*", _, Files)}.


	absolute_file_name(File) :-
		absolute_file_name(File, File).


	absolute_file_name(File, Full) :-
		{Rel == user ->
			Abs == user  % treat user specially
          ; get_flag(prolog_suffix, Sufs),
          (existing_file(Rel, Sufs, [], ExtRel) -> true ; ExtRel = Rel),
          canonical_path_name(ExtRel, Abs)}.


	file_base_name(File, Base) :-
		{pathname(File, _, Base, _)}.


	file_name_extension(File, Extension) :-
		{pathname(File, _, _, Extension)}.


	file_name_directory(File, Directory) :-
		{pathname(File, Directory, _, _)}.


	file_exists(File) :-
		{exists(File)}.


	file_modification_time(File, Time) :-
		{get_file_info(File, mtime, Time)}.


	file_size(File, Size) :-
		{get_file_info(File, size, Size)}.


	file_type(File, Type) :-
		{get_file_info(File, mode, Mode)},
		Mode /\ 8'170000 =:= Result,
		file_mode_type(Result, Type).

	file_mode_type(8'100000, regular) :- !.
	file_mode_type(8'040000, directory) :- !.
	file_mode_type(8'140000, socket) :- !.
	file_mode_type(8'120000, symlink) :- !.
	file_mode_type(_, unknown).


	delete_file(File) :-
		{delete(File)}.


	rename_file(Old, New) :-
		{rename(Old, New)}.


	symbolic_link(File, Target) :-
		{fail}.


	environment_variable(Variable, Value) :-
		{getenv(Variable, Value)}.


	set_environment_variable(Variable, Value) :-
		{concat_string([Variable,"=",Value], String),
		 sepia_kernel:setenv(String)}.


	date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
		{mjd_now(MJD),
		 mjd_to_date(MJD, Day/Month/Year),
		 mjd_to_time(MJD, Hours:Mins:Secs)}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs, _) :-
		{unix_to_mjd(Time, MJD),
		 mjd_to_date(MJD, Day/Month/Year),
		 mjd_to_time(MJD, Hours:Mins:Secs)}.


	cpu_time(Time) :-
		{cputime(Time)}.


	host_name(Name) :-
		{get_flag(hostname, String),
		 atom_string(Name, String)}.


:- end_object.
