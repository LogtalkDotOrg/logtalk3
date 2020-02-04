
:- category(directory_diagram(Format),
	extends(diagram(Format))).

	:- info([
		version is 1:11:0,
		author is 'Paulo Moura',
		date is 2019-06-13,
		comment is 'Common predicates for generating directory diagrams.',
		parameters is ['Format' - 'Graph language file format']
	]).

	:- uses(list, [
		member/2
	]).

	:- protected(remember_included_directory/1).
	:- protected(remember_referenced_logtalk_directory/1).
	:- protected(remember_referenced_prolog_directory/1).

	:- private(included_directory_/1).
	:- dynamic(included_directory_/1).

	:- private(referenced_logtalk_directory_/1).
	:- dynamic(referenced_logtalk_directory_/1).

	:- private(referenced_prolog_directory_/1).
	:- dynamic(referenced_prolog_directory_/1).

	files(Project, Files, UserOptions) :-
		files_directories(Files, Directories),
		::directories(Project, Directories, UserOptions).

	files_directories(Files, Directories) :-
		files_directories_bag(Files, Directories0),
		sort(Directories0, Directories).

	files_directories_bag([], []).
	files_directories_bag([File| Files], [Directory| Directories]) :-
		^^locate_file(File, _, _, Directory, _),
		files_directories_bag(Files, Directories).

	remember_included_directory(Path) :-
		(	::included_directory_(Path) ->
			true
		;	::assertz(included_directory_(Path))
		).

	remember_referenced_logtalk_directory(Path) :-
		(	::referenced_logtalk_directory_(Path) ->
			true
		;	::assertz(referenced_logtalk_directory_(Path))
		).

	remember_referenced_prolog_directory(Path) :-
		(	::referenced_prolog_directory_(Path) ->
			true
		;	::assertz(referenced_prolog_directory_(Path))
		).

	reset :-
		^^reset,
		::retractall(included_directory_(_)),
		::retractall(referenced_logtalk_directory_(_)),
		::retractall(referenced_prolog_directory_(_)).

	output_externals(Options) :-
		member(externals(false), Options),
		!.
	output_externals(_Options) :-
		::retract(included_directory_(Path)),
		::retractall(referenced_logtalk_directory_(Path)),
		::retractall(referenced_prolog_directory_(Path)),
		fail.
	output_externals(Options) :-
		::retract(referenced_logtalk_directory_(Directory)),
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		^^output_node(Directory, Relative, directory, [], external_directory, LinkingOptions),
		fail.
	output_externals(Options) :-
		::retract(referenced_prolog_directory_(Directory)),
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		^^output_node(Directory, Relative, directory, [], external_directory, LinkingOptions),
		fail.
	output_externals(_).

:- end_category.
