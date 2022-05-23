
:- category(directory_diagram(Format),
	extends(diagram(Format))).

	:- info([
		version is 1:12:0,
		author is 'Paulo Moura',
		date is 2022-05-23,
		comment is 'Common predicates for generating directory diagrams.',
		parameters is ['Format' - 'Graph language file format']
	]).

	:- uses(list, [
		member/2
	]).

	:- protected(remember_included_directory/1).
	:- mode(remember_included_directory(+atom), one).
	:- info(remember_included_directory/1, [
		comment is 'Remember included Logtalk directory in the diagram.',
		argnames is ['Path']
	]).

	:- protected(remember_referenced_logtalk_directory/1).
	:- mode(remember_referenced_logtalk_directory(+atom), one).
	:- info(remember_referenced_logtalk_directory/1, [
		comment is 'Remember referenced Logtalk directory in the diagram.',
		argnames is ['Path']
	]).

	:- protected(remember_referenced_prolog_directory/1).
	:- mode(remember_referenced_prolog_directory(+atom), one).
	:- info(remember_referenced_prolog_directory/1, [
		comment is 'Remember referenced Prolog directory in the diagram.',
		argnames is ['Path']
	]).

	:- private(included_directory_/1).
	:- dynamic(included_directory_/1).
	:- mode(included_directory_(?atom), zero_or_more).
	:- info(included_directory_/1, [
		comment is 'Table of Logtalk directories already included in the diagram.',
		argnames is ['Path']
	]).

	:- private(referenced_logtalk_directory_/1).
	:- dynamic(referenced_logtalk_directory_/1).
	:- mode(referenced_logtalk_directory_(?atom), zero_or_more).
	:- info(referenced_logtalk_directory_/1, [
		comment is 'Table of referenced Logtalk directories in the diagram.',
		argnames is ['Path']
	]).

	:- private(referenced_prolog_directory_/1).
	:- dynamic(referenced_prolog_directory_/1).
	:- mode(referenced_prolog_directory_(?atom), zero_or_more).
	:- info(referenced_prolog_directory_/1, [
		comment is 'Table of referenced Prolog directories in the diagram.',
		argnames is ['Path']
	]).

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
