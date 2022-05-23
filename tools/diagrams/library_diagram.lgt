
:- category(library_diagram(Format),
	extends(diagram(Format))).

	:- info([
		version is 2:16:0,
		author is 'Paulo Moura',
		date is 2022-05-23,
		comment is 'Common predicates for generating library diagrams.',
		parameters is ['Format' - 'Graph language file format'],
		see_also is [inheritance_diagram(_), uses_diagram(_), xref_diagram(_), entity_diagram(_)]
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- protected(add_library_documentation_url/4).
	:- mode(add_library_documentation_url(+atom, +list(compound), +atom, -list(compound)), one).
	:- info(add_library_documentation_url/4, [
		comment is 'Adds a documentation URL when using the option url_prefixes/2.',
		argnames is ['Kind', 'Options', 'Library', 'NodeOptions']
	]).

	:- protected(remember_included_library/2).
	:- mode(remember_included_library(+atom, +atom), one).
	:- info(remember_included_library/2, [
		comment is 'Remember included Logtalk library in the diagram.',
		argnames is ['Library', 'Path']
	]).

	:- protected(remember_referenced_logtalk_library/2).
	:- mode(remember_referenced_logtalk_library(+atom, +atom), one).
	:- info(remember_referenced_logtalk_library/2, [
		comment is 'Remember referenced Logtalk library in the diagram.',
		argnames is ['Library', 'Path']
	]).

	:- protected(remember_referenced_prolog_library/2).
	:- mode(remember_referenced_prolog_library(+atom, +atom), one).
	:- info(remember_referenced_prolog_library/2, [
		comment is 'Remember referenced Prolog library in the diagram.',
		argnames is ['Library', 'Path']
	]).

	:- private(included_library_/2).
	:- dynamic(included_library_/2).
	:- mode(included_library_(?atom, ?atom), zero_or_more).
	:- info(included_library_/2, [
		comment is 'Table of Logtalk libraries already included in the diagram.',
		argnames is ['Library', 'Path']
	]).

	:- private(referenced_logtalk_library_/2).
	:- dynamic(referenced_logtalk_library_/2).
	:- mode(referenced_logtalk_library_(?atom, ?atom), zero_or_more).
	:- info(referenced_logtalk_library_/2, [
		comment is 'Table of referenced Logtalk libraries in the diagram.',
		argnames is ['Library', 'Path']
	]).

	:- private(referenced_prolog_library_/2).
	:- dynamic(referenced_prolog_library_/2).
	:- mode(referenced_prolog_library_(?atom, ?atom), zero_or_more).
	:- info(referenced_prolog_library_/2, [
		comment is 'Table of referenced Prolog libraries in the diagram.',
		argnames is ['Library', 'Path']
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

	remember_included_library(Library, Path) :-
		(	::included_library_(Library, Path) ->
			true
		;	::assertz(included_library_(Library, Path))
		).

	remember_referenced_logtalk_library(Library, Path) :-
		(	::referenced_logtalk_library_(Library, Path) ->
			true
		;	::assertz(referenced_logtalk_library_(Library, Path))
		).

	remember_referenced_prolog_library(Library, Path) :-
		(	::referenced_prolog_library_(Library, Path) ->
			true
		;	::assertz(referenced_prolog_library_(Library, Path))
		).

	reset :-
		^^reset,
		::retractall(included_library_(_, _)),
		::retractall(referenced_logtalk_library_(_, _)),
		::retractall(referenced_prolog_library_(_, _)).

	output_externals(Options) :-
		member(externals(false), Options),
		!.
	output_externals(_Options) :-
		::retract(included_library_(Library, Path)),
		::retractall(referenced_logtalk_library_(Library, Path)),
		::retractall(referenced_prolog_library_(Library, Path)),
		fail.
	output_externals(Options) :-
		::retract(referenced_logtalk_library_(Library, Directory)),
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		add_library_documentation_url(logtalk, LinkingOptions, Library, NodeOptions),
		(	member(directory_paths(true), Options) ->
			^^output_node(Directory, Library, library, [Relative], external_library, NodeOptions)
		;	^^output_node(Directory, Library, library, [], external_library, NodeOptions)
		),
		fail.
	output_externals(Options) :-
		::retract(referenced_prolog_library_(Library, Directory)),
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		add_library_documentation_url(prolog, LinkingOptions, Library, NodeOptions),
		(	member(directory_paths(true), Options) ->
			^^output_node(Directory, Library, library, [Relative], external_library, NodeOptions)
		;	^^output_node(Directory, Library, library, [], external_library, NodeOptions)
		),
		fail.
	output_externals(_).

	add_library_documentation_url(logtalk, Options, Library, NodeOptions) :-
		(	^^option(urls(_, DocPrefix), Options),
			DocPrefix \== '' ->
			^^option(entity_url_suffix_target(Suffix, Target), Options),
			atomic_list_concat([DocPrefix, Suffix, Target, Library], URL),
			NodeOptions = [url(URL)| Options]
		;	NodeOptions = [url('')| Options]
		).
	% tbd
	add_library_documentation_url(prolog, NodeOptions, _, NodeOptions).

:- end_category.
