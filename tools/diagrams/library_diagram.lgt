
:- category(library_diagram(Format),
	extends(diagram(Format))).

	:- info([
		version is 2.2,
		author is 'Paulo Moura',
		date is 2016/10/13,
		comment is 'Common predicates for generating library diagrams.',
		parnames is ['Format']
	]).

	:- uses(list, [member/2, memberchk/2]).

	:- protected(add_library_documentation_url/4).
	:- mode(add_library_documentation_url(+atom, +list(compound), +atom, -list(compound)), one).
	:- info(add_library_documentation_url/4, [
		comment is 'Adds a documentation URL when using the option url_prefixes/2.',
		argnames is ['Kind', 'Options', 'Directory', 'NodeOptions']
	]).

	:- protected(remember_included_library/2).
	:- protected(remember_referenced_logtalk_library/2).
	:- protected(remember_referenced_prolog_library/2).

	:- private(included_library_/2).
	:- dynamic(included_library_/2).

	:- private(referenced_logtalk_library_/2).
	:- dynamic(referenced_logtalk_library_/2).

	:- private(referenced_prolog_library_/2).
	:- dynamic(referenced_prolog_library_/2).

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

	output_externals(_Options) :-
		::retract(included_library_(Library, Path)),
		::retractall(referenced_logtalk_library_(Library, Path)),
		::retractall(referenced_prolog_library_(Library, Path)),
		fail.
	output_externals(Options) :-
		^^format_object(Format),
		Format::graph_header(diagram_output_file, other, '(external libraries)', external, [urls('',''), tooltip('(external libraries)')| Options]),
		::retract(referenced_logtalk_library_(Library, Directory)),
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		add_library_documentation_url(logtalk, LinkingOptions, Relative, NodeOptions),
		(	memberchk(directory_paths(true), Options) ->
			^^output_node(Relative, Library, library, [Relative], external_library, NodeOptions)
		;	^^output_node(Relative, Library, library, [], external_library, NodeOptions)
		),
		fail.
	output_externals(Options) :-
		::retract(referenced_prolog_library_(Library, Directory)),
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		add_library_documentation_url(prolog, LinkingOptions, Relative, NodeOptions),
		(	memberchk(directory_paths(true), Options) ->
			^^output_node(Relative, Library, library, [Relative], external_library, NodeOptions)
		;	^^output_node(Relative, Library, library, [], external_library, NodeOptions)
		),
		fail.
	output_externals(Options) :-
		^^format_object(Format),
		Format::graph_footer(diagram_output_file, other, '(external libraries)', external, [urls('',''), tooltip('(external libraries)')| Options]).

	add_library_documentation_url(logtalk, Options, Directory, NodeOptions) :-
		(	member(urls(CodePrefix, DocPrefix), Options) ->
			memberchk(entity_url_suffix_target(Suffix, Target), Options),
			atom_concat(DocPrefix, Suffix, URL0),
			atom_concat(URL0, Target, URL1),
			atom_concat(URL1, Directory, URL),
			NodeOptions = [urls(CodePrefix, URL)| Options]
		;	NodeOptions = Options
		).
	% tbd
	add_library_documentation_url(prolog, NodeOptions, _, NodeOptions).

:- end_category.
