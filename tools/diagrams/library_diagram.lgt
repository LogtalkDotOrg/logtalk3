
:- category(library_diagram(Format),
	extends(diagram(Format))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2016/02/24,
		comment is 'Common predicates for generating library diagrams.',
		parnames is ['Format']
	]).

	:- uses(list, [member/2, memberchk/2]).

	:- protected(remember_included_library/2).
	:- protected(remember_referenced_logtalk_library/2).
	:- protected(remember_referenced_prolog_library/2).

	:- private(included_library_/2).
	:- dynamic(included_library_/2).

	:- private(referenced_logtalk_library_/2).
	:- dynamic(referenced_logtalk_library_/2).

	:- private(referenced_prolog_library_/2).
	:- dynamic(referenced_prolog_library_/2).

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
		Format::graph_header(diagram_output_file, other, '(external libraries)', external, [tooltip('(external libraries)')| Options]),
		::retract(referenced_logtalk_library_(Library, Directory)),
		memberchk(omit_path_prefixes(Prefixes), Options),
		^^add_link_options(Directory, Options, LinkingOptions),
		(	member(Prefix, Prefixes),
			atom_concat(Prefix, Relative, Directory) ->
			^^output_node(Directory, Library, library, [Relative], external_library, LinkingOptions)
		;	^^output_node(Directory, Library, library, [Directory], external_library, LinkingOptions)
		),
		fail.
	output_externals(Options) :-
		::retract(referenced_prolog_library_(Library, Directory)),
		memberchk(omit_path_prefixes(Prefixes), Options),
		^^add_link_options(Directory, Options, LinkingOptions),
		(	member(Prefix, Prefixes),
			atom_concat(Prefix, Relative, Directory) ->
			^^output_node(Directory, Library, library, [Relative], external_library, LinkingOptions)
		;	^^output_node(Directory, Library, library, [Directory], external_library, LinkingOptions)
		),
		fail.
	output_externals(Options) :-
		^^format_object(Format),
		Format::graph_footer(diagram_output_file, other, '(external libraries)', external, [tooltip('(external libraries)')| Options]).

:- end_category.
