
:- category(file_diagram(Format),
	extends(diagram(Format))).

	:- info([
		version is 2.1,
		author is 'Paulo Moura',
		date is 2016/02/29,
		comment is 'Common predicates for generating file diagrams.',
		parnames is ['Format']
	]).

	:- uses(list, [member/2, memberchk/2]).

	:- protected(remember_included_file/1).
	:- protected(remember_referenced_logtalk_file/1).
	:- protected(remember_referenced_prolog_file/1).

	:- private(included_file_/1).
	:- dynamic(included_file_/1).

	:- private(referenced_logtalk_file_/1).
	:- dynamic(referenced_logtalk_file_/1).

	:- private(referenced_prolog_file_/1).
	:- dynamic(referenced_prolog_file_/1).

	remember_included_file(Path) :-
		(	::included_file_(Path) ->
			true
		;	::assertz(included_file_(Path))
		).

	remember_referenced_logtalk_file(Path) :-
		(	::referenced_logtalk_file_(Path) ->
			true
		;	::assertz(referenced_logtalk_file_(Path))
		).

	remember_referenced_prolog_file(Path) :-
		(	::referenced_prolog_file_(Path) ->
			true
		;	::assertz(referenced_prolog_file_(Path))
		).

	reset :-
		^^reset,
		::retractall(included_file_(_)),
		::retractall(referenced_logtalk_file_(_)),
		::retractall(referenced_prolog_file_(_)).

	output_externals(_Options) :-
		::retract(included_file_(Path)),
		::retractall(referenced_logtalk_file_(Path)),
		::retractall(referenced_prolog_file_(Path)),
		fail.
	output_externals(Options) :-
		^^format_object(Format),
		Format::graph_header(diagram_output_file, other, '(external files)', external, [tooltip('(external files)')| Options]),
		::retract(referenced_logtalk_file_(Path)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		^^add_link_options(Path, Options, LinkingOptions),
		^^omit_path_prefix(Path, Options, Relative),
		(	memberchk(directory_paths(true), Options) ->
			^^output_node(Relative, Basename, file, [Relative], external_file, LinkingOptions)
		;	^^output_node(Relative, Basename, file, [], external_file, LinkingOptions)
		),
		fail.
	output_externals(Options) :-
		::retract(referenced_prolog_file_(Path)),
		modules_diagram_support::loaded_file_property(Path, basename(Basename)),
		^^add_link_options(Path, Options, LinkingOptions),
		^^omit_path_prefix(Path, Options, Relative),
		(	memberchk(directory_paths(true), Options) ->
			^^output_node(Relative, Basename, file, [Relative], external_file, LinkingOptions)
		;	^^output_node(Relative, Basename, file, [], external_file, LinkingOptions)
		),
		fail.
	output_externals(Options) :-
		^^format_object(Format),
		Format::graph_footer(diagram_output_file, other, '(external files)', external, [tooltip('(external files)')| Options]).

:- end_category.
