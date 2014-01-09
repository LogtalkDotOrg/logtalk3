
:- object(file_diagram(Format),
	imports(diagram(Format))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/08,
		comment is 'Predicates for generating file diagrams.',
		parnames is ['Format']
	]).

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
		Format::graph_header(output_file, other, '(external files)', external, [tooltip('(external files)')| Options]),
		::retract(referenced_logtalk_file_(Path)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		member(omit_path_prefix(Prefix), Options),
		^^linking_options(Path, Options, LinkingOptions),
		(	atom_concat(Prefix, Relative, Directory) ->
			^^output_node(Path, Basename, [Relative], external_file, LinkingOptions)
		;	^^output_node(Path, Basename, [Directory], external_file, LinkingOptions)
		),
		fail.
	output_externals(Options) :-
		::retract(referenced_prolog_file_(Path)),
		prolog_modules_diagram_support::source_file_property(Path, directory(Directory)),
		prolog_modules_diagram_support::source_file_property(Path, basename(Basename)),
		member(omit_path_prefix(Prefix), Options),
		^^linking_options(Path, Options, LinkingOptions),
		(	atom_concat(Prefix, Relative, Directory) ->
			^^output_node(Path, Basename, [Relative], external_file, LinkingOptions)
		;	^^output_node(Path, Basename, [Directory], external_file, LinkingOptions)
		),
		fail.
	output_externals(Options) :-
		^^format_object(Format),
		Format::graph_footer(output_file, other, '(external files)', external, [tooltip('(external files)')| Options]).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
