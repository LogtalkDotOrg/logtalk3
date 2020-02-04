
:- category(file_diagram(Format),
	extends(diagram(Format))).

	:- info([
		version is 2:12:0,
		author is 'Paulo Moura',
		date is 2019-06-13,
		comment is 'Common predicates for generating file diagrams.',
		parameters is ['Format' - 'Graph language file format']
	]).

	:- uses(list, [
		member/2
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

	output_externals(Options) :-
		member(externals(false), Options),
		!.
	output_externals(_Options) :-
		::retract(included_file_(Path)),
		::retractall(referenced_logtalk_file_(Path)),
		::retractall(referenced_prolog_file_(Path)),
		fail.
	output_externals(Options) :-
		::retract(referenced_logtalk_file_(Path)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		^^filter_file_extension(Basename, Options, Name),
		^^add_link_options(Path, Options, LinkingOptions),
		^^omit_path_prefix(Path, Options, Relative),
		(	member(directory_paths(true), Options) ->
			^^output_node(Path, Name, file, [Relative], external_file, LinkingOptions)
		;	^^output_node(Path, Name, file, [], external_file, LinkingOptions)
		),
		fail.
	output_externals(Options) :-
		::retract(referenced_prolog_file_(Path)),
		modules_diagram_support::loaded_file_property(Path, basename(Basename)),
		^^filter_file_extension(Basename, Options, Name),
		^^add_link_options(Path, Options, LinkingOptions),
		^^omit_path_prefix(Path, Options, Relative),
		(	member(directory_paths(true), Options) ->
			^^output_node(Path, Name, file, [Relative], external_file, LinkingOptions)
		;	^^output_node(Path, Name, file, [], external_file, LinkingOptions)
		),
		fail.
	output_externals(_).

:- end_category.
