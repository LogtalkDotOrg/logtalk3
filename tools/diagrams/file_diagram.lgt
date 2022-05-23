
:- category(file_diagram(Format),
	extends(diagram(Format))).

	:- info([
		version is 2:13:0,
		author is 'Paulo Moura',
		date is 2022-05-23,
		comment is 'Common predicates for generating file diagrams.',
		parameters is ['Format' - 'Graph language file format']
	]).

	:- uses(list, [
		member/2
	]).

	:- protected(remember_included_file/1).
	:- mode(remember_included_file(+atom), one).
	:- info(remember_included_file/1, [
		comment is 'Remember included Logtalk file in the diagram.',
		argnames is ['Path']
	]).

	:- protected(remember_referenced_logtalk_file/1).
	:- mode(remember_referenced_logtalk_file(+atom), one).
	:- info(remember_referenced_logtalk_file/1, [
		comment is 'Remember referenced Logtalk file in the diagram.',
		argnames is ['Path']
	]).

	:- protected(remember_referenced_prolog_file/1).
	:- mode(remember_referenced_prolog_file(+atom), one).
	:- info(remember_referenced_prolog_file/1, [
		comment is 'Remember referenced Prolog file in the diagram.',
		argnames is ['Path']
	]).

	:- private(included_file_/1).
	:- dynamic(included_file_/1).
	:- mode(included_file_(?atom), zero_or_more).
	:- info(included_file_/1, [
		comment is 'Table of Logtalk files already included in the diagram.',
		argnames is ['Path']
	]).

	:- private(referenced_logtalk_file_/1).
	:- dynamic(referenced_logtalk_file_/1).
	:- mode(referenced_logtalk_file_(?atom), zero_or_more).
	:- info(referenced_logtalk_file_/1, [
		comment is 'Table of referenced Logtalk files in the diagram.',
		argnames is ['Path']
	]).

	:- private(referenced_prolog_file_/1).
	:- dynamic(referenced_prolog_file_/1).
	:- mode(referenced_prolog_file_(?atom), zero_or_more).
	:- info(referenced_prolog_file_/1, [
		comment is 'Table of referenced Prolog files in the diagram.',
		argnames is ['Path']
	]).

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
