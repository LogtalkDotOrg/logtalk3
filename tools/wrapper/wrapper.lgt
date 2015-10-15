%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(wrapper,
	implements(expanding)).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2015/10/15,
		comment is 'Simple tool for helping porting plain Prolog code.'
	]).

	:- public(advise/1).
	:- mode(advise(+list(atom)), one).
	:- info(advise/1, [
		comment is 'Advise the user on missing directives for converting a list of plain Prolog files in Logtalk objects.',
		argnames is ['Files']
	]).

	:- private(unknown_predicate_called_but_not_defined_/2).
	:- dynamic(unknown_predicate_called_but_not_defined_/2).
	:- mode(unknown_predicate_called_but_not_defined_(?atom, ?predicate_indicator), zero_or_more).
	:- info(unknown_predicate_called_but_not_defined_/2, [
		comment is 'Table of predicates called but not defined.',
		argnames is ['Object', 'Predicate']
	]).

	:- private(missing_predicate_directive_/3).
	:- dynamic(missing_predicate_directive_/3).
	:- mode(missing_predicate_directive_(?atom, ?predicate_indicator, ?predicate_indicator), zero_or_more).
	:- info(missing_predicate_directive_/3, [
		comment is 'Table of missing predicate directives.',
		argnames is ['Object', 'Directive', 'Predicate']
	]).

	:- private(non_standard_predicate_call_/2).
	:- dynamic(non_standard_predicate_call_/2).
	:- mode(non_standard_predicate_call_(?atom, ?predicate_indicator), zero_or_more).
	:- info(non_standard_predicate_call_/2, [
		comment is 'Table of called non-standard predicates.',
		argnames is ['Object', 'Predicate']
	]).

	:- private(wrapper_object_/2).
	:- dynamic(wrapper_object_/2).
	:- mode(wrapper_object_(?atom, ?atom), zero_or_more).
	:- info(wrapper_object_/2, [
		comment is 'Table of converted files and corresponding objects.',
		argnames is ['Object', 'File']
	]).

	:- private(dynamic_directive_/3).
	:- dynamic(dynamic_directive_/3).
	:- mode(dynamic_directive_(?atom, ?integer, ?predicate_indicator), zero_or_more).
	:- info(dynamic_directive_/3, [
		comment is 'Table of declared dynamic predicates.',
		argnames is ['Object', 'Line', 'Predicate']
	]).

	:- private(multifile_directive_/3).
	:- dynamic(multifile_directive_/3).
	:- mode(multifile_directive_(?atom, ?integer, ?predicate_indicator), zero_or_more).
	:- info(multifile_directive_/3, [
		comment is 'Table of declared multifile predicates.',
		argnames is ['Object', 'Line', 'Predicate']
	]).

	:- private(add_directive_/2).
	:- dynamic(add_directive_/2).
	:- mode(add_directive_(?atom, ?predicate_indicator), zero_or_more).
	:- info(add_directive_/2, [
		comment is 'Table of directives to be added.',
		argnames is ['Object', 'Directive']
	]).

	:- private(add_directive_/3).
	:- dynamic(add_directive_/3).
	:- mode(add_directive_(?atom, ?predicate_indicator, ?predicate_indicator), zero_or_more).
	:- info(add_directive_/3, [
		comment is 'Table of directives to be added to complement existing directives.',
		argnames is ['Object', 'Directive', 'NewDirective']
	]).

	% load the plain Prolog files and advise on changes

	advise(Files) :-
		clean_issues_databases,
		load_and_wrap_files(Files),
		generate_advise,
		print_advise.

	clean_issues_databases :-
		retractall(unknown_predicate_called_but_not_defined_(_, _)),
		retractall(missing_predicate_directive_(_, _, _)),
		retractall(non_standard_predicate_call_(_, _)),
		retractall(dynamic_directive_(_,_,_)),
		retractall(multifile_directive_(_,_,_)),
		retractall(wrapper_object_(_, _)),
		retractall(add_directive_(_, _)),
		retractall(add_directive_(_, _, _)).

	load_and_wrap_files([]).
	load_and_wrap_files([File| Files]) :-
		load_and_wrap_file(File),
		load_and_wrap_files(Files).

	load_and_wrap_file(File) :-
		this(This),
		(	os::file_exists(File) ->
			logtalk_load(File, [hook(This), source_data(on), portability(warning)])
		;	logtalk::print_message(warning, wrapper, file_not_found(File))
		).

	generate_advise :-
		wrapper_object_(Object, _),
		missing_public_directives_advise(Object),
		missing_private_directives_advise(Object),
		missing_predicate_directives_advise(Object),
		missing_uses_directives_advise(Object),
		fail.
	generate_advise.		

	print_advise :-
		wrapper_object_(Object, File),
		logtalk::print_message(information, wrapper, advise_for_file(File)),
		print_add_advise(Object),
		fail.
	print_advise.

	print_add_advise(Object) :-
		\+ \+ add_directive_(Object, _),
		logtalk::print_message(information(code), wrapper, add_directives),
		add_directive_(Object, Directive),
		logtalk::print_message(information(code), wrapper, add_directive(Directive)),
		fail.
	print_add_advise(Object) :-
		\+ \+ add_directive_(Object, _, _),
		add_directive_(Object, Directive, NewDirective),
		logtalk::print_message(information(code), wrapper, add_directive(Directive, NewDirective)),
		fail.
	print_replace_advise(_).

	% predicates called from other files wrapped as objects
	% must be declared public

	missing_public_directives_advise(Object) :-
		setof(
			Predicate,
			provides_used_predicate(Object, Predicate),
			Predicates
		),
		Directive =.. [(public), Predicates],
		assertz(add_directive_(Object, Directive)),
		!.
	missing_public_directives_advise(_).

	provides_used_predicate(Object, Predicate) :-
		unknown_predicate_called_but_not_defined_(Other, Predicate),
		Other \== Object,
		object_property(Object, defines(Predicate, _)),
		\+ (
			object_property(Object, declares(Predicate, Properties)),
			member((multifile), Properties)
		).

	% internal dynamic predicates shoudl also be
	% declared private for improved performance

	missing_private_directives_advise(Object) :-
		setof(
			Predicate,
			internal_dynamic_predicate(Object, Predicate),
			Predicates
		),
		Directive =.. [(private), Predicates],
		assertz(add_directive_(Object, Directive)),
		!.
	missing_private_directives_advise(_).

	internal_dynamic_predicate(Object, Predicate) :-
		dynamic_directive_(Object, _, Predicate),
		\+ unknown_predicate_called_but_not_defined_(_, Predicate).

	% missing public/1 directives are only generated for
	% predicates declared as multifile
	missing_predicate_directives_advise(Object) :-
		missing_predicate_directive_(Object, (public), Predicate),
		PublicDirective =.. [(public), Predicate],
		MultifileDirective =.. [(multifile), Predicate],
		assertz(add_directive_(Object, MultifileDirective, PublicDirective)),
		fail.
	% other missing directives
	missing_predicate_directives_advise(Object) :-
		missing_predicate_directive_(Object, DirectiveFunctor, Predicate),
		DirectiveFunctor \== (public),
		Directive =.. [DirectiveFunctor, Predicate],
		assertz(add_directive_(Object, Directive)),
		fail.
	missing_predicate_directives_advise(_).

	% generate uses/2 directives for resolving now implicitly
	% qualified calls to predicates defined in other files

	missing_uses_directives_advise(Object) :-
		missing_uses_directive(Object, Other, Predicates),
		Directive =.. [uses, Other, Predicates],
		assertz(add_directive_(Object, Directive)),
		fail.
	missing_uses_directives_advise(_).

	missing_uses_directive(Object, Other, Predicates) :-
		setof(
			Predicate,
			unknown_predicate_called(Object, Other, Predicate),
			Predicates
		).

	% for non-standard built-in predicates, just call them
	% in the context of the "user" pseudo-object

	unknown_predicate_called(Object, user, Predicate) :-
		non_standard_predicate_call_(Object, Predicate).

	unknown_predicate_called(Object, Other, Predicate) :-
		unknown_predicate_called_but_not_defined_(Object, Predicate),
		object_property(Other, defines(Predicate, _)),
		wrapper_object_(Other, _).

	% wrapper for the plain Prolog files source code

	term_expansion(begin_of_file, [(:- object(Object))]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.pl', Basename).

	term_expansion(end_of_file, [(:- end_object), end_of_file]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.pl', Basename),
		logtalk_load_context(file, File),
		assertz(wrapper_object_(Object, File)).

	% special cases

	% save the position of dynamic/1 directives
	term_expansion((:- dynamic(Predicates)), [(:- dynamic(Predicates))]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.pl', Basename),
		logtalk_load_context(term_position, Line-_),
		flatten_to_list(Predicates, List),
		forall(
			member(Predicate, List),
			assertz(dynamic_directive_(Object, Line, Predicate))
		),
		fail.

	% save the position of multifile/1 directives
	term_expansion((:- multifile(Predicates)), [(:- multifile(Predicates))]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.pl', Basename),
		logtalk_load_context(term_position, Line-_),
		flatten_to_list(Predicates, List),
		forall(
			member(Predicate, List),
			assertz(multifile_directive_(Object, Line, Predicate))
		),
		fail.

	% discard include/1 directives for files already being processed
	term_expansion((:- include(Path)), []) :-
		os::expand_path(Path, ExpandedPath),
		wrapper_object_(_, File),
		% file extension might be missing
		atom_concat(ExpandedPath, _, File).

	% discard ensure_loaded/1 directives for files already being processed
	term_expansion((:- ensure_loaded(Path)), []) :-
		os::expand_path(Path, ExpandedPath),
		wrapper_object_(_, File),
		% file extension might be missing
		atom_concat(ExpandedPath, _, File).

	% hooks for intercepting relevant compiler lint messages

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(unknown_predicate_called_but_not_defined(_, _, _, Object, Predicate), _, core, _) :-
		assertz(unknown_predicate_called_but_not_defined_(Object, Predicate)).

	logtalk::message_hook(missing_predicate_directive(_, _, _, Object, DirectiveFunctor, Predicate), _, core, _) :-
		assertz(missing_predicate_directive_(Object, DirectiveFunctor, Predicate)).

	logtalk::message_hook(non_standard_predicate_call(_, _, _, Object, Predicate), _, core, _) :-
		assertz(non_standard_predicate_call_(Object, Predicate)).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(information, wrapper, '% ', user_output).
	logtalk::message_prefix_stream(information(code), wrapper, '', user_output).
	logtalk::message_prefix_stream(warning, wrapper, '*     ', user_error).

	% wraper messages

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(file_not_found(File), wrapper) -->
		['File not found: ~w'-[File], nl].

	logtalk::message_tokens(advise_for_file(File), wrapper) -->
		[nl, 'Advise for file: ~w'-[File], nl, nl].

	logtalk::message_tokens(public_directive(Predicates), wrapper) -->
		[':- public(~q).'-[Predicates], nl, nl].

	logtalk::message_tokens(add_directives, wrapper) -->
		['% Add the following directives:'-[], nl, nl].

	logtalk::message_tokens(replace_directives, wrapper) -->
		['% Replace the following directives:'-[], nl, nl].

	logtalk::message_tokens(add_directive(Directive), wrapper) -->
		[':- ~q.'-[Directive], nl, nl].

	logtalk::message_tokens(add_directive(Directive, NewDirective), wrapper) -->
		[	
			'% before the directive:'-[], nl,
			'% :- ~q'-[Directive], nl,
			'% add the directive:'-[], nl, nl,
			':- ~q.'-[NewDirective], nl, nl
		].

	logtalk::message_tokens(uses_directive(Object, Predicates), wrapper) -->
		[':- uses(~q, ~q).'-[Object, Predicates], nl, nl].

	directives_tokens([]) -->
		[].
	directives_tokens([Directive| Directives]) -->
		[':- ~q'-[Directive], nl],
		directives_tokens(Directives).

	% auxiliary predicates

	% flattens an item, a list of items, or a conjunction of items into a list
	flatten_to_list([A| B], [A| B]) :-
		!.
	flatten_to_list([], []) :-
		!.
	flatten_to_list((A, B), [A| BB]) :-
		!,
		flatten_to_list(B, BB).
	flatten_to_list(A, [A]).

	% we want to minimize any dependencies on other entities, including library objects

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

:- end_object.
