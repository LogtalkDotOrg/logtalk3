%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(xref_diagram(Format),
	extends(entity_diagram(Format))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/12/30,
		comment is 'Predicates for generating predicate call cross-referencing diagrams.',
		parnames is ['Format']
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- public(entity/2).
	:- mode(entity(+atom, +list(compound)), one).
	:- info(entity/2, [
		comment is 'Creates a diagram for a single entity using the specified options.',
		argnames is ['Entity', 'Options']
	]).

	:- public(entity/1).
	:- mode(entity(+atom), one).
	:- info(entity/1, [
		comment is 'Creates a diagram for a single entity using default options.',
		argnames is ['Entity']
	]).

	:- private(included_predicate_/1).
	:- dynamic(included_predicate_/1).

	:- private(referenced_predicate_/1).
	:- dynamic(referenced_predicate_/1).

	:- private(external_predicate_/1).
	:- dynamic(external_predicate_/1).

	entity(Entity, UserOptions) :-
		entity_kind(Entity, Kind, GroundEntity, Name),
		atom_concat(Name, '_', Identifier0),
		atom_concat(Identifier0, Kind, Identifier),
		^^format_object(Format),
		^^merge_options(UserOptions, Options),
		reset,
		^^output_file_path(Identifier, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::file_header(output_file, Identifier, Options),
		entity_property(Kind, Entity, file(Basename, Directory)),
		atom_concat(Directory, Basename, Path),
		^^add_link_options(Path, Options, GraphOptions),
		Format::graph_header(output_file, Identifier, GroundEntity, entity, GraphOptions),
		process(Kind, Entity, Options),
		output_external_predicates(Options),
		^^output_edges(Options),
		Format::graph_footer(output_file, Identifier, GroundEntity, entity, GraphOptions),
		Format::file_footer(output_file, Identifier, Options),
		close(Stream).

	entity(Entity) :-
		entity(Entity, []).

	entity_kind(Entity, Kind, GroundEntity, Name) :-
		(	current_object(Entity) ->
			Kind = object
		;	current_category(Entity) ->
			Kind = category
		;	current_protocol(Entity) ->
			Kind = protocol
		;	atom(Entity),
			current_logtalk_flag(modules, supported),
			{current_module(Entity)},
			Kind = module
		),
		(	atom(Entity) ->
			GroundEntity = Entity,
			Name = Entity
		;	^^ground_entity_identifier(Kind, Entity, GroundEntity),
			functor(GroundEntity, Functor, Arity),
			number_chars(Arity, Chars),
			atom_chars(ArityAtom, Chars),
			atom_concat(Functor, '_', Name0),
			atom_concat(Name0, ArityAtom, Name)
		).

	process(Kind, Entity, Options) :-
		entity_property(Kind, Entity, declares(Predicate0, Properties)),
		(	member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		),
		predicate_kind_caption(Kind, Properties, PredicateKind, Caption),
		add_predicate_documentation_url(Options, Entity, Predicate, PredicateOptions),
		^^output_node(Predicate, Predicate, Caption, [], PredicateKind, PredicateOptions),
		assertz(included_predicate_(Predicate)),
		fail.
	process(Kind, Entity, Options) :-
		Kind \== protocol,
		entity_property(Kind, Entity, defines(Predicate0, Properties)),
		\+ entity_property(Kind, Entity, declares(Predicate0, _)),
		\+ member(auxiliary, Properties),
		(	member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		),
		^^output_node(Predicate, Predicate, '', [], predicate, Options),
		assertz(included_predicate_(Predicate)),
		fail.
	process(Kind, Entity, Options) :-
		Kind \== protocol,
		entity_property(Kind, Entity, provides(Predicate, To, Properties)),
		\+ member(auxiliary, Properties),
		(	Kind == module ->
			^^output_node(':'(To,Predicate), ':'(To,Predicate), (multifile), [], multifile_predicate, Options),
			assertz(included_predicate_(':'(To,Predicate)))
		;	add_predicate_documentation_url(Options, Entity, To::Predicate, PredicateOptions),
			^^output_node(To::Predicate, To::Predicate, (multifile), [], multifile_predicate, PredicateOptions),
			assertz(included_predicate_(To::Predicate))
		),
		fail.
	process(Kind, Entity, Options) :-
		calls_local_predicate(Kind, Entity, Caller, Callee),
		\+ ^^edge(Caller, Callee, [calls], calls_predicate, _),
		remember_referenced_predicate(Caller),
		remember_referenced_predicate(Callee),
		^^save_edge(Caller, Callee, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	process(Kind, Entity, Options) :-
		calls_external_predicate(Kind, Entity, Caller, Callee),
		remember_external_predicate(Callee),
		\+ ^^edge(Caller, Callee, [calls], calls_predicate, _),
		^^save_edge(Caller, Callee, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	process(_, _, _) :-
		retract(included_predicate_(Predicate)),
		retractall(referenced_predicate_(Predicate)),
		fail.
	process(_, _, Options) :-
		retract(referenced_predicate_(Functor/Arity)),
		^^output_node(Functor/Arity, Functor/Arity, '', [], predicate, Options),
		fail.
	process(_, _, _).

	predicate_kind_caption(module, Properties, PredicateKind, Caption) :-
		!,
		(	member((multifile), Properties) ->
			PredicateKind = multifile_predicate,
			(	member((dynamic), Properties) ->
				Caption = 'exported, multifile, dynamic'
			;	Caption = 'exported, multifile'
			)
		;	PredicateKind = exported_predicate,
			Caption = exported
		).
	predicate_kind_caption(_, Properties, PredicateKind, Caption) :-
		(	member((multifile), Properties), member((public), Properties) ->
			PredicateKind = multifile_predicate,
			(	member((dynamic), Properties) ->
				Caption = 'public, multifile, dynamic'
			;	member(synchronized, Properties) ->
				Caption = 'public, multifile, synchronized'
			;	Caption = 'public, multifile'
			)
		;	memberchk(scope(Scope), Properties),
			scope_predicate_kind(Scope, PredicateKind),
			(	member((dynamic), Properties) ->
				atom_concat(Scope, ', dynamic', Caption)
			;	member(synchronized, Properties) ->
				atom_concat(Scope, ', synchronized', Caption)
			;	Caption = Scope
			)
		).

	scope_predicate_kind(public, public_predicate).
	scope_predicate_kind(protected, protected_predicate).
	scope_predicate_kind(private, private_predicate).

	add_predicate_documentation_url(Options, _, Entity::Functor/Arity, PredicateOptions) :-
		!,
		add_predicate_documentation_url(Options, Entity, Functor/Arity, PredicateOptions).
	add_predicate_documentation_url(Options, _, Entity::Functor//Arity, PredicateOptions) :-
		!,
		add_predicate_documentation_url(Options, Entity, Functor//Arity, PredicateOptions).
	add_predicate_documentation_url(Options, Entity, Predicate, PredicateOptions) :-
		!,
		(	member(url_prefixes(FilePrefix, DocPrefix), Options) ->
			functor(Entity, EntityFunctor, EntityArity),
			atom_concat(DocPrefix, EntityFunctor, URL0),
			atom_concat(URL0, '_', URL1),
			number_codes(EntityArity, EntityArityCodes),
			atom_codes(EntityArityAtom, EntityArityCodes),
			atom_concat(URL1, EntityArityAtom, URL2),
			memberchk(entity_url_suffix_target(Suffix, Target), Options),
			(	Target == '' ->
				atom_concat(URL2, Suffix, URL)
			;	atom_concat(URL2, Suffix, URL3),
				atom_concat(URL3, Target, URL4),
				(	Predicate = Functor/Arity ->
					atom_concat(URL4, Functor, URL5),
					atom_concat(URL5, '/', URL6)
				;	Predicate = Functor//Arity,
					atom_concat(URL4, Functor, URL5),
					atom_concat(URL5, '//', URL6)
				),
				number_codes(Arity, ArityCodes),
				atom_codes(ArityAtom, ArityCodes),
				atom_concat(URL6, ArityAtom, URL)
			),
			PredicateOptions = [urls(FilePrefix, URL)| Options]
		;	PredicateOptions = Options
		).
	add_predicate_documentation_url(Options, _, Options).

	calls_local_predicate(module, Entity, Caller, Callee) :-
		!,
		modules_diagram_support::module_property(Entity, calls(Callee, Properties)),
		Callee \= _::_,
		Callee \= ':'(_, _),
		memberchk(caller(Caller), Properties).
	calls_local_predicate(Kind, Entity, Caller, Callee) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(Callee0, CallsProperties)),
		Callee0 \= _::_,
		Callee0 \= ':'(_, _),
		memberchk(caller(Caller0), CallsProperties),
		(	entity_property(Kind, Entity, defines(Callee0, CalleeDefinesProperties)),
			member(non_terminal(CalleeNonTerminal), CalleeDefinesProperties) ->
			Callee = CalleeNonTerminal
		;	Callee = Callee0
		),
		(	entity_property(Kind, Entity, defines(Caller0, CallerDefinesProperties)),
			member(non_terminal(CallerNonTerminal), CallerDefinesProperties) ->
			Caller = CallerNonTerminal
		;	Caller = Caller0
		).

	calls_external_predicate(module, Entity, Caller, Callee) :-
		!,
		modules_diagram_support::module_property(Entity, calls(Callee, Properties)),
		(	Callee = Object::_, nonvar(Object)
		;	Callee = ':'(Module,_), nonvar(Module)
		),
		memberchk(caller(Caller), Properties).
	calls_external_predicate(Kind, Entity, Caller, Object::Callee) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(Object::Callee, Properties)),
		nonvar(Object),
		memberchk(caller(Caller), Properties).
	calls_external_predicate(Kind, Entity, Caller, ':'(Module,Callee)) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(':'(Module,Callee), Properties)),
		nonvar(Module),
		memberchk(caller(Caller), Properties).

	entity_property(object, Entity, Property) :-
		object_property(Entity, Property).
	entity_property(category, Entity, Property) :-
		category_property(Entity, Property).
	entity_property(protocol, Entity, Property) :-
		protocol_property(Entity, Property).
	entity_property(module, Entity, Property) :-
		modules_diagram_support::module_property(Entity, Property).

	reset :-
		retractall(included_predicate_(_)),
		retractall(referenced_predicate_(_)),
		retractall(external_predicate_(_)).

	remember_referenced_predicate(Predicate) :-
		(	referenced_predicate_(Predicate) ->
			true
		;	assertz(referenced_predicate_(Predicate))
		).

	remember_external_predicate(Predicate) :-
		(	external_predicate_(Predicate) ->
			true
		;	assertz(external_predicate_(Predicate))
		).

	output_external_predicates(Options) :-
		^^format_object(Format),
		Format::graph_header(output_file, other, '(external predicates)', external, [tooltip('(external predicates)')| Options]),
		retract(external_predicate_(Object::Predicate)),
		^^ground_entity_identifier(object, Object, Name),
		^^output_node(Name::Predicate, Name::Predicate, external, [], external_predicate, Options),
		fail.
	output_external_predicates(Options) :-
		retract(external_predicate_(':'(Module,Predicate))),
		^^output_node(':'(Module,Predicate), ':'(Module,Predicate), external, [], external_predicate, Options),
		fail.
	output_external_predicates(Options) :-
		retract(referenced_predicate_(Predicate)),
		^^output_node(Predicate, Predicate, external, [], external_predicate, Options),
		fail.
	output_external_predicates(Options) :-
		^^format_object(Format),
		Format::graph_footer(output_file, other, '(external predicates)', external, [tooltip('(external predicates)')| Options]).

	% by default, diagram layout is top to bottom:
	default_option(layout(top_to_bottom)).
	% by default, diagram title is empty:
	default_option(title('')).
	% by default, print current date:
	default_option(date(true)).
	% by default, print entity public predicates:
	default_option(interface(true)).
	% by default, print file labels:
	default_option(file_labels(true)).
	% by default, don't write inheritance links:
	default_option(inheritance_relations(false)).
	% by default, don't write provide links:
	default_option(provide_relations(false)).
	% by default, don't write cross-referencing links:
	default_option(xref_relations(false)).
	% by default, print entity relation labels:
	default_option(relation_labels(true)).
	% by default, write cross-referencing calls:
	default_option(xref_calls(true)).
	% by default, don't print node type captions
	default_option(node_type_captions(false)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't exclude any library sub-directories:
	default_option(exclude_libraries([])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).
	% by default, don't generate cluster, file, and entity URLs:
	default_option(url_prefixes('', '')).
	% by default, don't omit any path prefixes when printing paths:
	default_option(omit_path_prefixes([])).
	% by default, use a '.html' suffix for entity documentation URLs:
	default_option(entity_url_suffix_target('.html', '#')).

	diagram_name_suffix('_xref_diagram').

:- end_object.



:- object(xref_diagram,
	extends(xref_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/01,
		comment is 'Predicates for generating predicate call cross-referencing diagrams in DOT format.'
	]).

:- end_object.
