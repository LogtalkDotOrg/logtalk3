%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(xref_diagram(Format),
	extends(entity_diagram(Format))).

	:- info([
		version is 2.14,
		author is 'Paulo Moura',
		date is 2017/03/02,
		comment is 'Predicates for generating predicate call cross-referencing diagrams.',
		parnames is ['Format']
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- public(entity/2).
	:- mode(entity(+entity_identifier, +list(compound)), one).
	:- info(entity/2, [
		comment is 'Creates a diagram for a single entity using the specified options.',
		argnames is ['Entity', 'Options']
	]).

	:- public(entity/1).
	:- mode(entity(+entity_identifier), one).
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
		::reset,
		^^output_file_path(Identifier, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Identifier, Options),
			entity_property(Kind, Entity, file(Basename, Directory)),
			atom_concat(Directory, Basename, Path),
			^^add_link_options(Path, Options, GraphOptions),
			Format::graph_header(diagram_output_file, Identifier, GroundEntity, entity, GraphOptions),
			process(Kind, Entity, GraphOptions),
			output_external_predicates(Options),
			^^output_edges(Options),
			Format::graph_footer(diagram_output_file, Identifier, GroundEntity, entity, GraphOptions),
			Format::file_footer(diagram_output_file, Identifier, Options) ->
			true
		;	% failure is usually caused by errors in the source itself
			self(Self),
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::entity(Entity, UserOptions)))
		),
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
		% exclude dynamic predicates that have no clauses defined at compilation time
		Properties \== [],
		\+ included_predicate_(Predicate0),
		\+ member(auxiliary, Properties),
		(	member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		),
		memberchk(node_type_captions(Boolean), Options),
		^^output_node(Predicate, Predicate, local, [], local_predicate, [node_type_captions(Boolean)]),
		assertz(included_predicate_(Predicate)),
		fail.
	process(Kind, Entity, Options) :-
		Kind \== protocol,
		entity_property(Kind, Entity, updates(Predicate0, Properties)),
		\+ included_predicate_(Predicate0),
		\+ member(auxiliary, Properties),
		(	member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		),
		memberchk(node_type_captions(Boolean), Options),
		^^output_node(Predicate, Predicate, (dynamic), [], local_predicate, [node_type_captions(Boolean)]),
		assertz(included_predicate_(Predicate)),
		fail.
	process(Kind, Entity, Options) :-
		Kind \== protocol,
		entity_property(Kind, Entity, provides(Predicate, To, ProvidesProperties)),
		\+ member(auxiliary, ProvidesProperties),
		(	Kind == module ->
			memberchk(node_type_captions(Boolean), Options),
			^^output_node(':'(To,Predicate), ':'(To,Predicate), (multifile), [], multifile_predicate, [node_type_captions(Boolean)]),
			assertz(included_predicate_(':'(To,Predicate)))
		;	(	current_object(To) ->
				ToKind = object
			;	ToKind = category
			),
			entity_property(ToKind, To, declares(Predicate, DeclaresProperties)),
			member(non_terminal(NonTerminal), DeclaresProperties) ->
			add_predicate_documentation_url(Options, Entity, To::NonTerminal, PredicateOptions),
			^^output_node(To::NonTerminal, To::NonTerminal, (multifile), [], multifile_predicate, PredicateOptions),
			assertz(included_predicate_(To::NonTerminal))
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
		calls_self_predicate(Kind, Entity, Caller, Callee),
		\+ ^^edge(Caller, Callee, ['calls in self'], calls_self_predicate, _),
		remember_referenced_predicate(Caller),
		remember_referenced_predicate(Callee),
		^^save_edge(Caller, Callee, ['calls in self'], calls_self_predicate, [tooltip('calls in self')| Options]),
		fail.
	process(Kind, Entity, Options) :-
		calls_super_predicate(Kind, Entity, Caller, Callee),
		\+ ^^edge(Caller, Callee, ['super call'], calls_super_predicate, _),
		remember_referenced_predicate(Caller),
		remember_referenced_predicate(Callee),
		^^save_edge(Caller, Callee, ['super call'], calls_super_predicate, [tooltip('super call')| Options]),
		fail.
	process(Kind, Entity, Options) :-
		calls_external_predicate(Kind, Entity, Caller, Callee),
		remember_external_predicate(Callee),
		\+ ^^edge(Caller, Callee, [calls], calls_predicate, _),
		^^save_edge(Caller, Callee, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	process(Kind, Entity, Options) :-
		updates_predicate(Kind, Entity, Caller, Dynamic),
		(	Dynamic = ::_ ->
			Tooltip = 'updates in self',
			EdgeKind = updates_self_predicate
		;	Kind == category ->
			Tooltip = 'updates in this',
			EdgeKind = updates_this_predicate
		;	Tooltip = 'updates',
			EdgeKind = updates_predicate
		),
		\+ ^^edge(Caller, Dynamic, [Tooltip], EdgeKind, _),
		remember_referenced_predicate(Caller),
		remember_referenced_predicate(Dynamic),
		^^save_edge(Caller, Dynamic, [Tooltip], EdgeKind, [tooltip(Tooltip)| Options]),
		fail.
	process(_, _, _) :-
		retract(included_predicate_(Predicate)),
		retractall(referenced_predicate_(Predicate)),
		fail.
	process(_, _, _) :-
		retract(referenced_predicate_(Functor/Arity)),
		^^output_node(Functor/Arity, Functor/Arity, '', [], predicate, []),
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

	% multifile predicate
	add_predicate_documentation_url(Options, _, Entity::Functor/Arity, PredicateOptions) :-
		!,
		add_predicate_documentation_url(Options, Entity, Functor/Arity, PredicateOptions).
	% multifile non-terminal
	add_predicate_documentation_url(Options, _, Entity::Functor//Arity, PredicateOptions) :-
		!,
		add_predicate_documentation_url(Options, Entity, Functor//Arity, PredicateOptions).
	% local predicate or non-terminal
	add_predicate_documentation_url(Options, Entity, Predicate, PredicateOptions) :-
		(	(	(	current_object(Entity) ->
					object_property(Entity, file(Path))
				;	current_category(Entity) ->
					object_property(Entity, file(Path))
				;	current_protocol(Entity) ->
					protocol_property(Entity, file(Path))
				;	% entity is not loaded
					fail
				),
				member(path_url_prefixes(Prefix, CodePrefix, DocPrefix), Options),
				atom_concat(Prefix, _, Path) ->
				true
			;	member(url_prefixes(CodePrefix, DocPrefix), Options)
			) ->
			functor(Entity, EntityFunctor, EntityArity),
			atom_concat(DocPrefix, EntityFunctor, DocURL0),
			atom_concat(DocURL0, '_', DocURL1),
			number_codes(EntityArity, EntityArityCodes),
			atom_codes(EntityArityAtom, EntityArityCodes),
			atom_concat(DocURL1, EntityArityAtom, DocURL2),
			memberchk(entity_url_suffix_target(Suffix, Target), Options),
			(	Target == '' ->
				atom_concat(DocURL2, Suffix, DocURL)
			;	atom_concat(DocURL2, Suffix, DocURL3),
				atom_concat(DocURL3, Target, DocURL4),
				(	Predicate = Functor/Arity ->
					atom_concat(DocURL4, Functor, DocURL5),
					atom_concat(DocURL5, '/', DocURL6)
				;	Predicate = Functor//Arity,
					atom_concat(DocURL4, Functor, DocURL5),
					atom_concat(DocURL5, '//', DocURL6)
				),
				number_codes(Arity, ArityCodes),
				atom_codes(ArityAtom, ArityCodes),
				atom_concat(DocURL6, ArityAtom, DocURL)
			),
			PredicateOptions = [urls(CodePrefix, DocURL)| Options]
		;	PredicateOptions = Options
		).

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
		Callee0 \= ::_,
		Callee0 \= ^^_,
		Callee0 \= _<<_,
		Callee0 \= ':'(_, _),
		memberchk(caller(Caller0), CallsProperties),
		(	entity_property(Kind, Entity, defines(Callee0, CalleeDefinesProperties)),
			member(non_terminal(CalleeNonTerminal), CalleeDefinesProperties) ->
			Callee = CalleeNonTerminal
		;	Callee = Callee0
		),
		(	Caller0 = From::Predicate ->
			(	current_object(From) ->
				FromKind = object
			;	FromKind = category
			),
			entity_property(FromKind, From, declares(Predicate, DeclaresProperties)),
			(	member(non_terminal(NonTerminal), DeclaresProperties) ->
				Caller = From::NonTerminal
			;	Caller = From::Predicate
			)
		;	entity_property(Kind, Entity, defines(Caller0, CallerDefinesProperties)),
			member(non_terminal(CallerNonTerminal), CallerDefinesProperties) ->
			Caller = CallerNonTerminal
		;	Caller = Caller0
		).

	calls_super_predicate(Kind, Entity, Caller, Callee) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(^^Callee0, CallsProperties)),
		ground(Callee0),
		memberchk(caller(Caller0), CallsProperties),
		(	Caller0 = From::Predicate ->
			% multifile predicate caller (unlikely but possible)
			Predicate = Functor/Arity,
			functor(Template, Functor, Arity),
			(	current_object(From) ->
				From::predicate_property(Template, CallerProperties)
			;	current_category(From) ->
				create_object(Object, [imports(From)], [], []),
				Object::predicate_property(Template, CallerProperties),
				abolish_object(Object)
			;	% entity not loaded
				logtalk::print_message(warning, diagrams, entity_not_loaded(From)),
				fail
			),
			(	member(non_terminal(NonTerminal), CallerProperties) ->
				Caller = From::NonTerminal
			;	Caller = From::Predicate
			)
		;	% local predicate caller
			entity_property(Kind, Entity, defines(Caller0, CallerProperties)),
			member(non_terminal(CallerNonTerminal), CallerProperties) ->
			Caller = CallerNonTerminal
		;	Caller = Caller0
		),
		% usually caller and callee are the same predicate but that's not required
		(	entity_property(Kind, Entity, defines(Callee0, CalleeProperties)),
			member(non_terminal(CalleeNonTerminal), CalleeProperties) ->
			Callee = CalleeNonTerminal
		;	Callee = Callee0
		).

	calls_self_predicate(Kind, Entity, Caller, Callee) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(::Callee, CallsProperties)),
		ground(Callee),
		memberchk(caller(Caller0), CallsProperties),
		(	Caller0 = From::Predicate ->
			% multifile predicate caller
			Predicate = Functor/Arity,
			functor(Template, Functor, Arity),
			(	current_object(From) ->
				From::predicate_property(Template, CallerProperties)
			;	current_category(From) ->
				create_object(Object, [imports(From)], [], []),
				Object::predicate_property(Template, CallerProperties),
				abolish_object(Object)
			;	% entity not loaded
				logtalk::print_message(warning, diagrams, entity_not_loaded(From)),
				fail
			),
			(	member(non_terminal(NonTerminal), CallerProperties) ->
				Caller = From::NonTerminal
			;	Caller = From::Predicate
			)
		;	% local predicate caller
			entity_property(Kind, Entity, defines(Caller0, CallerDefinesProperties)),
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
		entity_property(Kind, Entity, calls(Object::Callee0, CallsProperties)),
		nonvar(Object),
		memberchk(caller(Caller0), CallsProperties),
		entity_property(Kind, Entity, defines(Caller0, CallerProperties)),
		\+ member(auxiliary, CallerProperties),
		Callee0 = Functor/Arity,
		functor(Template, Functor, Arity),
		(	current_object(Object) ->
			Object::predicate_property(Template, CalleeProperties)
		;	% entity not loaded
			logtalk::print_message(warning, diagrams, entity_not_loaded(Object)),
			fail
		),
		(	member(non_terminal(CalleeNonTerminal), CalleeProperties) ->
			Callee = CalleeNonTerminal
		;	Callee = Callee0
		),
		(	member(non_terminal(CallerNonTerminal), CallerProperties) ->
			Caller = CallerNonTerminal
		;	Caller = Caller0
		).
	calls_external_predicate(Kind, Entity, Caller, ':'(Module,Callee)) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(':'(Module,Callee), Properties)),
		nonvar(Module),
		memberchk(caller(Caller), Properties),
		entity_property(Kind, Entity, defines(Caller, CallerProperties)),
		\+ member(auxiliary, CallerProperties).

	updates_predicate(Kind, Entity, Updater, Dynamic) :-
		Kind \== protocol,
		entity_property(Kind, Entity, updates(Dynamic, UpdatesProperties)),
		(	Dynamic = Object::PredicateIndicator ->
			% we can have a parametric object ...
			callable(Object), ground(PredicateIndicator)
		;	ground(Dynamic)
		),
		memberchk(updater(Updater0), UpdatesProperties),
		(	Updater0 = From::Predicate ->
			(	current_object(From) ->
				FromKind = object
			;	FromKind = category
			),
			entity_property(FromKind, From, declares(Predicate, DeclaresProperties)),
			(	member(non_terminal(NonTerminal), DeclaresProperties) ->
				Updater = From::NonTerminal
			;	Updater = From::Predicate
			)
		;	entity_property(Kind, Entity, defines(Updater0, DefinesProperties)),
			member(non_terminal(UpdaterNonTerminal), DefinesProperties) ->
			Updater = UpdaterNonTerminal
		;	Updater = Updater0
		).

	entity_property(object, Entity, Property) :-
		object_property(Entity, Property).
	entity_property(category, Entity, Property) :-
		category_property(Entity, Property).
	entity_property(protocol, Entity, Property) :-
		protocol_property(Entity, Property).
	entity_property(module, Entity, Property) :-
		modules_diagram_support::module_property(Entity, Property).

	reset :-
		^^reset,
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
		Format::graph_header(diagram_output_file, other, '(external predicates)', external, [urls('',''), tooltip('(external predicates)')| Options]),
		retract(external_predicate_(Object::Predicate)),
		^^ground_entity_identifier(object, Object, Name),
		add_predicate_documentation_url(Options, Object, Predicate, PredicateOptions),
		^^output_node(Name::Predicate, Name::Predicate, external, [], external_predicate, PredicateOptions),
		fail.
	output_external_predicates(Options) :-
		retract(external_predicate_(':'(Module,Predicate))),
		memberchk(node_type_captions(Boolean), Options),
		^^output_node(':'(Module,Predicate), ':'(Module,Predicate), external, [], external_predicate, [node_type_captions(Boolean)]),
		fail.
	output_external_predicates(Options) :-
		retract(referenced_predicate_(Predicate)),
		memberchk(node_type_captions(Boolean), Options),
		^^output_node(Predicate, Predicate, external, [], external_predicate, [node_type_captions(Boolean)]),
		fail.
	output_external_predicates(Options) :-
		^^format_object(Format),
		Format::graph_footer(diagram_output_file, other, '(external predicates)', external, [urls('',''), tooltip('(external predicates)')| Options]).

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
	% by default, print file name extensions:
	default_option(file_extensions(true)).
	% by default, print entity relation labels:
	default_option(relation_labels(true)).
	% by default, write cross-referencing calls:
	default_option(xref_calls(true)).
	% by default, print node type captions
	default_option(node_type_captions(true)).
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
	% by default, don't zooming into libraries and entities:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for zoom linked diagrams
	default_option(zoom_url_suffix('.svg')).

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
