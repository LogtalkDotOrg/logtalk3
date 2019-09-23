%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 2.59,
		author is 'Paulo Moura',
		date is 2019/09/23,
		comment is 'Predicates for generating predicate call cross-referencing diagrams.',
		parameters is ['Format' - 'Graph language file format'],
		see_also is [entity_diagram(_), inheritance_diagram(_), uses_diagram(_)]
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(os, [
		decompose_file_name/3
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
		self(Self),
		entity_kind(Entity, Kind, GroundEntity, Name),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, Kind, Entity)),
		atom_concat(Name, '_', Identifier0),
		atom_concat(Identifier0, Kind, Identifier),
		^^format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		^^output_file_path(Identifier, Options, Format, OutputPath),
		^^diagram_caption(Kind, Entity, Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Identifier, [description(Description)| Options]),
			entity_property(Kind, Entity, file(Basename, Directory)),
			atom_concat(Directory, Basename, Path),
			^^add_link_options(Path, Options, GraphOptions),
			Format::graph_header(diagram_output_file, Identifier, GroundEntity, entity, GraphOptions),
			process_entity(Kind, Entity, GraphOptions),
			output_externals(Options),
			^^output_edges(Options),
			Format::graph_footer(diagram_output_file, Identifier, GroundEntity, entity, GraphOptions),
			Format::file_footer(diagram_output_file, Identifier, Options) ->
			logtalk::print_message(comment, diagrams, generated_diagram(Self, Kind, Entity))
		;	% failure is usually caused by errors in the source itself
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

	process_entity(Kind, Entity, Options) :-
		entity_property(Kind, Entity, declares(Predicate0, Properties)),
		(	member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		),
		predicate_kind_caption(Kind, Properties, PredicateKind, Caption),
		add_predicate_code_url(Options, Kind, Entity, Properties, PredicateOptions),
		^^output_node(Predicate, Predicate, Caption, [], PredicateKind, PredicateOptions),
		remember_included_predicate(Predicate0),
		fail.
	process_entity(Kind, Entity, Options) :-
		Kind \== protocol,
		entity_property(Kind, Entity, defines(Predicate0, Properties)),
		% exclude predicates that have no clauses defined at compilation time
		memberchk(number_of_clauses(NumberOfClauses), Properties),
		NumberOfClauses > 0,
		\+ included_predicate_(Predicate0),
		\+ member(auxiliary, Properties),
		(	member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		),
		add_predicate_code_url(Options, Kind, Entity, Properties, PredicateOptions),
		^^output_node(Predicate, Predicate, local, [], local_predicate, PredicateOptions),
		remember_included_predicate(Predicate0),
		fail.
	process_entity(Kind, Entity, Options) :-
		Kind \== protocol,
		entity_property(Kind, Entity, updates(Predicate0, Properties)),
		\+ included_predicate_(Predicate0),
		\+ member(auxiliary, Properties),
		(	member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		),
		add_predicate_code_url(Options, Kind, Entity, Properties, PredicateOptions),
		^^output_node(Predicate, Predicate, (dynamic), [], local_predicate, PredicateOptions),
		remember_included_predicate(Predicate0),
		fail.
	process_entity(Kind, Entity, Options) :-
		Kind \== protocol,
		entity_property(Kind, Entity, provides(Predicate, To, ProvidesProperties)),
		\+ member(auxiliary, ProvidesProperties),
		(	Kind == module ->
			add_predicate_code_url(Options, Kind, Entity, ProvidesProperties, PredicateOptions),
			^^output_node(':'(To,Predicate), ':'(To,Predicate), (multifile), [], multifile_predicate, PredicateOptions),
			remember_included_predicate(':'(To,Predicate))
		;	(	current_object(To) ->
				ToKind = object
			;	ToKind = category
			),
			entity_property(ToKind, To, declares(Predicate, DeclaresProperties)),
			member(non_terminal(NonTerminal), DeclaresProperties) ->
			add_predicate_code_url(Options, Kind, Entity, ProvidesProperties, PredicateOptions),
			^^output_node(To::NonTerminal, To::NonTerminal, (multifile), [], multifile_predicate, PredicateOptions),
			remember_included_predicate(To::Predicate)
		;	add_predicate_code_url(Options, Kind, Entity, ProvidesProperties, PredicateOptions),
			^^output_node(To::Predicate, To::Predicate, (multifile), [], multifile_predicate, PredicateOptions),
			remember_included_predicate(To::Predicate)
		),
		fail.
	process_entity(Kind, Entity, Options) :-
		calls_local_predicate(Kind, Entity, Caller, Line, Callee),
		\+ ^^edge(Caller, Callee, [calls], calls_predicate, _),
		remember_referenced_predicate(Caller),
		remember_referenced_predicate(Callee),
		add_xref_code_url(Options, Kind, Entity, Line, XRefOptions),
		^^save_edge(Caller, Callee, [calls], calls_predicate, [tooltip(calls)| XRefOptions]),
		fail.
	process_entity(Kind, Entity, Options) :-
		calls_self_predicate(Kind, Entity, Caller, Line, Callee),
		\+ ^^edge(Caller, ::Callee, [calls], calls_self_predicate, _),
		remember_referenced_predicate(Caller),
		remember_external_predicate(::Callee),
		add_xref_code_url(Options, Kind, Entity, Line, XRefOptions),
		^^save_edge(Caller, ::Callee, [calls], calls_self_predicate, [tooltip(calls)| XRefOptions]),
		fail.
	process_entity(Kind, Entity, Options) :-
		calls_super_predicate(Kind, Entity, Caller, Line, Callee),
		\+ ^^edge(Caller, ^^Callee, [calls], calls_super_predicate, _),
		remember_referenced_predicate(Caller),
		remember_external_predicate(^^Callee),
		add_xref_code_url(Options, Kind, Entity, Line, XRefOptions),
		^^save_edge(Caller, ^^Callee, [calls], calls_super_predicate, [tooltip(calls)| XRefOptions]),
		fail.
	process_entity(Kind, Entity, Options) :-
		calls_external_predicate(Kind, Entity, Caller, Line, Callee0),
		remember_referenced_predicate(Caller),
		remember_external_predicate(Callee0),
		\+ ^^edge(Caller, Callee0, [calls], calls_predicate, _),
		add_xref_code_url(Options, Kind, Entity, Line, XRefOptions),
		(	Callee0 = Other::Predicate ->
			^^ground_entity_identifier(object, Other, Name),
			Callee = Name::Predicate
		;	Callee = Callee0
		),
		^^save_edge(Caller, Callee, [calls], calls_predicate, [tooltip(calls)| XRefOptions]),
		fail.
	process_entity(Kind, Entity, Options) :-
		updates_predicate(Kind, Entity, Caller, Line, Dynamic),
		(	Dynamic = ::_ ->
			EdgeKind = updates_self_predicate
		;	Kind == category ->
			EdgeKind = updates_this_predicate
		;	EdgeKind = updates_predicate
		),
		\+ ^^edge(Caller, Dynamic, [updates], EdgeKind, _),
		remember_referenced_predicate(Caller),
		remember_referenced_predicate(Dynamic),
		add_xref_code_url(Options, Kind, Entity, Line, XRefOptions),
		^^save_edge(Caller, Dynamic, [updates], EdgeKind, [tooltip(updates)| XRefOptions]),
		fail.
	process_entity(_, _, _) :-
		retract(included_predicate_(Predicate)),
		retractall(referenced_predicate_(Predicate)),
		fail.
	process_entity(Kind, Entity, Options) :-
		retract(referenced_predicate_((:-)/1)),
		% initialization/1 directive; only create a node for the first directive occurrence
		once((
			entity_property(Kind, Entity, calls(_, Properties)),
			member(caller((:-)/1), Properties)
		)),
		(	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		),
		add_xref_code_url(Options, Kind, Entity, Line, XRefOptions),
		^^output_node((:-)/1, (:-)/1, directive, [], directive, XRefOptions),
		fail.
	process_entity(_, _, _) :-
		retract(referenced_predicate_(Functor/Arity)),
		^^output_node(Functor/Arity, Functor/Arity, '', [], predicate, []),
		fail.
	process_entity(_, _, _).

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

	add_predicate_documentation_url(Options, Object, Predicate, XRefOptions) :-
		(	Predicate = Functor/Arity ->
			functor(Template, Functor, Arity)
		;	Predicate = Functor//Arity,
			Arity2 is Arity + 2,
			functor(Template, Functor, Arity2)
		),
		(	% first, find the entity declaring the called predicate; but fail if
			% the entity is not loaded
			catch(Object::predicate_property(Template, declared_in(Entity)), _, fail),
			% second, find the file defining the entity declaring the predicate
			(	current_object(Entity) ->
				object_property(Entity, file(Path))
			;	current_category(Entity) ->
				object_property(Entity, file(Path))
			;	current_protocol(Entity) ->
				protocol_property(Entity, file(Path))
			;	% entity is not loaded
				fail
			),
			% third, find the documentation URL prefix, looking for a
			% path specific prefix before considering the generic prefix
			(	member(path_url_prefixes(Prefix, _, DocPrefix), Options),
				atom_concat(Prefix, _, Path) ->
				true
			;	member(url_prefixes(_, DocPrefix), Options)
			),
			DocPrefix \== '' ->
			functor(Entity, EntityFunctor, EntityArity),
			atom_concat(DocPrefix, EntityFunctor, DocURL0),
			atom_concat(DocURL0, '_', DocURL1),
			number_codes(EntityArity, EntityArityCodes),
			atom_codes(EntityArityAtom, EntityArityCodes),
			atom_concat(DocURL1, EntityArityAtom, DocURL2),
			memberchk(entity_url_suffix_target(Suffix, Target), Options),
			atom_concat(DocURL2, Suffix, DocURL3),
			(	Target == '' ->
				DocURL = DocURL3
			;	atom_concat(DocURL3, Target, DocURL4),
				predicate_target_value(Predicate, TargetValue),
				atom_concat(DocURL4, TargetValue, DocURL)
			),
			XRefOptions = [url(DocURL)| Options]
		;	% could not find entity file or URL prefixes not defined
			XRefOptions = [url('')| Options]
		).

	predicate_target_value(Predicate, TargetValue) :-
		(	Predicate = Functor/Arity ->
			atom_concat(Functor, '/', TargetValue0)
		;	Predicate = Functor//Arity,
			atom_concat(Functor, '//', TargetValue0)
		),
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(TargetValue0, ArityAtom, TargetValue).

	add_predicate_code_url(Options, Kind, Entity, Properties, PredicateOptions) :-
		(	member(line_count(Line), Properties),
			Line =\= -1 ->
			add_xref_code_url(Options, Kind, Entity, Line, PredicateOptions)
		;	PredicateOptions = [url('')| Options]
		).

	add_xref_code_url(Options, Kind, Entity, Line, XRefOptions) :-
		(	% first, find the file defining the entity
			(	Kind == object ->
				object_property(Entity, file(Path))
			;	Kind == category ->
				category_property(Entity, file(Path))
			;	Kind == protocol ->
				protocol_property(Entity, file(Path))
			;	{atom(Entity), current_module(Entity)} ->
				modules_diagram_support::module_property(Entity, file(Path))
			;	% entity is not loaded
				fail
			),
			% second, find the code URL prefix, looking for a path
			% specific prefix before considering the generic prefix
			(	member(path_url_prefixes(Prefix, CodePrefix, _), Options),
				atom_concat(Prefix, _, Path) ->
				true
			;	memberchk(url_prefixes(CodePrefix, _), Options)
			),
			% third, cut down any specified local path prefix
			% before constructing the final code URL
			memberchk(omit_path_prefixes(PathPrefixes), Options),
			(	member(PathPrefix, PathPrefixes),
				atom_concat(PathPrefix, RelativePath, Path) ->
				atom_concat(CodePrefix, RelativePath, CodeURL0)
			;	% prefix to be cut not specified; use the file
				% absolute path as a local URL
				CodeURL0 = Path
			),
			(	Line = -1 ->
				% no line number available; simply link to the entity file
				CodeURL = CodeURL0
			;	sub_atom(CodeURL0, 0, 1, _, '/') ->
				% absolute path; link to local file and don't append line number
				CodeURL = CodeURL0
			;	\+ sub_atom(CodeURL0, 0, 7, _, 'http://'),
				\+ sub_atom(CodeURL0, 0, 8, _, 'https://') ->
				% assume local file and don't append line number
				CodeURL = CodeURL0
			;	% append line number; check first for BitBucket code hosting
				% style URL line reference
				member(url_line_references(bitbucket), Options) ->
				decompose_file_name(RelativePath, _, File),
				atom_concat(CodeURL0, '?fileviewer=file-view-default#', CodeURL1),
				atom_concat(CodeURL1, File, CodeURL2),
				atom_concat(CodeURL2, '-', CodeURL3),
				number_codes(Line, LineCodes),
				atom_codes(LineAtom, LineCodes),
				atom_concat(CodeURL3, LineAtom, CodeURL)
			;	% assume github or gitlab line reference syntax
				atom_concat(CodeURL0, '#L', CodeURL1),
				number_codes(Line, LineCodes),
				atom_codes(LineAtom, LineCodes),
				atom_concat(CodeURL1, LineAtom, CodeURL)
			) ->
			XRefOptions = [url(CodeURL)| Options]
		;	% could not find entity file or URL prefixes not defined
			XRefOptions = [url('')| Options]
		).

	calls_local_predicate(module, Entity, Caller, Line, Callee) :-
		!,
		modules_diagram_support::module_property(Entity, calls(Callee, Properties)),
		Callee \= _::_,
		Callee \= ':'(_, _),
		memberchk(caller(Caller), Properties),
		(	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		).
	calls_local_predicate(Kind, Entity, Caller, Line, Callee) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(Callee0, CallsProperties)),
		Callee0 \= _::_,
		Callee0 \= ::_,
		Callee0 \= ^^_,
		Callee0 \= _<<_,
		Callee0 \= ':'(_, _),
		memberchk(caller(Caller0), CallsProperties),
		(	member(line_count(Line), CallsProperties) ->
			true
		;	Line = -1
		),
		(	entity_property(Kind, Entity, declares(Callee0, CalleeDeclaresProperties)),
			member(non_terminal(CalleeNonTerminal), CalleeDeclaresProperties) ->
			Callee = CalleeNonTerminal
		;	entity_property(Kind, Entity, defines(Callee0, CalleeDefinesProperties)),
			\+ member(auxiliary, CalleeDefinesProperties) ->
			(	member(non_terminal(CalleeNonTerminal), CalleeDefinesProperties) ->
				Callee = CalleeNonTerminal
			;	Callee = Callee0
			)
		;	fail
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

	calls_super_predicate(Kind, Entity, Caller, Line, Callee) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(^^Callee0, CallsProperties)),
		ground(Callee0),
		memberchk(caller(Caller0), CallsProperties),
		(	member(line_count(Line), CallsProperties) ->
			true
		;	Line = -1
		),
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

	calls_self_predicate(Kind, Entity, Caller, Line, Callee) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(::Callee, CallsProperties)),
		ground(Callee),
		memberchk(caller(Caller0), CallsProperties),
		(	member(line_count(Line), CallsProperties) ->
			true
		;	Line = -1
		),
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

	calls_external_predicate(module, Entity, Caller, Line, Callee) :-
		!,
		modules_diagram_support::module_property(Entity, calls(Callee, Properties)),
		(	Callee = Object::_, nonvar(Object)
		;	Callee = ':'(Module,_), nonvar(Module)
		),
		memberchk(caller(Caller), Properties),
		(	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		).
	calls_external_predicate(Kind, Entity, Caller, Line, Object::Callee) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(Object::Callee0, CallsProperties)),
		nonvar(Object),
		memberchk(caller(Caller0), CallsProperties),
		(	member(line_count(Line), CallsProperties) ->
			true
		;	Line = -1
		),
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
	calls_external_predicate(Kind, Entity, Caller, Line, ':'(Module,Callee)) :-
		Kind \== protocol,
		entity_property(Kind, Entity, calls(':'(Module,Callee), Properties)),
		nonvar(Module),
		memberchk(caller(Caller), Properties),
		(	member(line_count(Line), Properties) ->
			true
		;	Line = -1
		),
		entity_property(Kind, Entity, defines(Caller, CallerProperties)),
		\+ member(auxiliary, CallerProperties).

	updates_predicate(Kind, Entity, Updater, Line, Dynamic) :-
		Kind \== protocol,
		entity_property(Kind, Entity, updates(Dynamic, UpdatesProperties)),
		(	Dynamic = Object::PredicateIndicator ->
			% we can have a parametric object ...
			callable(Object), ground(PredicateIndicator)
		;	ground(Dynamic)
		),
		memberchk(updater(Updater0), UpdatesProperties),
		(	member(line_count(Line), UpdatesProperties) ->
			true
		;	Line = -1
		),
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

	remember_included_predicate(Reference) :-
		(	included_predicate_(Reference) ->
			true
		;	Reference = Entity::Predicate ->
			functor(Entity, Functor, Arity),
			functor(Template, Functor, Arity),
			assertz(included_predicate_(Template::Predicate))
		;	assertz(included_predicate_(Reference))
		).

	remember_referenced_predicate(Reference) :-
		(	referenced_predicate_(Reference) ->
			true
		;	Reference = Entity::Predicate ->
			functor(Entity, Functor, Arity),
			functor(Template, Functor, Arity),
			assertz(referenced_predicate_(Template::Predicate))
		;	assertz(referenced_predicate_(Reference))
		).

	remember_external_predicate(Reference) :-
		(	external_predicate_(Reference) ->
			true
		;	Reference = Entity::Predicate ->
			functor(Entity, Functor, Arity),
			functor(Template, Functor, Arity),
			assertz(external_predicate_(Template::Predicate))
		;	assertz(external_predicate_(Reference))
		).

	output_externals(Options) :-
		member(externals(false), Options),
		!.
	output_externals(Options) :-
		retract(external_predicate_(Object::Predicate)),
		^^ground_entity_identifier(object, Object, Name),
		add_predicate_documentation_url(Options, Object, Predicate, PredicateOptions),
		^^output_node(Name::Predicate, Name::Predicate, external, [], external_predicate, PredicateOptions),
		fail.
	output_externals(Options) :-
		retract(external_predicate_(':'(Module,Predicate))),
		(	modules_diagram_support::module_property(Module, defines(Predicate,Properties)) ->
			add_predicate_code_url(Options, module, Module, Properties, PredicateOptions),
			^^output_node(':'(Module,Predicate), ':'(Module,Predicate), external, [], external_predicate, PredicateOptions)
		;	memberchk(node_type_captions(Boolean), Options),
			^^output_node(':'(Module,Predicate), ':'(Module,Predicate), external, [], external_predicate, [node_type_captions(Boolean)])
		),
		fail.
	output_externals(Options) :-
		retract(external_predicate_(Predicate)),
		memberchk(node_type_captions(Boolean), Options),
		^^output_node(Predicate, Predicate, external, [], external_predicate, [node_type_captions(Boolean)]),
		fail.
	output_externals(_).

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
	% by default, print external nodes:
	default_option(externals(true)).
	% by default, print node type captions:
	default_option(node_type_captions(true)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any directories:
	default_option(exclude_directories([])).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, exclude only the "startup" and "scratch_directory" libraries:
	default_option(exclude_libraries([startup, scratch_directory])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).
	% by default, don't omit any prefix when printing paths:
	default_option(omit_path_prefixes([])).
	% by default, use a '.html' suffix for entity documentation URLs:
	default_option(entity_url_suffix_target('.html', '#')).
	% by default, don't link to sub-diagrams:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for linked diagrams
	default_option(zoom_url_suffix('.svg')).
	% by default, assume GitHub/GitLab line references in URLs
	default_option(url_line_references(github)).

	diagram_description('Cross-referencing diagram').

	diagram_name_suffix('_xref_diagram').

	message_diagram_description('cross-referencing').

:- end_object.



:- object(xref_diagram,
	extends(xref_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/01,
		comment is 'Predicates for generating predicate call cross-referencing diagrams in DOT format.',
		see_also is [entity_diagram, inheritance_diagram, uses_diagram]
	]).

:- end_object.
