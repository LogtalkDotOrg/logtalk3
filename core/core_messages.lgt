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


:- category(core_messages).

	:- info([
		version is 1.22,
		author is 'Paulo Moura',
		date is 2017/05/07,
		comment is 'Logtalk core (compiler and runtime) default message translations.'
	]).

	:- built_in.

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, core, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	% Quintus Prolog based prefixes (also used in SICStus Prolog):
	message_prefix_stream(banner,         '',       user_output).
	message_prefix_stream(help,           '',       user_output).
	message_prefix_stream(information,    '',       user_output).
	message_prefix_stream(information(_), '',       user_output).
	message_prefix_stream(comment,        '% ',     user_output).
	message_prefix_stream(comment(_),     '% ',     user_output).
	message_prefix_stream(warning,        '*     ', user_error).
	message_prefix_stream(warning(_),     '*     ', user_error).
	message_prefix_stream(error,          '!     ', user_error).
	message_prefix_stream(error(_),       '!     ', user_error).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, core) -->
		{ground_term_copy(Message, GroundMessage)},
		message_tokens(GroundMessage).

	% file compilation and loading messages

	message_tokens(loading_file(File, _Flags)) -->
		['[ loading ~w ...  ]'-[File], nl].

	message_tokens(loaded_file(File, _Flags)) -->
		['[ ~w loaded ]'-[File], nl].

	message_tokens(skipping_reloading_file(File, _Flags)) -->
		['[ ~w already loaded; skipping ]'-[File], nl].

	message_tokens(reloading_file(File, _Flags)) -->
		['[ reloading ~w ... ]'-[File], nl].

	message_tokens(reloaded_file(File, _Flags)) -->
		['[ ~w reloaded ]'-[File], nl].

	message_tokens(compiling_file(File, _Flags)) -->
		['[ compiling ~w'-[File]],
		(	{current_logtalk_flag(debug, on)} ->
			(	{current_logtalk_flag(hook, Hook)} ->
				{ground_term_copy(Hook, GroundHook)},
				[' in debug mode using the hook object ~q ... ]'-[GroundHook], nl]
			;	[' in debug mode ... ]'-[], nl]
			)
		;	(	{current_logtalk_flag(hook, Hook)} ->
				{ground_term_copy(Hook, GroundHook)},
				[' using the hook object ~q ... ]'-[GroundHook], nl]
			;	[' ... ]'-[], nl]
			)
		).

	message_tokens(compiled_file(File, _Flags)) -->
		['[ ~w compiled ]'-[File], nl].

	message_tokens(up_to_date_file(File, _Flags)) -->
		['[ compiling ~w ... up-to-date ]'-[File], nl].

	% entity compilation messages

	message_tokens(compiling_entity(Type, Entity)) -->
		(	{current_logtalk_flag(debug, on)} ->
			['- compiling ~w ~q in debug mode ... '-[Type, Entity], nl]
		;	{current_logtalk_flag(optimize, on)} ->
			['- compiling ~w ~q in optimal mode ... '-[Type, Entity], nl]
		;	['- compiling ~w ~q ... '-[Type, Entity], nl]
		).
	message_tokens(compiled_entity(_Type, _Entity)) -->
		['compiled'-[], nl].

	message_tokens(redefining_entity(Type, Entity)) -->
		['Redefining ~w ~q'-[Type, Entity], nl].

	message_tokens(redefining_entity_from_file(File, Lines, Type, Entity, OldFile)) -->
		['Redefining ~w ~q (loaded from file ~w)'-[Type, Entity, OldFile], nl],
		message_context(File, Lines).

	% make messages

	message_tokens(no_make_target_specified) -->
		['No make target specified'-[], nl].

	message_tokens(invalid_make_target(Target)) -->
		['Invalid make target: ~w'-[Target], nl].

	message_tokens(modified_files_reloaded) -->
		['Reloaded all Logtalk source files modified or that required'-[], nl,
		 'recompilation due to a change to the compilation mode'-[], nl
		].

	message_tokens(intermediate_files_deleted) -->
		['Deleted all intermediate files for the loaded Logtalk source files'-[], nl].

	message_tokens(scanning_for_missing_entities_predicates) -->
		['Scanning for missing entities and predicates ...'-[], nl].

	message_tokens(completed_scanning_for_missing_entities_predicates) -->
		['... completed scanning for missing entities and predicates'-[], nl].

	message_tokens(scanning_for_circular_dependencies) -->
		['Scanning for circular entity dependencies ...'-[], nl].

	message_tokens(completed_scanning_for_circular_dependencies) -->
		['... completed scanning for circular entity dependencies'-[], nl].

	message_tokens(missing_protocols(Protocols)) -->
		['Missing protocols:'-[], nl],
		(	{Protocols == []} ->
			['  (none)'-[], nl, nl]
		;	missing_entities(Protocols), [nl]
		).

	message_tokens(missing_categories(Categories)) -->
		['Missing categories:'-[], nl],
		(	{Categories == []} ->
			['  (none)'-[], nl, nl]
		;	missing_entities(Categories), [nl]
		).

	message_tokens(missing_objects(Objects)) -->
		['Missing objects:'-[], nl],
		(	{Objects == []} ->
			['  (none)'-[], nl, nl]
		;	missing_entities(Objects), [nl]
		).

	message_tokens(missing_modules(Modules)) -->
		['Missing modules:'-[], nl],
		(	{Modules == []} ->
			['  (none)'-[], nl, nl]
		;	missing_entities(Modules), [nl]
		).

	message_tokens(missing_predicates(Predicates)) -->
		['Missing predicates:'-[], nl],
		(	{Predicates == []} ->
			['  (none)'-[], nl, nl]
		;	missing_predicates(Predicates), [nl]
		).

	message_tokens(circular_references(CircularReferences)) -->
		['Circular references:'-[], nl],
		(	{CircularReferences == []} ->
			['  (none)'-[], nl, nl]
		;	circular_references(CircularReferences), [nl]
		).

	% startup messages

	message_tokens(possibly_incompatible_prolog_version(Current, Compatible)) -->
		['Possibly incompatible backend Prolog compiler version detected!'-[], nl,
		 'Running Prolog compiler version: ~w'-[Current], nl,
		 'Advised Prolog compiler version: ~w'-[Compatible], nl
		].

	message_tokens(banner) -->
		{current_logtalk_flag(version_data, logtalk(Major, Minor, Patch, Status))},
		(	{Status == stable} ->
			[nl, 'Logtalk ~d.~d.~d'-[Major, Minor, Patch], nl, 'Copyright (c) 1998-2017 Paulo Moura'-[], nl, nl]
		;	[nl, 'Logtalk ~d.~d.~d-~w'-[Major, Minor, Patch, Status], nl, 'Copyright (c) 1998-2017 Paulo Moura'-[], nl, nl]
		).

	message_tokens(default_flags) -->
		{current_logtalk_flag(unknown_entities, UnknownEntities),
		 current_logtalk_flag(unknown_predicates, UnknownPredicates),
		 current_logtalk_flag(undefined_predicates, UndefinedPredicates),
		 current_logtalk_flag(portability, Portability),
		 current_logtalk_flag(redefined_built_ins, Redefined),
		 current_logtalk_flag(missing_directives, Missing),
		 current_logtalk_flag(singleton_variables, Singletons),
		 current_logtalk_flag(underscore_variables, Underscore),
		 current_logtalk_flag(complements, Complements),
		 current_logtalk_flag(dynamic_declarations, DynamicDeclarations),
		 current_logtalk_flag(context_switching_calls, ContextCalls),
		 current_logtalk_flag(events, Events),
		 current_logtalk_flag(report, Report),
		 current_logtalk_flag(scratch_directory, ScratchDirectory),
		 current_logtalk_flag(code_prefix, Code),
		 (current_logtalk_flag(hook, Hook) -> true; Hook = n/d),
		 current_logtalk_flag(optimize, Optimize),
		 current_logtalk_flag(source_data, SourceData),
		 current_logtalk_flag(clean, Clean),
		 current_logtalk_flag(debug, Debug),
		 current_logtalk_flag(reload, Reload),
		 current_logtalk_flag(prolog_compiler, PrologCompiler),
		 current_logtalk_flag(prolog_loader, PrologLoader),
		 current_logtalk_flag(prolog_dialect, PrologDialect),
		 current_logtalk_flag(modules, Modules),
		 current_logtalk_flag(threads, Threads),
		 current_logtalk_flag(tabling, Tabling),
		 current_logtalk_flag(coinduction, Coinduction),
		 current_logtalk_flag(unicode, Unicode),
		 current_logtalk_flag(encoding_directive, Encodings),
		 ground_term_copy(Hook, GroundHook)},
		[
			'Default lint compilation flags: '-[], nl,
			'  unknown_predicates: ~w, undefined_predicates: ~w'-[UnknownPredicates, UndefinedPredicates], nl,
			'  portability: ~w, unknown_entities: ~w'-[Portability, UnknownEntities], nl,
			'  missing_directives: ~w, redefined_built_ins: ~w'-[Missing, Redefined], nl,
			'  singleton_variables: ~w, underscore_variables: ~w'-[Singletons, Underscore], nl,
			'Default optional features compiler flags:'-[], nl,
			'  complements: ~w, dynamic_declarations: ~w'-[Complements, DynamicDeclarations], nl,
			'  context_switching_calls: ~w, events: ~w'-[ContextCalls, Events], nl,
			'Other default compilation flags:'-[], nl,
			'  report: ~w, scratch_directory: ~w'-[Report, ScratchDirectory], nl,
			'  source_data: ~w, code_prefix: ~q, hook: ~q'-[SourceData, Code, GroundHook], nl,
			'  optimize: ~w, debug: ~w, clean: ~w, reload: ~w'-[Optimize, Debug, Clean, Reload], nl,
			'Backend Prolog compiler flags:'-[], nl,
			'  prolog_compiler: ~w'-[PrologCompiler], nl,
			'  prolog_loader:   ~w'-[PrologLoader], nl,
			'Read-only compilation flags (backend Prolog compiler features):'-[], nl,
			'  prolog_dialect: ~w, modules: ~w, threads: ~w'-[PrologDialect, Modules, Threads], nl,
			'  tabling: ~w, coinduction: ~w'-[Tabling, Coinduction], nl,
			'  unicode: ~w, encoding_directive: ~w'-[Unicode, Encodings], nl, nl
		].

	% help

	message_tokens(help) -->
		['For Logtalk help, use ?- {help(loader)}. or ?- logtalk_load(help(loader)).'-[], nl, nl].

	% settings files messages

	message_tokens(loaded_settings_file(Path)) -->
		['Loaded settings file found on directory ~w'-[Path], nl, nl].
	message_tokens(settings_file_disabled) -->
		['Loading of settings file disabled in the backend Prolog compiler adapter file.'-[], nl, nl].
	message_tokens(error_loading_settings_file(Path)) -->
		['Errors found while loading settings file from directory ~w'-[Path], nl, nl].
	message_tokens(no_settings_file_found(allow)) -->
		['No settings file found in the startup or Logtalk user directories.'-[], nl,
		 'Using default flag values set in the backend Prolog compiler adapter file.'-[], nl, nl
		].
	message_tokens(no_settings_file_found(restrict)) -->
		['No settings file found in the Logtalk user directory.'-[], nl,
		 'Using default flag values set in the backend Prolog compiler adapter file.'-[], nl, nl
		].

	% debugging messages

	message_tokens(logtalk_debugger_aborted) -->
		['Debugging session aborted by user. Debugger still on.'-[], nl].

	message_tokens(debug_handler_provider_already_exists(File, Lines, Type, Entity, Provider)) -->
		['A definition for the debug handler predicate already exists in: ~q'-[Provider], nl],
		message_context(File, Lines, Type, Entity).

	% compiler error and warning messages

	message_tokens(loading_failure(File)) -->
		['Unexpected failure while loading the code generated for the file:'-[], nl,
		 '  ~w'-[File], nl,
		 'Likely bug in the backend Prolog compiler. Please file a bug report.'-[], nl
		].

	message_tokens(loading_error(File, Error)) -->
		['Unexpected error while loading the code generated for the file:'-[], nl,
		 '  ~w'-[File], nl,
		 '  '-[]
		],
		error_term_tokens(Error),
		['Likely bug in the backend Prolog compiler. Please file a bug report.'-[], nl
		].

	message_tokens(compiler_error(File, Lines, Error)) -->
		error_term_tokens(Error),
		message_context(File, Lines).

	message_tokens(compiler_stream_error(Error)) -->
		error_term_tokens(Error).

	message_tokens(term_expansion_error(File, Lines, Type, Entity, HookEntity, Term, Error)) -->
		['Error found when term-expanding ~q using hook entity ~q: '-[Term, HookEntity]],
		error_term_tokens(Error),
		message_context(File, Lines, Type, Entity).

	message_tokens(term_expansion_error(File, Lines, HookEntity, Term, Error)) -->
		['Error found when term-expanding ~q using hook entity ~q: '-[Term, HookEntity]],
		error_term_tokens(Error),
		message_context(File, Lines).

	message_tokens(goal_expansion_error(File, Lines, Type, Entity, HookEntity, Goal, Error)) -->
		['Error found when goal-expanding ~q using hook entity ~q: '-[Goal, HookEntity]],
		error_term_tokens(Error),
		message_context(File, Lines, Type, Entity).

	message_tokens(goal_expansion_error(File, Lines, HookEntity, Goal, Error)) -->
		['Error found when goal-expanding ~q using hook entity ~q: '-[Goal, HookEntity]],
		error_term_tokens(Error),
		message_context(File, Lines).

	message_tokens(redefined_logtalk_built_in_predicate(File, Lines, Type, Entity, Predicate)) -->
		['Redefining a Logtalk built-in predicate: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(redefined_prolog_built_in_predicate(File, Lines, Type, Entity, Predicate)) -->
		['Redefining a Prolog built-in predicate: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(reference_to_unknown_object(File, Lines, Type, Entity, Object)) -->
		['Reference to unknown object: ~q'-[Object], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(reference_to_unknown_protocol(File, Lines, Type, Entity, Protocol)) -->
		['Reference to unknown protocol: ~q'-[Protocol], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(reference_to_unknown_category(File, Lines, Type, Entity, Category)) -->
		['Reference to unknown category: ~q'-[Category], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(reference_to_unknown_module(File, Lines, Type, Entity, Module)) -->
		['Reference to unknown module: ~q'-[Module], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(missing_predicate_directive(File, Lines, Type, Entity, Directive, Predicate)) -->
		['Missing ~w directive for the predicate: ~q'-[Directive, Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(missing_scope_directive(File, Lines, Type, Entity, Predicate)) -->
		['Missing scope directive for the predicate: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_standard_predicate_call(File, Lines, Type, Entity, Predicate)) -->
		['Call to non-standard Prolog built-in predicate: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_standard_arithmetic_function_call(File, Lines, Type, Entity, Function)) -->
		['Call to non-standard Prolog built-in arithmetic function: ~q'-[Function], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_standard_prolog_flag(File, Lines, Type, Entity, Flag)) -->
		['Use of non-standard Prolog flag: ~q'-[Flag], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_standard_prolog_flag(File, Lines, Flag)) -->
		['Use of non-standard Prolog flag: ~q'-[Flag], nl],
		message_context(File, Lines).

	message_tokens(non_standard_prolog_flag_value(File, Lines, Type, Entity, Flag, Value)) -->
		['Use of non-standard Prolog flag value: ~q:~q'-[Flag, Value], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_standard_prolog_flag_value(File, Lines, Flag, Value)) -->
		['Use of non-standard Prolog flag: ~q:~q'-[Flag, Value], nl],
		message_context(File, Lines).

	message_tokens(non_standard_file_directive(File, Lines, Directive)) -->
		['Use of non-standard file directive: ~q'-[Directive], nl],
		message_context(File, Lines).

	message_tokens(unclassified_variables_in_lambda_expression(File, Lines, Type, Entity, UnqualifiedVars, LambdaExpression)) -->
		(	{UnqualifiedVars = [UnqualifiedVar]} ->
			['Unclassified variable ~q in lambda expression: ~q'-[UnqualifiedVar, LambdaExpression], nl]
		;	['Unclassified variables ~q in lambda expression: ~q'-[UnqualifiedVars, LambdaExpression], nl]
		),
		message_context(File, Lines, Type, Entity).

	message_tokens(variables_with_dual_role_in_lambda_expression(File, Lines, Type, Entity, MixedUpVars, LambdaExpression)) -->
		(	{MixedUpVars = [MixedUpVar]} ->
			['Variable ~q have dual role in lambda expression: ~q'-[MixedUpVar, LambdaExpression], nl]
		;	['Variables ~q have dual role in lambda expression: ~q'-[MixedUpVars, LambdaExpression], nl]
		),
		message_context(File, Lines, Type, Entity).

	message_tokens(complementing_category_ignored(File, Lines, Category, Object)) -->
		['Complementing category will be ignored: ~q'-[Category], nl,
		 'Complemented object, ~q, compiled with complementing categories support turned off'-[Object], nl],
		message_context(File, Lines).

	message_tokens(prolog_dialect_goal_expansion(File, Lines, Type, Entity, Goal, ExpandedGoal)) -->
		['Prolog dialect rewrite of goal ~q as ~q'-[Goal, ExpandedGoal], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(prolog_dialect_goal_expansion(File, Lines, Goal, ExpandedGoal)) -->
		['Prolog dialect rewrite of goal ~q as ~q'-[Goal, ExpandedGoal], nl],
		message_context(File, Lines).

	message_tokens(prolog_dialect_term_expansion(File, Lines, Type, Entity, Term, ExpandedTerms)) -->
		['Prolog dialect rewrite of term ~q as ~q'-[Term, ExpandedTerms], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(prolog_dialect_term_expansion(File, Lines, Term, ExpandedTerms)) -->
		['Prolog dialect rewrite of term ~q as ~q'-[Term, ExpandedTerms], nl],
		message_context(File, Lines).

	message_tokens(declared_static_predicate_called_but_not_defined(File, Lines, Type, Entity, Predicate)) -->
		['Declared static predicate called but not defined: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(declared_static_non_terminal_called_but_not_defined(File, Lines, Type, Entity, NonTerminal)) -->
		['Declared static non terminal called but not defined: ~q'-[NonTerminal], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(unknown_predicate_called_but_not_defined(File, Lines, Type, Entity, Predicate)) -->
		['Unknown predicate called but not defined: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(unknown_non_terminal_called_but_not_defined(File, Lines, Type, Entity, NonTerminal)) -->
		['Unknown non-terminal called but not defined: ~q'-[NonTerminal], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(missing_reference_to_built_in_protocol(File, Lines, Type, Entity, Protocol)) -->
		['Missing reference to the built-in protocol: ~q'-[Protocol], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(compiling_proprietary_prolog_directive(File, Lines, Type, Entity, Directive)) -->
		['Compiling proprietary Prolog directive: ~q'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(compiling_query_as_initialization_goal(File, Lines, Type, Entity, Directive)) -->
		['Compiling query as an initialization goal: ~q'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(singleton_variables(File, Lines, Type, Entity, Names, Term)) -->
		(	{Names = [Name]} ->
			['Singleton variable: ~w'-[Name], nl]
		;	['Singleton variables: ~w'-[Names], nl]
		),
		term_type_tokens(Term),
		message_context(File, Lines, Type, Entity).

	message_tokens(singleton_variables(File, Lines, Names, Term)) -->
		(	{Names = [Name]} ->
			['Singleton variable: ~w'-[Name], nl]
		;	['Singleton variables: ~w'-[Names], nl]
		),
		term_type_tokens(Term),
		message_context(File, Lines).

	message_tokens(compilation_and_loading_warnings(CCounter, LCounter)) -->
		(	{CCounter + LCounter =:= 0} ->
			% no warnings
			['(0 warnings)'-[], nl]
		;	{CCounter =:= 0} ->
			% no compilation warnings
			loading_warnings(LCounter), [nl]
		;	{LCounter =:= 0} ->
			% no loading warnings
			compilation_warnings(CCounter), [nl]
		;	% both compilation and loading warnings
			loading_warnings(LCounter), [' and '-[]], compilation_warnings(CCounter), [nl]
		).

	message_tokens(renamed_compiler_flag(File, Lines, Type, Entity, Flag, NewFlag)) -->
		['The compiler flag ~w have been renamed to ~w'-[Flag, NewFlag], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(renamed_compiler_flag(File, Lines, Flag, NewFlag)) -->
		['The compiler flag ~w have been renamed to ~w'-[Flag, NewFlag], nl],
		message_context(File, Lines).

	message_tokens(deprecated_control_construct(File, Lines, Type, Entity, Term)) -->
		['The ~w control construct is deprecated'-[Term], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_directive(File, Lines, Type, Entity, Directive)) -->
		['The ~w directive is deprecated'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(ignored_directive(File, Lines, Directive)) -->
		['The ~w directive is ignored'-[Directive], nl],
		message_context(File, Lines).

	message_tokens(misplaced_encoding_directive(File, Lines)) -->
		['The encoding/1 directive is misplaced'-[], nl],
		message_context(File, Lines).

	% auxiliary grammar rules

	loading_warnings(LCounter) -->
		(	{LCounter =:= 1} ->
			['~d loading warning'-[LCounter]]
		;	['~d loading warnings'-[LCounter]]
		).

	compilation_warnings(CCounter) -->
		(	{CCounter =:= 1} ->
			['~d compilation warning'-[CCounter]]
		;	['~d compilation warnings'-[CCounter]]
		).

	error_term_tokens(error(error(Error, Term), _)) -->
		error_term_tokens(error(Error, Term)).
	error_term_tokens(error(Error, Term)) -->
		error_tokens(Error),
		term_tokens(Term).
	error_term_tokens(Error) -->
		error_tokens(Error).

	% based on the ISO Prolog Core standard
	error_tokens(instantiation_error) -->
		['Instantiation error'-[], nl].
	error_tokens(type_error(ValidType, Culprit)) -->
		['Type error: expected ~q but got ~q'-[ValidType, Culprit], nl].
	error_tokens(domain_error(ValidDomain, Culprit)) -->
		['Domain error: value ~q is not in domain ~q'-[Culprit, ValidDomain], nl].
	error_tokens(permission_error(Operation, PermissionType, Culprit)) -->
		['Permission error: ~q ~q ~q'-[Operation, PermissionType, Culprit], nl].
	error_tokens(existence_error(ObjectType, Culprit)) -->
		['Existence error: ~q ~q does not exist'-[ObjectType, Culprit], nl].
	error_tokens(representation_error(Flag)) -->
		['Representation error: ~q'-[Flag], nl].
	error_tokens(evaluation_error(Error)) -->
		['Evaluation error: ~q'-[Error], nl].
	error_tokens(resource_error(Resource)) -->
		['Resource error: ~q'-[Resource], nl].
	error_tokens(syntax_error(Error)) -->
		['Syntax error: ~q'-[Error], nl].
	error_tokens(system_error) -->
		['System error'-[], nl].
	% catchall clause
	error_tokens(Error) -->
		['~q'-[Error], nl].

	term_tokens(entity(Type, Entity)) -->
		['  in ~w ~q'-[Type, Entity], nl].
	term_tokens(directive(Directive)) -->
		['  in directive :- ~q'-[Directive], nl].
	term_tokens(clause(Clause)) -->
		['  in clause ~q'-[Clause], nl].
	term_tokens(grammar_rule(Rule)) -->
		['  in grammar rule ~q'-[Rule], nl].
	term_tokens(term(Term)) -->
		['  in term ~q'-[Term], nl].
	term_tokens(Term) -->
		['  in ~q'-[Term], nl].

	term_type_tokens((:- Term)) -->
		(	{var(Term)} ->
			[]
		;	['  in directive '-[]], predicate_indicator_tokens(Term), [nl]
		).
	term_type_tokens((Term :- _)) -->
		(	{var(Term)} ->
			[]
		;	['  in clause for predicate '-[]], predicate_indicator_tokens(Term), [nl]
		).
	term_type_tokens((Term, _ --> _)) -->
		(	{var(Term)} ->
			[]
		;	['  in grammar rule for non-terminal '-[]], non_terminal_indicator_tokens(Term), [nl]
		).
	term_type_tokens((Term --> _)) -->
		(	{var(Term)} ->
			[]
		;	['  in grammar rule for non-terminal '-[]], non_terminal_indicator_tokens(Term), [nl]
		).
	term_type_tokens(Term) -->		% facts
		(	{var(Term)} ->
			[]
		;	['  in clause for predicate '-[]], predicate_indicator_tokens(Term), [nl]
		).

	predicate_indicator_tokens(Entity::Term) -->
		(	{var(Term)} ->
			['~q'-[Entity::Term]]
		;	{functor(Term, Functor, Arity)},
			['~q'-[Entity::Functor/Arity]]
		).
	predicate_indicator_tokens(Term) -->
		{functor(Term, Functor, Arity)},
		['~q'-[Functor/Arity]].

	non_terminal_indicator_tokens(Entity::Term) -->
		(	{var(Term)} ->
			['~q'-[Entity::Term]]
		;	{functor(Term, Functor, Arity)},
			['~q'-[Entity::Functor//Arity]]
		).
	non_terminal_indicator_tokens(Term) -->
		{functor(Term, Functor, Arity)},
		['~q'-[Functor//Arity]].

	message_context(File, Lines, Type, Entity) -->
		['  while compiling ~w ~q'-[Type, Entity], nl],
		message_context(File, Lines).

	message_context(File, Lines) -->
		(	{Lines = Line-Line} ->
			['  in file ~w at or above line ~d'-[File, Line], nl]
		;	['  in file ~w between lines ~w'-[File, Lines], nl]
		).

	missing_entities([]) -->
		[].
	missing_entities([Entity-reference(Kind,From,File,Line)| Entities]) -->
		['  ~q'-[Entity], nl],
		['    referenced from ~w ~q'-[Kind,From], nl],
		make_warning_context(File, Line),
		missing_entities(Entities).

	missing_predicates([]) -->
		[].
	missing_predicates([Predicate-reference(Kind,From,File,Line)| Predicates]) -->
		['  ~q'-[Predicate], nl],
		['    referenced from ~w ~q'-[Kind,From], nl],
		make_warning_context(File, Line),
		missing_predicates(Predicates).

	circular_references([]) -->
		[].
	circular_references([CircularReference-references(FileLines)| CircularReferences]) -->
		['  ~q'-[CircularReference], nl],
		circular_reference_file_lines(FileLines),
		circular_references(CircularReferences).

	circular_reference_file_lines([]) -->
		[].
	circular_reference_file_lines([File-Line| FileLines]) -->
		make_warning_context(File, Line),
		circular_reference_file_lines(FileLines).

	make_warning_context(File, Line) -->
		(	{File == ''} ->
			[]
		;	['    in file ~w at line ~w'-[File,Line], nl]
		).

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

:- end_category.
