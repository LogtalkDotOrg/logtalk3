%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:143:0,
		author is 'Paulo Moura',
		date is 2025-01-02,
		comment is 'Logtalk core (compiler and runtime) default message tokenization.'
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
	message_prefix_stream(debug,          '>>> ',   user_error).
	message_prefix_stream(debug(_),       '>>> ',   user_error).

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
		['[ compiling ~w '-[File]],
		(	{current_logtalk_flag(debug, on)} ->
			['in debug mode '-[]]
		;	{current_logtalk_flag(optimize, on)} ->
			['in optimal mode '-[]]
		;	[]
		),
		(	{current_logtalk_flag(hook, Hook)} ->
			{ground_term_copy(Hook, GroundHook)},
			['using the hook object ~q ... ]'-[GroundHook], nl]
		;	['... ]'-[], nl]
		).

	message_tokens(compiled_file(File, _Flags)) -->
		['[ ~w compiled ]'-[File], nl].

	message_tokens(up_to_date_file(File, _Flags)) -->
		['[ compiling ~w ... up-to-date ]'-[File], nl].

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

	message_tokens(reload_files_in_mode(Mode)) -->
		['Reloading files in ~w mode'-[Mode], nl].

	message_tokens(modified_files_reloaded) -->
		[	'Reloaded all Logtalk source files modified or that required'-[], nl,
			'recompilation due to a change to the compilation mode'-[], nl
		].

	message_tokens(intermediate_files_deleted) -->
		['Deleted all intermediate files for the loaded Logtalk source files'-[], nl].

	message_tokens(scanning_for_missing_entities_predicates) -->
		['Scanning for missing entities and predicates ...'-[], nl].

	message_tokens(completed_scanning_for_missing_entities_predicates) -->
		['... completed scanning for missing entities and predicates'-[], nl].

	message_tokens(scanning_for_duplicated_library_aliases) -->
		['Scanning for duplicated library aliases ...'-[], nl].

	message_tokens(scanning_for_library_paths_end_slash) -->
		['Scanning for library paths not ending with a slash ...'-[], nl].

	message_tokens(completed_scanning_of_library_alias_definitions) -->
		['... completed scanning of library alias definitions'-[], nl].

	message_tokens(scanning_for_circular_dependencies) -->
		['Scanning for circular entity dependencies ...'-[], nl].

	message_tokens(completed_scanning_for_circular_dependencies) -->
		['... completed scanning for circular entity dependencies'-[], nl].

	message_tokens(running_all_defined_documentation_actions) -->
		['Running all defined documentation actions ...'-[], nl].

	message_tokens(dynamic_binding_caches_deleted) -->
		['Dynamic binding caches deleted'-[], nl].

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

	message_tokens(duplicated_library_aliases(Duplicates)) -->
		(	{Duplicates = [Duplicate]} ->
			['Duplicated library alias: ~q'-[Duplicate], nl]
		;	['Duplicated library aliases: ~q'-[Duplicates], nl]
		).

	message_tokens(library_paths_dont_end_with_slash(Paths)) -->
		(	{Paths = [Path]} ->
			['Library path does not end with slash: ~q'-[Path], nl]
		;	['Library paths do not end with slash: ~q'-[Paths], nl]
		).

	% startup messages

	message_tokens(possibly_incompatible_prolog_version(v(Major,Minor,Patch), v(Major0,Minor0,Patch0))) -->
		[	'Possibly incompatible backend Prolog compiler version detected!'-[], nl,
			'Running version ~d.~d.~d but version ~d.~d.~d (or later) is advised.'-[Major, Minor, Patch, Major0, Minor0, Patch0], nl
		].

	message_tokens(banner) -->
		{current_logtalk_flag(version_data, logtalk(Major, Minor, Patch, Status))},
		(	{Status == stable} ->
			[nl, 'Logtalk ~d.~d.~d'-[Major, Minor, Patch], nl, 'Copyright (c) 1998-2025 Paulo Moura'-[], nl, nl]
		;	[nl, 'Logtalk ~d.~d.~d-~w'-[Major, Minor, Patch, Status], nl, 'Copyright (c) 1998-2025 Paulo Moura'-[], nl, nl]
		).

	message_tokens(default_flags) -->
		default_lint_flags,
		default_optional_features_flags,
		default_other_flags,
		default_backend_flags,
		default_read_only_flags.

	% help

	message_tokens(help) -->
		[	'For Logtalk help on libraries, entities, and built-in features:'-[], nl,
			'?- {help(loader)}. or ?- logtalk_load(help(loader)).'-[], nl, nl,
			'For Logtalk compiler and developer tools warnings and errors explanations:'-[], nl,
			'?- {tutor(loader)}. or ?- logtalk_load(tutor(loader)).'-[], nl, nl
		].

	% settings files messages

	message_tokens(loaded_settings_file(Path)) -->
		['Loaded settings file found on directory ~w'-[Path], nl, nl].
	message_tokens(settings_file_disabled) -->
		['Loading of settings file disabled in the backend Prolog compiler adapter file.'-[], nl, nl].
	message_tokens(error_loading_settings_file(Path)) -->
		['Errors found while loading settings file from directory ~w'-[Path], nl, nl].
	message_tokens(no_settings_file_found(allow)) -->
		[	'No settings file found in startup, Logtalk user, home, or config directories.'-[], nl,
			'Using default flag values set in the backend Prolog compiler adapter file.'-[], nl, nl
		].
	message_tokens(no_settings_file_found(restrict)) -->
		[	'No settings file found in the Logtalk user or user home directories.'-[], nl,
			'Using default flag values set in the backend Prolog compiler adapter file.'-[], nl, nl
		].

	% debugging messages

	message_tokens(logtalk_debugger_aborted) -->
		['Debugging session aborted by user. Debugger still on.'-[], nl].

	% runtime error

	message_tokens(runtime_error(Error)) -->
		error_term_tokens(Error),
		[nl].

	% compiler error and warning messages

	message_tokens(loading_failure(File)) -->
		[	'Unexpected failure while loading the code generated for the file:'-[], nl,
			'  ~w'-[File], nl,
			'Likely bug in the backend Prolog compiler. Please file a bug report.'-[], nl
		].

	message_tokens(loading_error(File, Error)) -->
		[	'Unexpected error while loading the code generated for the file:'-[], nl,
			'  ~w'-[File], nl,
			'  '-[]
		],
		error_term_tokens(Error),
		['Likely bug in the backend Prolog compiler. Please file a bug report.'-[], nl
		].

	message_tokens(compiler_error(File, Lines, Error)) -->
		error_term_tokens(Error),
		error_message_context(File, Lines).

	message_tokens(compiler_stream_error(Error)) -->
		error_term_tokens(Error),
		[nl].

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
		['Redefining Logtalk built-in predicate: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(redefined_prolog_built_in_predicate(File, Lines, Type, Entity, Predicate)) -->
		['Redefining Prolog built-in predicate: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(redefined_operator(File, Lines, Type, Entity, OriginalSpec, Spec)) -->
		['Redefining standard operator ~q as ~q'-[OriginalSpec, Spec], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(redefined_operator(File, Lines, OriginalSpec, Spec)) -->
		['Redefining standard operator ~q as ~q'-[OriginalSpec, Spec], nl],
		message_context(File, Lines).

	message_tokens(goal_is_always_true(File, Lines, Type, Entity, Goal)) -->
		['Goal is always true: ~q'-[Goal], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(goal_is_always_false(File, Lines, Type, Entity, Goal)) -->
		['Goal is always false: ~q'-[Goal], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(goal_is_always_error(File, Lines, Type, Entity, Goal, Error)) -->
		['Goal is always an error: ~q (~q)'-[Goal, Error], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(no_matching_clause_for_predicate_goal(File, Lines, Type, Entity, Goal)) -->
		['No matching clause for predicate goal: ~q'-[Goal], nl],
		message_context(File, Lines, Type, Entity).
	message_tokens(no_matching_clause_for_non_terminal_goal(File, Lines, Type, Entity, Goal)) -->
		['No matching clause for non-terminal goal: ~q'-[Goal], nl],
		message_context(File, Lines, Type, Entity).

	% unknown entity messages

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

	message_tokens(module_used_as_object(File, Lines, Type, Entity, Module)) -->
		['Reference to unknown object but there is a module with the same name: ~q'-[Module], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(missing_predicate_directive(File, Lines, Type, Entity, DirectiveFunctor/DirectiveArity, Predicate)) -->
		% avoid extraneous parenthesis due to directives names being declared as operators in some backends
		['Missing ~w/~w directive for '-[DirectiveFunctor, DirectiveArity]],
		(	{Predicate = _/_} ->
			['predicate: ~q'-[Predicate], nl]
		;	{Predicate = _::_/_} ->
			['predicate: ~q'-[Predicate], nl]
		;	{Predicate = ':'(_, _/_)} ->
			['predicate: ~q'-[Predicate], nl]
		;	['non-terminal: ~q'-[Predicate], nl]
		),
		message_context(File, Lines, Type, Entity).

	message_tokens(missing_predicate_directive(File, Lines, Type, Entity, Directive)) -->
		['Missing predicate directive: ~q.'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(missing_scope_directive(File, Lines, Type, Entity, _Directive, Functor/Arity)) -->
		['Missing scope directive for predicate: ~q'-[Functor/Arity], nl],
		message_context(File, Lines, Type, Entity).
	message_tokens(missing_scope_directive(File, Lines, Type, Entity, _Directive, Functor//Arity)) -->
		['Missing scope directive for non-terminal: ~q'-[Functor//Arity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(duplicated_predicate_reference(File, Lines, Type, Entity, Predicate, OriginalFile, OriginalLines)) -->
		['Duplicated predicate reference in directive: ~q'-[Predicate], nl],
		first_found_at(OriginalFile, OriginalLines, File),
		message_context(File, Lines, Type, Entity).
	message_tokens(duplicated_non_terminal_reference(File, Lines, Type, Entity, NonTerminal, OriginalFile, OriginalLines)) -->
		['Duplicated non-terminal reference in directive: ~q'-[NonTerminal], nl],
		first_found_at(OriginalFile, OriginalLines, File),
		message_context(File, Lines, Type, Entity).

	message_tokens(duplicated_directive(File, Lines, Type, Entity, Directive, OriginalFile, OriginalLines)) -->
		['Duplicated directive: ~q'-[Directive], nl],
		first_found_at(OriginalFile, OriginalLines, File),
		message_context(File, Lines, Type, Entity).
	message_tokens(duplicated_directive(File, Lines, Type, Entity, Directive)) -->
		['Duplicated directive: ~q'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(suspicious_call(File, Lines, Type, Entity, Call, [AlternativeCall| AlternativeCalls])) -->
		['Suspicious call: ~q instead of '-[Call]],
		alternative_calls(AlternativeCalls, AlternativeCall),
		message_context(File, Lines, Type, Entity).
	message_tokens(suspicious_call(File, Lines, Type, Entity, Call, reason(Reason))) -->
		['Suspicious call: ~q '-[Call]],
		suspicious_call_reason(Reason),
		message_context(File, Lines, Type, Entity).

	message_tokens(non_tail_recursive_predicate(File, Lines, Type, Entity, Predicate)) -->
		['Non-tail recursive predicate definition: ~q '-[Predicate], nl],
		message_context(File, Lines, Type, Entity).
	message_tokens(non_tail_recursive_non_terminal(File, Lines, Type, Entity, NonTerminal)) -->
		['Non-tail recursive non-terminal definition: ~q '-[NonTerminal], nl],
		message_context(File, Lines, Type, Entity).

	% portability messages

	message_tokens(non_standard_predicate_call(File, Lines, Type, Entity, Predicate)) -->
		['Call to non-standard Prolog built-in predicate: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_standard_arithmetic_function_call(File, Lines, Type, Entity, Function)) -->
		['Call to non-standard Prolog arithmetic function: ~q'-[Function], nl],
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

	message_tokens(non_standard_predicate_option(File, Lines, Type, Entity, Predicate, Option)) -->
		['Use of non-standard ~w predicate option: ~q'-[Predicate, Option], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(logtalk_built_in_predicate_as_directive(File, Lines, Directive)) -->
		['Use of Logtalk built-in predicate as a directive: ~q'-[Directive], nl],
		message_context(File, Lines).

	message_tokens(top_level_shortcut_as_directive(File, Lines, Directive)) -->
		['Use of Logtalk or Prolog top-level shortcut as a directive: ~q'-[Directive], nl],
		message_context(File, Lines).

	message_tokens(missing_function(File, Lines, Type, Entity, Function)) -->
		['Missing arithmetic function: ~q'-[Function], nl],
		message_context(File, Lines, Type, Entity).

	% lambda expression messages

	message_tokens(parameter_variable_used_elsewhere(File, Lines, Type, Entity, Lambda, Variable)) -->
		['Lambda expression ~q parameter variable ~q used elsewhere in clause'-[Lambda, Variable], nl],
		message_context(File, Lines, Type, Entity).

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

	% Prolog dialect specific term- and goal-expansion messages

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

	% other warning messages

	message_tokens(redundant_entity_qualifier_in_predicate_directive(File, Lines, Type, Entity, QualifiedResource)) -->
		['Redundant entity qualification in predicate directive argument: ~q'-[QualifiedResource], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(complementing_category_ignored(File, Lines, Category, Object)) -->
		[	'Complementing category will be ignored: ~q'-[Category], nl,
			'Complemented object, ~q, compiled with complementing categories support turned off'-[Object], nl
		],
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

	message_tokens(message_not_understood(File, Lines, Type, Entity, Obj, Pred)) -->
		{functor(Pred, Name, Arity)},
		['Unknown message sent to object: ~q'-[Obj::Name/Arity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(message_not_understood(File, Lines, Obj, Pred)) -->
		{functor(Pred, Name, Arity)},
		['Unknown message sent to object: ~q'-[Obj::Name/Arity], nl],
		message_context(File, Lines).

	message_tokens(unknown_module_predicate(File, Lines, Type, Entity, Module, Pred)) -->
		{functor(Pred, Name, Arity)},
		['Unknown module predicate: ~q'-[':'(Module, Name/Arity)], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(unknown_module_predicate(File, Lines, Module, Pred)) -->
		{functor(Pred, Name, Arity)},
		['Unknown module predicate: ~q'-[':'(Module, Name/Arity)], nl],
		message_context(File, Lines).

	message_tokens(missing_reference_to_built_in_protocol(File, Lines, Type, Entity, Protocol)) -->
		['Missing reference to the built-in protocol: ~q'-[Protocol], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(compiling_proprietary_prolog_directive(File, Lines, Type, Entity, Directive)) -->
		['Compiling proprietary Prolog directive: ~q'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(compiling_query_as_initialization_goal(File, Lines, Type, Entity, Directive)) -->
		['Compiling query as an initialization goal: ~q'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(invalid_logtalk_load_context_key(File, Lines, Type, Entity, Key)) -->
		['Invalid logtalk_load_context/2 predicate key: ~q'-[Key], nl],
		message_context(File, Lines, Type, Entity).
	message_tokens(invalid_logtalk_load_context_key(File, Lines, Key)) -->
		['Invalid logtalk_load_context/2 predicate key: ~q'-[Key], nl],
		message_context(File, Lines).

	message_tokens(unsupported_directive(File, Lines, Type, Entity, Directive)) -->
		['Unsupported directive (with current backend): ~q'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(unsupported_directive(File, Lines, Directive)) -->
		['Unsupported directive (with current backend): ~q'-[Directive], nl],
		message_context(File, Lines).

	% singleton variable messages

	message_tokens(singleton_variables(File, Lines, Type, Entity, Names, _Term)) -->
		(	{Names = [Name]} ->
			['Singleton variable: ~w'-[Name], nl]
		;	['Singleton variables: ~w'-[Names], nl]
		),
		message_context(File, Lines, Type, Entity).

	message_tokens(singleton_variables(File, Lines, Names, _Term)) -->
		(	{Names = [Name]} ->
			['Singleton variable: ~w'-[Name], nl]
		;	['Singleton variables: ~w'-[Names], nl]
		),
		message_context(File, Lines).

	% deprecated feature messages

	message_tokens(deprecated_date_format(File, Lines, Type, Entity, Deprecated, Advised)) -->
		['Deprecated date format: ~w (use instead ISO 8601 format ~w)'-[Deprecated, Advised], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_version_format(File, Lines, Type, Entity, Deprecated)) -->
		['Deprecated version format: ~w (use instead a Major:Minor:Patch compound term)'-[Deprecated], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_compiler_flag(File, Lines, Type, Entity, Flag, NewFlag)) -->
		['Deprecated compiler flag: ~w (renamed to ~w)'-[Flag, NewFlag], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_compiler_flag(File, Lines, Flag, NewFlag)) -->
		['Deprecated compiler flag: ~w (renamed to ~w)'-[Flag, NewFlag], nl],
		message_context(File, Lines).

	message_tokens(deprecated_control_construct(File, Lines, Type, Entity, Construct)) -->
		['Deprecated control construct: ~q'-[Construct], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_construct(File, Lines, Type, Entity, Construct, Replacement)) -->
		['Deprecated construct: ~q (compiled as ~q)'-[Construct, Replacement], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_directive(File, Lines, Type, Entity, Directive)) -->
		['Deprecated directive: ~q'-[Directive], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_predicate(File, Lines, Type, Entity, Predicate)) -->
		['Deprecated predicate: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_predicate(File, Lines, Type, Entity, Predicate, Replacement)) -->
		['Deprecated predicate: ~q (compiled as a call to ~q)'-[Predicate, Replacement], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_function(File, Lines, Type, Entity, Function)) -->
		['Deprecated function: ~q'-[Function], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(deprecated_function(File, Lines, Type, Entity, Function, Replacement)) -->
		['Deprecated function: ~q (replaceable by the standard ~q function)'-[Function, Replacement], nl],
		message_context(File, Lines, Type, Entity).

	% encoding/1 directive messages

	message_tokens(ignored_encoding_directive(File, Lines)) -->
		['The encoding/1 directive is ignored'-[], nl],
		message_context(File, Lines).

	message_tokens(misplaced_encoding_directive(File, Lines)) -->
		['The encoding/1 directive is misplaced'-[], nl],
		message_context(File, Lines).

	% steadfastness messages

	message_tokens(possible_non_steadfast_predicate(File, Lines, Type, Entity, Predicate)) -->
		['Predicate ~q may not be steadfast due to cut and variable aliasing in clause head'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(possible_non_steadfast_non_terminal(File, Lines, Type, Entity, NonTerminal)) -->
		['Non-terminal ~q may not be steadfast due to cut and variable aliasing in grammar rule head'-[NonTerminal], nl],
		message_context(File, Lines, Type, Entity).

	% disjunction guidelines messages

	message_tokens(disjunction_as_body(File, Lines, Type, Entity, Head, _)) -->
		{functor(Head, Name, Arity)},
		['Predicate ~q clause body is a disjunction'-[Name/Arity], nl],
		message_context(File, Lines, Type, Entity).

	% grammar rules

	message_tokens(calls_non_terminal_as_predicate(File, Lines, Type, Entity, NonTerminal)) -->
		['Non-terminal called as a predicate: ~q'-[NonTerminal], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(calls_predicate_as_non_terminal(File, Lines, Type, Entity, Predicate)) -->
		['Predicate called as a non-terminal: ~q'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(unsound_construct_in_grammar_rule(File, Lines, Type, Entity, GRBody)) -->
		['~q is not a sound construct in a grammar rule'-[GRBody], nl],
		message_context(File, Lines, Type, Entity).

	% left-recursion

	message_tokens(left_recursion(File, Lines, Type, Entity, Term)) -->
		(	{functor(Term, (-->), 2)} ->
			['Left-recursion in grammar rule: ~q'-[Term], nl]
		;	['Left-recursion in clause: ~q'-[Term], nl]
		),
		message_context(File, Lines, Type, Entity).

	% suspicious cuts

	message_tokens(suspicious_cut_in_if_then_else(File, Lines, Type, Entity, Head)) -->
		{functor(Head, Name, Arity)},
		['Suspicious cut in conditional test in clause for predicate: ~q'-[Name/Arity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(suspicious_cut_in_if_then_else(File, Lines, Type, Entity, Head, _If)) -->
		{functor(Head, Name, Arity)},
		['Predicate ~q clause possibly missing parenthesis around if-then-else'-[Name/Arity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(suspicious_cut_in_soft_cut(File, Lines, Type, Entity, Head)) -->
		{functor(Head, Name, Arity)},
		['Suspicious cut in conditional test in clause for predicate: ~q'-[Name/Arity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(suspicious_cut_in_soft_cut(File, Lines, Type, Entity, Head, _If)) -->
		{functor(Head, Name, Arity)},
		['Predicate ~q clause possibly missing parenthesis around the soft-cut'-[Name/Arity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(suspicious_cut_in_disjunction(File, Lines, Type, Entity, Head)) -->
		{functor(Head, Name, Arity)},
		['Suspicious cut as disjunction left goal in clause for predicate: ~q'-[Name/Arity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(suspicious_cut_in_disjunction(File, Lines, Type, Entity, _Head, Disjunction)) -->
		['Suspicious cut in disjunction left goal: ~q'-[Disjunction], nl],
		message_context(File, Lines, Type, Entity).

	% suspicious tests in if-then-else and soft-cut control constructs

	message_tokens(suspicious_if_then_else_test(File, Lines, Type, Entity, _Head, Test)) -->
		['If-then-else test is a unification between a variable and a ground term: ~q'-[Test], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(suspicious_soft_cut_test(File, Lines, Type, Entity, _Head, Test)) -->
		['Soft-cut test is a unification between a variable and a ground term: ~q'-[Test], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(missing_else_part(File, Lines, Type, Entity, Construct)) -->
		['Else part of the conditional is missing: ~q'-[Construct], nl],
		message_context(File, Lines, Type, Entity).

	% catch/3 goals that catch all exceptions

	message_tokens(catchall_catch(File, Lines, Type, Entity, Goal)) -->
		['The ~q goal catches all exceptions'-[Goal], nl],
		message_context(File, Lines, Type, Entity).

	% naming guidelines messages

	message_tokens(camel_case_entity_name(File, Lines, Type, Entity)) -->
		['Entity name in camel case: ~w'-[Entity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(entity_name_with_digits_in_the_middle(File, Lines, Type, Entity)) -->
		['Entity name with digits in the middle: ~w'-[Entity], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(camel_case_predicate_name(File, Lines, Type, Entity, Predicate)) -->
		['Predicate name in camel case: ~w'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(camel_case_non_terminal_name(File, Lines, Type, Entity, NonTerminal)) -->
		['Non-terminal name in camel case: ~w'-[NonTerminal], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(predicate_name_with_digits_in_the_middle(File, Lines, Type, Entity, Predicate)) -->
		['Predicate name with digits in the middle: ~w'-[Predicate], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_terminal_name_with_digits_in_the_middle(File, Lines, Type, Entity, NonTerminal)) -->
		['Non-terminal name with digits in the middle: ~w'-[NonTerminal], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_camel_case_variable_name(File, Lines, Type, Entity, Name)) -->
		['Variable name not in camel case: ~w'-[Name], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(non_camel_case_variable_name(File, Lines, Name)) -->
		['Variable name not in camel case: ~w'-[Name], nl],
		message_context(File, Lines).

	message_tokens(variable_name_with_digits_in_the_middle(File, Lines, Type, Entity, Name)) -->
		['Variable name with digits in the middle: ~w'-[Name], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(variable_name_with_digits_in_the_middle(File, Lines, Name)) -->
		['Variable name with digits in the middle: ~w'-[Name], nl],
		message_context(File, Lines).

	message_tokens(variable_names_differ_only_on_case(File, Lines, Type, Entity, Name, OtherName)) -->
		['Variables differ only on case: ~w ~w'-[Name, OtherName], nl],
		message_context(File, Lines, Type, Entity).

	message_tokens(variable_names_differ_only_on_case(File, Lines, Name, OtherName)) -->
		['Variables differ only on case: ~w ~w'-[Name, OtherName], nl],
		message_context(File, Lines).

	message_tokens(file_and_entity_names_differ(File, Entity)) -->
		['Source file basename differ from file entity name: ~q'-[Entity], nl],
		message_context(File).

	% duplicated clause and grammar rule messages

	message_tokens(duplicated_clause(File, Lines, Type, Entity, Clause, OriginalFile, OriginalLines)) -->
		['Duplicated clause: ~q'-[Clause], nl],
		first_found_at(OriginalFile, OriginalLines, File),
		message_context(File, Lines, Type, Entity).

	message_tokens(duplicated_grammar_rule(File, Lines, Type, Entity, GrammarRule, OriginalFile, OriginalLines)) -->
		['Duplicated grammar rule: ~q'-[GrammarRule], nl],
		first_found_at(OriginalFile, OriginalLines, File),
		message_context(File, Lines, Type, Entity).

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
		error_tokens(Error),
		term_tokens(term).

	% based on the ISO Prolog Core standard
	error_tokens(instantiation_error) -->
		['Instantiation error'-[], nl].
	error_tokens(type_error(ValidType, Culprit)) -->
		['Type error: expected ~q but got ~q'-[ValidType, Culprit], nl].
	error_tokens(domain_error(ValidDomain, Culprit)) -->
		['Domain error: value ~q is not in domain ~q'-[Culprit, ValidDomain], nl].
	error_tokens(consistency_error(Consistency, Argument1, Argument2)) -->
		['Consistency error: ~q expected for arguments ~q and ~q'-[Consistency, Argument1, Argument2], nl].
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

	term_tokens(logtalk(Goal, c(This, Entity, r(Sender, Self, MetaCallContext, Stack)))) -->
		[
			'  in goal: ~q'-[Goal], nl,
			'  with execution context:'-[], nl,
			'    entity:            ~q'-[Entity], nl,
			'    sender:            ~q'-[Sender], nl,
			'    this:              ~q'-[This], nl,
			'    self:              ~q'-[Self], nl,
			'    meta-call context: ~q'-[MetaCallContext], nl,
			'    coinduction stack: ~q'-[Stack], nl
		].
	term_tokens(entity(Type, Entity)) -->
		['  in ~w ~q'-[Type, Entity], nl].
	term_tokens(directive(Directive)) -->
		(	{callable(Directive)} ->
			{functor(Directive, Name, Arity)},
			['  in directive ~q/~w'-[Name, Arity], nl]
		;	['  in directive'-[], nl]
		).
	term_tokens(clause(Clause)) -->
		(	{Clause = '$VAR'(_)} ->
			['  in clause'-[], nl]
		;	{\+ callable(Clause)} ->
			['  in clause'-[], nl]
		;	{Clause = (Entity::Head :- _), callable(Head), Head \= '$VAR'(_)} ->
			{functor(Head, Name, Arity)},
			['  in clause for multifile predicate ~q::~q/~w'-[Entity, Name, Arity], nl]
		;	{Clause = (Head :- _), callable(Head), Head \= '$VAR'(_)} ->
			{functor(Head, Name, Arity)},
			['  in clause for predicate ~q/~w'-[Name, Arity], nl]
		;	{Clause \= (_ :- _), Clause = Entity::Head, callable(Head), Head \= '$VAR'(_)} ->
			{functor(Head, Name, Arity)},
			['  in clause for multifile predicate ~q::~q/~w'-[Entity, Name, Arity], nl]
		;	{Clause \= (_ :- _), callable(Clause)} ->
			{functor(Clause, Name, Arity)},
			['  in clause for predicate ~q/~w'-[Name, Arity], nl]
		;	['  in clause'-[], nl]
		).
	term_tokens(grammar_rule('-->'(Left, _))) -->
		(	{Left = '$VAR'(_)} ->
			['  in grammar rule'-[], nl]
		;	{\+ callable(Left)} ->
			['  in grammar rule'-[], nl]
		;	{Left = ','(Head, _), callable(Head), Head \= '$VAR'(_)} ->
			{functor(Head, Name, Arity)},
			['  in grammar rule for non-terminal ~q//~w'-[Name, Arity], nl]
		;	{Left \= ','(_, _), callable(Left)} ->
			{functor(Left, Name, Arity)},
			['  in grammar rule for non-terminal ~q//~w'-[Name, Arity], nl]
		;	['  in grammar rule'-[], nl]
		).
	term_tokens(term(Term)) -->
		['  in term ~q'-[Term], nl].
	term_tokens(term) -->
		['  in term'-[], nl].

	first_found_at(File, OriginalLines, File) -->
		(	{OriginalLines == 1-1} ->
			['  first found at line 1'-[], nl]
		;	{OriginalLines = Line-Line} ->
			['  first found at or above line ~d'-[Line], nl]
		;	['  first found between lines ~w'-[OriginalLines], nl]
		).
	first_found_at(OriginalFile, OriginalLines, _) -->
		(	{OriginalLines == 1-1} ->
			['  first found in file ~w at line 1'-[OriginalFile], nl]
		;	{OriginalLines = Line-Line} ->
			['  first found in file ~w at or above line ~d'-[OriginalFile, Line], nl]
		;	['  first found in file ~w between lines ~w'-[OriginalFile, OriginalLines], nl]
		).

	message_context(File, Lines, Type, Entity) -->
		['  while compiling ~w ~q'-[Type, Entity], nl],
		(	{Lines == 0-0} ->
			['  in auxiliary clause generated for file ~w'-[File], nl, nl]
		;	{Lines == 1-1} ->
			['  in file ~w at line 1'-[File], nl, nl]
		;	{Lines = Line-Line} ->
			['  in file ~w at or above line ~d'-[File, Line], nl, nl]
		;	['  in file ~w between lines ~w'-[File, Lines], nl, nl]
		).

	message_context(File, Lines) -->
		['  while compiling file'-[], nl],
		(	{Lines == 0-0} ->
			['  in auxiliary clause generated for file ~w'-[File], nl, nl]
		;	{Lines == 1-1} ->
			['  in file ~w at line 1'-[File], nl, nl]
		;	{Lines = Line-Line} ->
			['  in file ~w at or above line ~d'-[File, Line], nl, nl]
		;	['  in file ~w between lines ~w'-[File, Lines], nl, nl]
		).

	message_context(File) -->
		['  while compiling file ~w'-[File], nl, nl].

	error_message_context(File, Lines) -->
		(	{Lines == 0-0} ->
			['  in auxiliary clause generated for file ~w'-[File], nl, nl]
		;	{Lines == 1-1} ->
			['  in file ~w at line 1'-[File], nl, nl]
		;	{Lines = Line-Line} ->
			['  in file ~w at or above line ~d'-[File, Line], nl, nl]
		;	['  in file ~w between lines ~w'-[File, Lines], nl, nl]
		).

	alternative_calls([], AlternativeCall) -->
		['~q'-[AlternativeCall], nl].
	alternative_calls([NextAlternativeCall| AlternativeCalls], AlternativeCall) -->
		['~q or '-[AlternativeCall]],
		alternative_calls(AlternativeCalls, NextAlternativeCall).

	suspicious_call_reason(multifile(Entity::Head)) -->
		{functor(Head, Functor, Arity)},
		['in clause for multifile predicate ~q'-[Entity::Functor/Arity], nl].
	suspicious_call_reason(multifile(':'(Module,Head))) -->
		{functor(Head, Functor, Arity)},
		['in clause for multifile predicate ~q'-[':'(Module,Functor/Arity)], nl].
	suspicious_call_reason(multifile(Head)) -->
		{functor(Head, Functor, Arity)},
		['in clause for multifile predicate ~q'-[Functor/Arity], nl].
	suspicious_call_reason(repeat(Entity::Head)) -->
		{functor(Head, Functor, Arity)},
		['loop without a cut in clause for predicate ~q'-[Entity::Functor/Arity], nl].
	suspicious_call_reason(repeat(':'(Module,Head))) -->
		{functor(Head, Functor, Arity)},
		['loop without a cut in clause for predicate ~q'-[':'(Module,Functor/Arity)], nl].
	suspicious_call_reason(repeat(Head)) -->
		{functor(Head, Functor, Arity)},
		['loop without a cut in clause for predicate ~q'-[Functor/Arity], nl].
	suspicious_call_reason(shared_variable(Variable)) -->
		['as variable ~q occurs in expression'-[Variable], nl].
	suspicious_call_reason(no_shared_variables(Predicate)) -->
		(	{Predicate == forall} ->
			['as generator and test goals may share no variables'-[], nl]
		;	% assume bagof/setof/findall
			['as template and goal may share no variables'-[], nl]
		).
	suspicious_call_reason(cyclic_terms) -->
		['as unification will succeed creating a cyclic term'-[], nl].
	suspicious_call_reason(no_variable_bindings_after_unification) -->
		['as unification will succeed without any variable bindings'-[], nl].
	suspicious_call_reason(existential_variables([Variable], Goal)) -->
		['as existential variable ~q do not exist in goal ~q '-[Variable, Goal], nl].
	suspicious_call_reason(existential_variables([Variable1, Variable2| Variables], Goal)) -->
		['as existential variables ~q do not exist in goal ~q '-[[Variable1, Variable2| Variables], Goal], nl].
	suspicious_call_reason(float_comparison) -->
		['as the goal compares float values for equality'-[], nl].
	suspicious_call_reason(comparing_numbers_using_unification) -->
		['as the goal compares numbers using unification'-[], nl].
	suspicious_call_reason(singleton_variables(Predicate, _, [Singleton])) -->
		['in ~w goal contains singleton variable ~q'-[Predicate, Singleton], nl].
	suspicious_call_reason(singleton_variables(Predicate, _, [Singleton| Singletons])) -->
		['in ~w goal contains singleton variables ~q'-[Predicate, [Singleton| Singletons]], nl].
	suspicious_call_reason(as(Text)) -->
		['as ~w'-[Text], nl].
	suspicious_call_reason(due_to(Text)) -->
		['due to ~w'-[Text], nl].

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

	default_lint_flags -->
		{	current_logtalk_flag(unknown_predicates, UnknownPredicates0), align(UnknownPredicates0, UnknownPredicates),
			current_logtalk_flag(undefined_predicates, UndefinedPredicates),
			current_logtalk_flag(unknown_entities, UnknownEntities0), align(UnknownEntities0, UnknownEntities),
			current_logtalk_flag(steadfastness, Steadfastness),
			current_logtalk_flag(missing_directives, Missing0), align(Missing0, Missing),
			current_logtalk_flag(duplicated_directives, Duplicated),
			current_logtalk_flag(duplicated_clauses, DuplicatedClauses0), align(DuplicatedClauses0, DuplicatedClauses),
			current_logtalk_flag(portability, Portability),
			current_logtalk_flag(redefined_built_ins, RedefinedBuiltIns0), align(RedefinedBuiltIns0, RedefinedBuiltIns),
			current_logtalk_flag(redefined_operators, RedefinedOperators),
			current_logtalk_flag(trivial_goal_fails, Trivial0), align(Trivial0, Trivial),
			current_logtalk_flag(always_true_or_false_goals, Always),
			current_logtalk_flag(grammar_rules, GrammarRules0), align(GrammarRules0, GrammarRules),
			current_logtalk_flag(arithmetic_expressions, ArithmeticExpressions),
			current_logtalk_flag(lambda_variables, Lambda0), align(Lambda0, Lambda),
			current_logtalk_flag(suspicious_calls, SuspiciousCalls),
			current_logtalk_flag(disjunctions, Disjunctions0), align(Disjunctions0, Disjunctions),
			current_logtalk_flag(conditionals, Conditionals),
			current_logtalk_flag(singleton_variables, Singletons0), align(Singletons0, Singletons),
			current_logtalk_flag(catchall_catch, CatchallCatch),
			current_logtalk_flag(deprecated, Deprecated0), align(Deprecated0, Deprecated),
			current_logtalk_flag(naming, Naming),
			current_logtalk_flag(left_recursion, LeftRecursion0), align(LeftRecursion0, LeftRecursion),
			current_logtalk_flag(tail_recursive, TailRecursive),
			current_logtalk_flag(encodings, Encodings0), align(Encodings0, Encodings),
			current_logtalk_flag(general, General)
		},
		[
			'Default lint compilation flags: '-[], nl,
			'  unknown_predicates:   ~w    undefined_predicates:       ~w'-[UnknownPredicates, UndefinedPredicates], nl,
			'  unknown_entities:     ~w    steadfastness:              ~w'-[UnknownEntities, Steadfastness], nl,
			'  missing_directives:   ~w    duplicated_directives:      ~w'-[Missing, Duplicated], nl,
			'  duplicated_clauses:   ~w    portability:                ~w'-[DuplicatedClauses, Portability], nl,
			'  redefined_built_ins:  ~w    redefined_operators:        ~w'-[RedefinedBuiltIns, RedefinedOperators], nl,
			'  trivial_goal_fails:   ~w    always_true_or_false_goals: ~w'-[Trivial, Always], nl,
			'  grammar_rules:        ~w    arithmetic_expressions:     ~w'-[GrammarRules, ArithmeticExpressions], nl,
			'  lambda_variables:     ~w    suspicious_calls:           ~w'-[Lambda, SuspiciousCalls], nl,
			'  disjunctions:         ~w    conditionals:               ~w'-[Disjunctions, Conditionals], nl,
			'  singleton_variables:  ~w    catchall_catch:             ~w'-[Singletons, CatchallCatch], nl,
			'  deprecated:           ~w    naming:                     ~w'-[Deprecated, Naming], nl,
			'  left_recursion:       ~w    tail_recursive:             ~w'-[LeftRecursion, TailRecursive], nl,
			'  encodings:            ~w    general:                    ~w'-[Encodings, General], nl
		].

	default_optional_features_flags -->
		{
			current_logtalk_flag(dynamic_declarations, DynamicDeclarations0), align(DynamicDeclarations0, DynamicDeclarations),
			current_logtalk_flag(complements, Complements),
			current_logtalk_flag(context_switching_calls, ContextCalls0), align(ContextCalls0, ContextCalls),
			current_logtalk_flag(events, Events)
		},
		[
			'Default optional features compiler flags:'-[], nl,
			'  dynamic_declarations:    ~w  complements: ~w'-[DynamicDeclarations, Complements], nl,
			'  context_switching_calls: ~w  events:      ~w'-[ContextCalls, Events], nl
		].

	default_other_flags -->
		{
			current_logtalk_flag(report, Report0), align(Report0, Report),
			current_logtalk_flag(scratch_directory, ScratchDirectory),
			current_logtalk_flag(source_data, SourceData0), align(SourceData0, SourceData),
			current_logtalk_flag(code_prefix, Code),
			current_logtalk_flag(optimize, Optimize0), align(Optimize0, Optimize),
			current_logtalk_flag(debug, Debug),
			current_logtalk_flag(clean, Clean0), align(Clean0, Clean),
			current_logtalk_flag(reload, Reload),
			(current_logtalk_flag(hook, Hook) -> true; Hook = n/d),
			ground_term_copy(Hook, GroundHook)
		},
		[
			'Other default compilation flags:'-[], nl,
			'  report:      ~w      scratch_directory: ~w'-[Report, ScratchDirectory], nl,
			'  source_data: ~w      code_prefix:       ~q'-[SourceData, Code], nl,
			'  optimize:    ~w      debug:             ~w'-[Optimize, Debug], nl,
			'  clean:       ~w      reload:            ~w'-[Clean, Reload], nl,
			'  hook:        ~q'-[GroundHook], nl
		].

	default_backend_flags -->
		{
			current_logtalk_flag(prolog_dialect, PrologDialect),
			current_logtalk_flag(underscore_variables, Underscore),
			current_logtalk_flag(prolog_compiler, PrologCompiler),
			current_logtalk_flag(prolog_loader, PrologLoader)
		},
		[
			'Backend Prolog compiler file compilation and loading flags:'-[], nl,
			'  prolog_dialect:  ~w'-[PrologDialect], nl,
			'  underscore_variables: ~w'-[Underscore], nl,
			'  prolog_compiler: ~w'-[PrologCompiler], nl,
			'  prolog_loader:   ~w'-[PrologLoader], nl
		].

	default_read_only_flags -->
		{
			current_logtalk_flag(unicode, Unicode0), align(Unicode0, Unicode),
			current_logtalk_flag(encoding_directive, Encodings),
			current_logtalk_flag(engines, Engines0), align(Engines0, Engines),
			current_logtalk_flag(threads, Threads),
			current_logtalk_flag(modules, Modules0), align(Modules0, Modules),
			current_logtalk_flag(coinduction, Coinduction),
			current_logtalk_flag(tabling, Tabling)
		},
		[
			'Read-only compilation flags (backend Prolog compiler features):'-[], nl,
			'  unicode: ~w  encoding_directive: ~w'-[Unicode, Encodings], nl,
			'  engines: ~w  threads:            ~w'-[Engines, Threads], nl,
			'  modules: ~w  coinduction:        ~w '-[Modules, Coinduction], nl,
			'  tabling: ~w'-[Tabling], nl, nl
		].

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

	align(warning, warning).
	align(silent, 'silent ').
	align(allow, 'allow ').
	align(deny, 'deny  ').
	align(on, 'on ').
	align(off, off).
	align(unsupported, unsupported).
	align(supported, 'supported  ').
	align(full, 'full       ').
	align(bmp, 'bmp        ').
	align(source, 'source     ').

:- end_category.
