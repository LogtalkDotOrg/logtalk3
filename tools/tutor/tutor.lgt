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


:- object(tutor).

	:- info([
		version is 0:83:0,
		author is 'Paulo Moura',
		date is 2025-10-06,
		comment is 'This object adds explanations and suggestions to selected compiler warning and error messages.',
		remarks is [
			'Usage' - 'Simply load this object at startup using the goal ``logtalk_load(tutor(loader))``.'
		]
	]).

	:- public(explain//1).
	:- mode_non_terminal(explain(@callable), zero_or_one).
	:- info(explain//1, [
		comment is 'Generates an explanation for a message.',
		argnames is ['Message']
	]).

	% intercept all compiler warning and error messages

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, Kind, core, Tokens) :-
		message_hook(Message, Kind, core, Tokens).
	logtalk::message_hook(Message, Kind, lgtunit, Tokens) :-
		message_hook(Message, Kind, lgtunit, Tokens).
	logtalk::message_hook(Message, Kind, packs, Tokens) :-
		message_hook(Message, Kind, packs, Tokens).

	message_hook(Message, Kind, Component, Tokens) :-
		phrase(explain(Message), ExplanationTokens),
		% avoid empty line between the compiler message and its explanation
		(	list::append(Tokens0, [nl, nl], Tokens) ->
			list::append([begin(Kind,Ctx)| Tokens0], [nl| ExplanationTokens], ExtendedTokens0)
		;	list::append([begin(Kind,Ctx)| Tokens], ExplanationTokens, ExtendedTokens0)
		),
		% add begin/2 and end/1 tokens to enable message coloring
		% if supported by the backend Prolog compiler
		list::append(ExtendedTokens0, [end(Ctx)], ExtendedTokens),
		logtalk::message_prefix_stream(Kind, Component, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, ExtendedTokens),
		% take into account tools such as VSCode that require copying messages to a file for parsing
		(	logtalk::message_prefix_file(Kind, Component, Prefix, File, Mode, Options) ->
			open(File, Mode, FileStream, Options),
			logtalk::print_message_tokens(FileStream, Prefix, ExtendedTokens),
			close(FileStream)
		;	true
		).

	% errors

	error_term(error(error(Error, _), _)) -->
		error_term(error(Error, _)).
	error_term(error(Error, _)) -->
		error(Error).
	error_term(Error) -->
		error(Error).

	error(type_error(callable, _)) -->
		['Callable terms are either atoms or compound terms.'-[], nl, nl].
	error(type_error(predicate_alias_specification, _)) -->
		[	'Aliases are specified using the (as)/2 infix operator: Original as Alias.'-[], nl,
			'In alternative, the (::)/2 infix operator can also be used: Original::Alias.'-[], nl, nl
		].

	error(type_error(source_file_name, _)) -->
		[	'Source file names are specified using either an atom (representing a relative or'-[], nl,
			'absolute path, with or without file extension) or library notation (a compound'-[], nl,
			'term where the name is a library name and the single argument is an atom).'-[], nl, nl
		].
	error(type_error(library_path, _)) -->
		[	'Library notation syntax is a compound term where the name is a library name and'-[], nl,
			'the single argument is an atom representing a relative or absolute path, with or'-[], nl,
			'without file extension.'-[], nl, nl
		].

	error(type_error(object_identifier, _)) -->
		['Object identifiers must be either atoms or compound terms.'-[], nl, nl].
	error(type_error(category_identifier, _)) -->
		['Category identifiers must be either atoms or compound terms.'-[], nl, nl].
	error(type_error(protocol_identifier, _)) -->
		['Protocol identifiers must be atoms.'-[], nl, nl].

	error(domain_error(object_relation, Relation)) -->
		[	'The ~w relation is not a valid relation between an object'-[Relation], nl,
			'and other entities.'-[], nl, nl
		].
	error(domain_error(protocol_relation, Relation)) -->
		[	'The ~w relation is not a valid relation between a protocol'-[Relation], nl,
			'and other entities.'-[], nl, nl
		].
	error(domain_error(category_relation, Relation)) -->
		[	'The ~w relation is not a valid relation between a category'-[Relation], nl,
			'and other entities.'-[], nl, nl
		].

	error(domain_error(ancestor, _)) -->
		[	'The alias/2 directive must reference an entity ancestor (listed'-[], nl,
			'in the entity opening directive). Typo in the ancestor name?'-[], nl, nl
		].

	error(domain_error(class, _)) -->
		[	'An object can only instantiate or specialize an object playing the'-[], nl,
			'role of a class.'-[], nl, nl
		].
	error(domain_error(prototype, _)) -->
		['An object can only extend an object playing the role of a prototype.'-[], nl, nl].


	error(domain_error(compiler_flag, _)) -->
		[	'Compiler flags syntax is a compound term where the name is'-[], nl,
			'the flag name and the single argument is the flag value.'-[], nl, nl
		].

	error(domain_error(directive, '.'/2)) -->
		[	'The common Prolog top-level shortcut for loading files should never'-[], nl,
			'be used in source files, including as directives. To fix this error,'-[], nl,
			'either move the loading of the files to your application loader file'-[], nl,
			'or use an include/1 directive if the contents of those files should'-[], nl,
			'be part of an object or category containing the directive.'-[], nl, nl
		].
	error(domain_error(directive, (',')/2)) -->
		[	'Use individual directives instead of conjunction of directives'-[], nl,
			'to fix this error.'-[], nl, nl
		].
	error(domain_error(directive, set_prolog_flag/2)) -->
		[	'This standard Prolog directive can be used in a source file but not as'-[], nl,
			'an entity directive. It may be possible to fix this error by simply'-[], nl,
			'moving the directive to the top of the source file.'-[], nl, nl
		].
	error(domain_error(directive, _)) -->
		[	'This is not a Logtalk supported directive. If you are trying to use a'-[], nl,
			'proprietary Prolog directive, it may be possible to handle it in the'-[], nl,
			'Prolog compiler adapter file. If the directive should be copied as-is'-[], nl,
			'to the generated code, wrap it using the {}/1 control construct.'-[], nl, nl
		].
	error(domain_error(protocol_directive, threaded/0)) -->
		['The threaded/0 directive can only be used in objects.'-[], nl, nl].
	error(domain_error(category_directive, threaded/0)) -->
		['The threaded/0 directive can only be used in objects.'-[], nl, nl].
	error(domain_error(protocol_directive, (initialization)/1)) -->
		['The initialization/1 directive can only be used in objects.'-[], nl, nl].
	error(domain_error(category_directive, (initialization)/1)) -->
		['The initialization/1 directive can only be used in objects.'-[], nl, nl].

	error(domain_error(message_sending_goal, _)) -->
		[	'The argument of the message delegation control construct must be'-[], nl,
			'a (::)/2 message-sending goal.'-[], nl, nl
		].

	error(domain_error(meta_directive_template, _)) -->
		['Likely one on the meta-argument specifiers is not valid.'-[], nl, nl].
	error(domain_error(meta_argument_specifier, _)) -->
		[	'One (or more) of the meta-argument specifiers in the meta-predicate'-[], nl,
			'(or meta-non-terminal) template is ambiguous. A typical case is the'-[], nl,
			'module-aware specifier, ":", which can mean a goal, a closure, or a'-[], nl,
			'non-callable but module-aware argument. The warning may be fixed by'-[], nl,
			'overriding the meta-predicate directive.'-[], nl, nl
		].

	error(domain_error([1,_], _)) -->
		['The parameter position is outside the valid range. Typo?'-[], nl, nl].

	error(consistency_error(same_arity, _/_, _/_)) -->
		[	'The arity of these two predicate indicators is expected to be equal.'-[], nl,
			'Typo?'-[], nl, nl
		].
	error(consistency_error(same_arity, _//_, _//_)) -->
		[	'The arity of these two non-terminal indicators is expected to be'-[], nl,
			'equal. Typo?'-[], nl, nl
		].
	error(consistency_error(same_number_of_parameters, _, _)) -->
		[	'Inconsistent number of parameters between the entity name and'-[], nl,
			'its documentation. The numbers are expected to be equal. Typo?'-[], nl, nl
		].
	error(consistency_error(same_number_of_arguments, _, _)) -->
		[	'Inconsistent number of arguments between the predicate and its'-[], nl,
			'documentation. The numbers are expected to be equal. Typo?'-[], nl, nl
		].
	error(consistency_error(same_closure_specification, _, _)) -->
		[	'Inconsistent number of closure additional arguments and the predicate'-[], nl,
			'meta directive. The numbers are expected to be equal. Typo?'-[], nl, nl
		].

	error(permission_error(modify, object, _)) -->
		['An object already exists with this identifier.'-[], nl, nl].
	error(permission_error(modify, category, _)) -->
		['A category already exists with this identifier.'-[], nl, nl].
	error(permission_error(modify, protocol, _)) -->
		['A protocol already exists with this identifier.'-[], nl, nl].

	error(permission_error(modify, static_object, _)) -->
		['Static objects cannot be abolished.'-[], nl, nl].
	error(permission_error(modify, static_category, _)) -->
		['Static categories cannot be abolished.'-[], nl, nl].
	error(permission_error(modify, static_protocol, _)) -->
		['Static protocols cannot be abolished.'-[], nl, nl].

	error(permission_error(create, object, _)) -->
		['That {}/1 identifier is reserved for working with parametric object proxies.'-[], nl, nl].

	error(permission_error(create, engine, _)) -->
		['An engine with this identifier already exists.'-[], nl, nl].

	error(permission_error(create, predicate_declaration, _)) -->
		[	'The object does not allow dynamic declaration of new predicates.'-[], nl,
			'Set its "dynamic_declarations" flag to "allow" to fix this error.'-[], nl, nl
		].
	error(permission_error(modify, predicate_declaration, _)) -->
		['This predicate declaration properties cannot be modified.'-[], nl, nl].
	error(permission_error(modify, static_predicate, _)) -->
		[	'Clauses of static predicates cannot be retrieved nor modified.'-[], nl,
			'This error may be fixed by declaring the predicate dynamic.'-[], nl, nl
		].

	error(permission_error(modify, private_predicate, _)) -->
		['This predicate is out-of-scope of the caller trying to modify it.'-[], nl, nl].
	error(permission_error(modify, protected_predicate, _)) -->
		['This predicate is out-of-scope of the caller trying to modify it.'-[], nl, nl].
	error(permission_error(modify, built_in_method, _)) -->
		[	'Built-in methods cannot be redefined by the user.'-[], nl,
			'Use a different predicate name to fix this error.'-[], nl, nl
		].

	error(permission_error(modify, uses_object_predicate, _)) -->
		[	'This predicate is already listed in a uses/2 directive.'-[], nl,
			'Typo in the predicate indicator or a duplicated predicate reference?'-[], nl,
			'Trying to define a local clause for the same predicate?'-[], nl, nl
		].
	error(permission_error(modify, uses_module_predicate, _)) -->
		[	'This predicate is already listed in a use_module/2 directive.'-[], nl,
			'Typo in the predicate indicator or a duplicated predicate reference?'-[], nl,
			'Trying to define a local clause for the same predicate?'-[], nl, nl
		].
	error(permission_error(modify, uses_object_non_terminal, _)) -->
		[	'This non-terminal is already listed in a uses/2 directive.'-[], nl,
			'Typo in the predicate indicator or a duplicated predicate reference?'-[], nl,
			'Trying to define a local clause for the same predicate?'-[], nl, nl
		].
	error(permission_error(modify, uses_module_non_terminal, _)) -->
		[	'This non-terminal is already listed in a use_module/2 directive.'-[], nl,
			'Typo in the predicate indicator or a duplicated predicate reference?'-[], nl,
			'Trying to define a local clause for the same predicate?'-[], nl, nl
		].

	error(permission_error(modify, predicate_scope, _)) -->
		[	'This predicate is already declared in a previous scope directive.'-[], nl,
			'There can be only one scope directive per predicate.'-[], nl, nl
		].
	error(permission_error(modify, meta_predicate_template, _)) -->
		[	'A meta-predicate template is already declared in a previous directive.'-[], nl,
			'There can be only one meta-predicate directive per predicate within an'-[], nl,
			'entity.'-[], nl, nl
		].
	error(permission_error(modify, meta_non_terminal_template, _)) -->
		[	'A meta-non-terminal template is already declared in a previous directive.'-[], nl,
			'There can be only one meta-non-terminal directive per non-terminal within'-[], nl,
			'an entity.'-[], nl, nl
		].

	error(permission_error(access, private_predicate, _)) -->
		['The predicate is private and not within the scope of the sender or caller.'-[], nl, nl].
	error(permission_error(access, protected_predicate, _)) -->
		['The predicate is protected and not within the scope of the sender or caller.'-[], nl, nl].

	error(permission_error(access, static_predicate, _)) -->
		['The clause/2 built-in method can only access clauses for dynamic predicates.'-[], nl, nl].

	error(permission_error(modify, object_alias, _)) -->
		[	'This name is already declared as an alias to another object.'-[], nl,
			'Typo in the alias or the object name?'-[], nl, nl
		].
	error(permission_error(repeat, object_alias, _)) -->
		[	'This name is already declared as an alias to the same object.'-[], nl,
			'Simply delete the repeated declaration to fix this error.'-[], nl, nl
		].

	error(permission_error(declare, (dynamic), _/_)) -->
		[	'Category predicates cannot be declared as dynamic or as both multifile and'-[], nl,
			'dynamic as categories cannot contain clauses for dynamic predicates.'-[], nl, nl
		].
	error(permission_error(declare, (dynamic), _//_)) -->
		[	'Category non-terminals cannot be declared as both multifile and dynamic'-[], nl,
			'as categories cannot contain grammar rules for dynamic non-terminals.'-[], nl, nl
		].
	error(permission_error(declare, (multifile), _/_)) -->
		[	'Category predicates cannot be declared as both multifile and dynamic'-[], nl,
			'as categories cannot contain clauses for dynamic predicates.'-[], nl, nl
		].
	error(permission_error(declare, (multifile), _//_)) -->
		[	'Category non-terminals cannot be declared as both multifile and dynamic'-[], nl,
			'as categories cannot contain grammar rules for dynamic non-terminals.'-[], nl, nl
		].

	error(permission_error(define, dynamic_predicate, _)) -->
		[	'Categories cannot define clauses for dynamic predicates'-[], nl,
			'as they can be imported by any number of objects.'-[], nl, nl
		].
	error(permission_error(define, predicate, _)) -->
		[	'Protocols cannot contain predicate clauses or grammar rules,'-[], nl,
			'only entity, predicate, and grammar rule directives.'-[], nl, nl
		].
	error(permission_error(define, non_terminal, _)) -->
		[	'Protocols cannot contain predicate clauses or grammar rules,'-[], nl,
			'only predicate and grammar rule directives.'-[], nl, nl
		].

	error(permission_error(access, object, _)) -->
		[	'An object cannot delegate a message to the sender of the message'-[], nl,
			'being handled as that would allow breaking object encapsulation.'-[], nl, nl
		].
	error(permission_error(access, database, _)) -->
		[	'The object does not allow debugging context switch calls.'-[], nl,
			'Set its "context_switching_calls" flag to "allow" to fix this error.'-[], nl, nl
		].

	error(permission_error(implement, self, _)) -->
		['A protocol cannot implement itself. Typo in the object or category identifier?'-[], nl, nl].
	error(permission_error(import, self, _)) -->
		['A category cannot import itself. Typo in the object identifier?'-[], nl, nl].
	error(permission_error(instantiate, class, _)) -->
		['An object cannot instantiate a prototype, only a class.'-[], nl, nl].
	error(permission_error(specialize, self, _)) -->
		['A class cannot specialize itself. Typo in the superclass identifier?'-[], nl, nl].
	error(permission_error(specialize, class, _)) -->
		['An object cannot specialize a prototype, only a class.'-[], nl, nl].
	error(permission_error(extend, self, _)) -->
		['An entity cannot extend itself. Typo in the parent entity identifier?'-[], nl, nl].
	error(permission_error(extend, prototype, _)) -->
		['A prototype cannot extend an instance or a class. Typo in the parent object identifier?'-[], nl, nl].
	error(permission_error(complement, self, _)) -->
		['A category cannot complement itself. Typo in the object identifier?'-[], nl, nl].

	error(permission_error(repeat, entity_relation, implements/1)) -->
		['Write instead implements((P1, P2, ...)) or implements([P1, P2, ...])'-[], nl, nl].
	error(permission_error(repeat, entity_relation, imports/1)) -->
		['Write instead imports((C1, C2, ...)) or imports([C1, C2, ...])'-[], nl, nl].
	error(permission_error(repeat, entity_relation, instantiates/1)) -->
		['Write instead instantiates((C1, C2, ...)) or instantiates([C1, C2, ...])'-[], nl, nl].
	error(permission_error(repeat, entity_relation, specializes/1)) -->
		['Write instead specializes((C1, C2, ...)) or specializes([C1, C2, ...])'-[], nl, nl].
	error(permission_error(repeat, entity_relation, extends/1)) -->
		['Write instead extends((O1, O2, ...)) or extends([O1, O2, ...])'-[], nl, nl].
	error(permission_error(repeat, entity_relation, complements/1)) -->
		['Write instead complements((O1, O2, ...)) or complements([O1, O2, ...])'-[], nl, nl].

	error(permission_error(include, file, _)) -->
		['A source file cannot include itself. Typo in the file name?'-[], nl, nl].
	error(permission_error(load, file, _)) -->
		['A source file cannot load itself. Typo in the file name?'-[], nl, nl].

	error(permission_error(uses, self, _)) -->
		[	'An object cannot reference itself in a uses/2 directive.'-[], nl,
			'Typo in the object name?'-[], nl, nl
		].
	error(permission_error(use_module, self, _)) -->
		[	'A module (being compiled as an object) cannot reference itself'-[], nl,
			'in a use_module/2 directive. Typo in the object name?'-[], nl, nl
		].
	error(permission_error(reference, self, _)) -->
		[	'An entity cannot reference itself in an alias/2 directive.'-[], nl,
			'Typo in the entity name?'-[], nl, nl
		].

	error(existence_error(directive, object/1)) -->
		[	'Unmatched closing object directive found.'-[], nl,
			'Typo in the opening directive or wrong closing directive?'-[], nl, nl
		].
	error(existence_error(directive, category/1)) -->
		[	'Unmatched closing category directive found.'-[], nl,
			'Typo in the opening directive or wrong closing directive?'-[], nl, nl
		].
	error(existence_error(directive, protocol/1)) -->
		[	'Unmatched closing protocol directive found.'-[], nl,
			'Typo in the opening directive or wrong closing directive?'-[], nl, nl
		].

	error(existence_error(directive, end_object/0)) -->
		[	'Unexpected end-of-file while compiling an object.'-[], nl,
			'Typo in the closing directive or closing directive missing?'-[], nl, nl
		].
	error(existence_error(directive, end_protocol/0)) -->
		[	'Unexpected end-of-file while compiling a protocol.'-[], nl,
			'Typo in the closing directive or closing directive missing?'-[], nl, nl
		].
	error(existence_error(directive, end_category/0)) -->
		[	'Unexpected end-of-file while compiling a category.'-[], nl,
			'Typo in the closing directive or closing directive missing?'-[], nl, nl
		].

	error(existence_error(directive, if/0)) -->
		[	'Conditional compilation block directive missing.'-[], nl,
			'Typo in the opening directive or opening directive missing?'-[], nl, nl
		].
	error(existence_error(directive, endif/0)) -->
		[	'Unexpected end-of-file while compiling a conditional compilation block.'-[], nl,
			'Typo in the closing directive or closing directive missing?'-[], nl, nl
		].

	error(existence_error(directive, multifile(_))) -->
		['Multifile directives are mandatory.'-[], nl, nl].

	error(existence_error(object, _)) -->
		[	'The referenced object does not exist. Typo in the object name?'-[], nl,
			'Failure when loading the file defining the object?'-[], nl, nl
		].
	error(existence_error(protocol, _)) -->
		[	'The referenced protocol does not exist. Typo in the protocol name?'-[], nl,
			'Failure when loading the file defining the protocol?'-[], nl, nl
		].
	error(existence_error(category, _)) -->
		[	'The referenced category does not exist. Typo in the category name?'-[], nl,
			'Failure when loading the file defining the category?'-[], nl, nl
		].

	error(existence_error(file, _)) -->
		[	'Typo in the file name or in the file path? If using a relative path,'-[], nl,
			'also check that the current directory is the expected one.'-[], nl, nl
		].

	error(existence_error(ancestor, object)) -->
		['Compiling a "super" call but there is not ancestor object.'-[], nl, nl].
	error(existence_error(ancestor, category)) -->
		['Compiling a "super" call but there is not ancestor category.'-[], nl, nl].

	error(existence_error(procedure, _)) -->
		[	'The referenced predicate does not exist.'-[], nl,
			'Typo in the predicate name or number of arguments?'-[], nl,
			'Wrong scope for calling the predicate?'-[], nl, nl
		].
	error(existence_error(predicate_declaration, _)) -->
		[	'There is no declared predicate with that functor.'-[], nl,
			'Typo in the predicate name or number of arguments?'-[], nl, nl
		].

	error(existence_error(library, _)) -->
		[	'The referenced library does not exist. Typo in the library name?'-[], nl,
			'Libraries are defined using the logtalk_library_path/2 predicate.'-[], nl, nl
		].

	explain(compiler_error(_, _, Error)) -->
		error_term(Error).
	explain(runtime_error(Error)) -->
		error_term(Error).
	explain(compiler_stream_error(Error)) -->
		error_term(Error).

	% deprecated feature messages

	explain(deprecated_compiler_flag(_, _, _, _, _, _)) -->
		[	'Code that uses deprecated compiler flags will likely break when those'-[], nl,
			'flags are removed in future Logtalk versions.'-[], nl, nl
		].
	explain(deprecated_compiler_flag(_, _, _, _)) -->
		[	'Code that uses deprecated compiler flags will likely break when those'-[], nl,
			'flags are removed in future Logtalk versions.'-[], nl, nl
		].
	explain(deprecated_control_construct(_, _, _, _, _)) -->
		[	'Code that uses deprecated control constructs will likely break when those'-[], nl,
			'control constructs are removed in future Logtalk or Prolog versions.'-[], nl, nl
		].
	explain(deprecated_construct(_, _, _, _, _, _)) -->
		[	'Code that uses deprecated constructs may hinder portability.'-[], nl,
			'Use the suggested alternative instead to fix this warning.'-[], nl, nl
		].
	explain(deprecated_directive(_, _, _, _, _)) -->
		[	'Code that uses deprecated directives will likely break when those'-[], nl,
			'directives are removed in future Logtalk or Prolog versions.'-[], nl, nl
		].
	explain(deprecated_predicate(_, _, _, _, _)) -->
		[	'Code that calls deprecated predicates will likely break when those'-[], nl,
			'predicates are removed in future Logtalk or Prolog versions.'-[], nl, nl
		].
	explain(deprecated_predicate(_, _, _, _, _, _)) -->
		[	'Code that calls deprecated predicates will likely break when those'-[], nl,
			'predicates are removed in future Logtalk or Prolog versions.'-[], nl, nl
		].
	explain(deprecated_function(_, _, _, _, _)) -->
		[	'Code that calls deprecated arithmetic functions will likely break when'-[], nl,
			'those functions are removed in future Logtalk or Prolog versions.'-[], nl, nl
		].
	explain(deprecated_function(_, _, _, _, _, _)) -->
		[	'Code that calls deprecated arithmetic functions will likely break when'-[], nl,
			'those functions are removed in future Logtalk or Prolog versions.'-[], nl, nl
		].

	explain(deprecated_date_format(_, _, _, _, _, _)) -->
		[	'Date representation is changing to the ISO 8601 format to avoid'-[], nl,
			'ambiguity when interpreting dates.'-[], nl, nl
		].

	explain(deprecated_version_format(_, _, _, _, _)) -->
		[	'Version representation is changing to a compound term to simplify'-[], nl,
			'semantic versioning and forthcoming version management tools.'-[], nl, nl
		].

	% grammar rules

	explain(calls_non_terminal_as_predicate(_, _, _, _, _)) -->
		[	'Calls to non-terminals from predicates should always be made using the'-[], nl,
			'phrase/2-3 built-in methods instead of assuming how grammar rules are'-[], nl,
			'compiled into predicate clauses.'-[], nl, nl
		].

	explain(calls_predicate_as_non_terminal(_, _, _, _, _)) -->
		[	'Calls to predicates from non-terminals should always be made using the'-[], nl,
			'call//1 built-in method instead of assuming how grammar rules are'-[], nl,
			'compiled into predicate clauses.'-[], nl, nl
		].

	explain(unsound_construct_in_grammar_rule(_, _, _, _, \+ _)) -->
		[	'The use of this construct may result in unrestricted look ahead that may'-[], nl,
			'or may not be valid depending on the grammar rule implicit arguments. It'-[], nl,
			'is advisable to only use this construct with a {}/1 argument.'-[], nl, nl
		].

	explain(unsound_construct_in_grammar_rule(_, _, _, _, (_ -> _))) -->
		[	'Using this construct may result in an early commit that may or may not'-[], nl,
			'be valid depending on the grammar rule implicit arguments. It is advisable'-[], nl,
			'to only use this construct when the condition is a {}/1 argument.'-[], nl, nl
		].

	explain(unsound_construct_in_grammar_rule(_, _, _, _, '*->'(_, _))) -->
		[	'Using this construct may result in an early commit that may or may not'-[], nl,
			'be valid depending on the grammar rule implicit arguments. It is advisable'-[], nl,
			'to only use this construct when the condition is a {}/1 argument.'-[], nl, nl
		].

	% left-recursion

	explain(left_recursion(_, _, _, _, _)) -->
		[	'The use of left-recursion in clauses and grammar rules usually results in'-[], nl,
			'non-terminating programs (assuming the default SLD resolution inference.'-[], nl,
			'rule). Consider rewriting your code to use right-recursion. Tabling (when'-[], nl,
			'suportedby the backend) provides an alternative solution (due to its SLG'-[], nl,
			'resolution inference rule).'-[], nl, nl
		].

	% other warning messages

	explain(redundant_entity_qualifier_in_predicate_directive(_, _, _, _, _)) -->
		[	'Entity qualification in predicate directives should never be used'-[], nl,
			'when the entity is the same entity that contains the directives.'-[], nl, nl
		].

	explain(complementing_category_ignored(_, _, _, _)) -->
		[	'Set the object "complements" flag to "restrict" or "allow"'-[], nl,
			'to enable patching it using complementing categories.'-[], nl, nl
		].

	explain(declared_static_predicate_called_but_not_defined(_, _, _, _, _)) -->
		[	'Calls to declared, static, but undefined predicates fail. Predicate'-[], nl,
			'definition missing? Typo in the predicate name or number of arguments?'-[], nl,
			'Should the predicate be declared dynamic?'-[], nl, nl
		].
	explain(declared_static_non_terminal_called_but_not_defined(_, _, _, _, _)) -->
		[	'Calls to declared, static, but undefined grammar rules fail. Grammar'-[], nl,
			'rule definition missing? Typo in the grammar rule name or number of'-[], nl,
			'arguments? Should the grammar rule be declared dynamic?'-[], nl, nl
		].

	explain(unknown_predicate_called_but_not_defined(_, _, _, _, _)) -->
		[	'Calls to unknown and undefined predicates generate a runtime error.'-[], nl,
			'Misspelt predicate name? Wrong number of arguments?'-[], nl, nl
		].
	explain(unknown_non_terminal_called_but_not_defined(_, _, _, _, _)) -->
		[	'Calls to unknown and undefined grammar rules generate a runtime error.'-[], nl,
			'Misspelt grammar rule name? Wrong number of arguments?'-[], nl, nl
		].

	explain(redefined_logtalk_built_in_predicate(_, _, _, _, _)) -->
		[	'Avoid redefining Logtalk built-in predicates as it harms readability.'-[], nl,
			'Lack of awareness that a built-in predicate with that name exists?'-[], nl, nl
		].

	explain(redefined_prolog_built_in_predicate(_, _, _, _, _)) -->
		[	'Is the redefinition a consequence of making the code portable?'-[], nl,
			'Lack of awareness that a built-in predicate with that name exists?'-[], nl, nl
		].

	explain(redefined_operator(_, _, _, _, _, _)) -->
		[	'Redefining standard operators can break term parsing causing syntax'-[], nl,
			'errors or change how terms are parsed introducing bugs.'-[], nl, nl
		].

	explain(redefined_operator(_, _, _, _)) -->
		[	'Redefining standard operators can break term parsing causing syntax'-[], nl,
			'errors or change how terms are parsed introducing bugs.'-[], nl, nl
		].

	explain(goal_is_always_true(_, _, _, _, _)) -->
		['Misspelt variable in goal? Wrong operator or built-in predicate?'-[], nl, nl].

	explain(goal_is_always_false(_, _, _, _, _)) -->
		['Misspelt variable in goal? Wrong operator or built-in predicate?'-[], nl, nl].

	explain(no_matching_clause_for_predicate_goal(_, _, _, _, _)) -->
		[	'Calls to locally defined predicates without a clause with a matching head'-[], nl,
			'fail. Typo in a predicate argument? Predicate definition incomplete?'-[], nl, nl
		].
	explain(no_matching_clause_for_non_terminal_goal(_, _, _, _, _)) -->
		[	'Calls to locally defined non-terminals without a gramamr rule with'-[], nl,
			'a matching head fail. Typo in a non-terminal argument? Non-terminal'-[], nl,
			'definition incomplete?'-[], nl, nl
		].

	explain(missing_reference_to_built_in_protocol(_, _, Type, _, Protocol)) -->
		[	'The ~w implements reserved but user-defined predicates specified'-[Type], nl,
			'in the built-in protocol. Add the implements(~q) argument to'-[Protocol], nl,
			'the ~w opening directive to fix this warning.'-[Type], nl, nl
		].

	explain(duplicated_directive(_, _, _, _, _, _, _)) -->
		['Easy to fix warning: simply delete or correct the duplicated directive.'-[], nl, nl].
	explain(duplicated_directive(_, _, _, _, _)) -->
		['Easy to fix warning: simply delete or correct the duplicated directive.'-[], nl, nl].

	explain(duplicated_clause(_, _, _, _, _, _, _)) -->
		[	'Duplicated clauses are usually a source code editing error and can'-[], nl,
			'result in spurious choice-points, degrading performance. Delete or'-[], nl,
			'correct the duplicated clause to fix this warning.'-[], nl, nl
		].
	explain(duplicated_grammar_rule(_, _, _, _, _, _, _)) -->
		[	'Duplicated grammar rules are usually a source code editing error and'-[], nl,
			'can result in spurious choice-points, degrading performance. Delete'-[], nl,
			'or correct the duplicated grammar rule to fix this warning.'-[], nl, nl
		].

	explain(duplicated_predicate_reference(_, _, _, _, _, _, _)) -->
		['Easy to fix warning: simply delete the duplicated reference.'-[], nl, nl].
	explain(duplicated_non_terminal_reference(_, _, _, _, _, _, _)) -->
		['Easy to fix warning: simply delete the duplicated reference.'-[], nl, nl].

	explain(non_tail_recursive_predicate(_, _, _, _, _)) -->
		[	'Non-tail recursive predicate definitions consume space proportional'-[], nl,
			'to the number of recursive calls. A predicate definition is non-tail'-[], nl,
			'recursive when the recursive call is not the last goal in the clause'-[], nl,
			'body. This warning can be fixed by redefining the predicate, often by'-[], nl,
			'using an accumulator. This warning should be fixed when the predicate'-[], nl,
			'is deterministic.'-[], nl, nl
		].

	explain(non_tail_recursive_non_terminal(_, _, _, _, _)) -->
		[	'Non-tail recursive non-terminal definitions consume space proportional'-[], nl,
			'to the number of recursive calls. A non-terminal definition is non-tail'-[], nl,
			'recursive when the recursive call is not the last goal in the grammar'-[], nl,
			'rule body. This warning can be fixed by redefining the non-terminal,'-[], nl,
			'often by using an accumulator. This warning should be fixed when the'-[], nl,
			'non-terminal is deterministic.'-[], nl, nl
		].

	% lambda expression messages

	explain(parameter_variable_used_elsewhere(_, _, _, _, _, _)) -->
		[	'An occurrence of a lambda parameter variable before the lambda expression'-[], nl,
			'is a common source of errors. An occurrence of a lambda parameter variable'-[], nl,
			'after the lambda expression is bad programming style.'-[], nl, nl
		].

	explain(unclassified_variables_in_lambda_expression(_, _, _, _, _, _)) -->
		[	'All variables in a lambda expression should be listed as either'-[], nl,
			'lambda free variables or lambda parameters.'-[], nl, nl
		].

	explain(variables_with_dual_role_in_lambda_expression(_, _, _, _, _, _)) -->
		[	'In general, a variable in a lambda expression should not be listed as'-[], nl,
			'both a lambda free variable or a lambda parameter. Typo or intended?'-[], nl, nl
		].

	% portability messages

	explain(non_standard_predicate_call(_, _, _, _, _)) -->
		[	'Calls to non-standard built-in predicates can make the code non-portable'-[], nl,
			'when using other backend compilers. Are there portable alternatives that'-[], nl,
			'can be used?'-[], nl, nl
		].
	explain(non_standard_arithmetic_function_call(_, _, _, _, _)) -->
		[	'Calls to non-standard built-in functions can make the code non-portable'-[], nl,
			'when using other backend compilers. Are there portable alternatives that'-[], nl,
			'can be used?'-[], nl, nl
		].

	explain(non_standard_predicate_option(_, _, _, _, _, _)) -->
		[	'Use of non-standard predicate options can make the code non-portable'-[], nl,
			'when using other backend compilers. Are there portable alternatives'-[], nl,
			'that can be used?'-[], nl, nl
		].

	explain(non_standard_prolog_flag(_, _, _, _, _)) -->
		[	'Use of non-standard Prolog flags can make the code non-portable when using'-[], nl,
			'other backend compilers. Are there portable alternatives that can be used?'-[], nl, nl
		].
	explain(non_standard_prolog_flag(_, _, _)) -->
		[	'Use of non-standard Prolog flags can make the code non-portable when using'-[], nl,
			'other backend compilers. Are there portable alternatives that can be used?'-[], nl, nl
		].
	explain(non_standard_prolog_flag_value(_, _, _, _, _, _)) -->
		[	'Although the flag itself is standard, the use of a non-standard flag value'-[], nl,
			'may make your code non-portable when using other backend compilers.'-[], nl,
			'Are there portable alternatives that you can use instead?'-[], nl, nl
		].
	explain(non_standard_prolog_flag_value(_, _, _, _)) -->
		[	'Although the flag itself is standard, the use of a non-standard flag value'-[], nl,
			'may make your code non-portable when using other backend compilers.'-[], nl,
			'Are there portable alternatives that you can use instead?'-[], nl, nl
		].

	explain(non_standard_file_directive(_, _, _)) -->
		[	'Use of non-standard file directives may make your code non-portable when using'-[], nl,
			'other backend compilers. Are there portable alternatives that you can use instead?'-[], nl, nl
		].

	explain(logtalk_built_in_predicate_as_directive(_, _, Directive)) -->
		[	'Use of built-in predicates as directives may make your code non-portable when using'-[], nl,
			'other backend compilers. Use instead a standard initialization/1 directive to call'-[], nl,
			'those predicates: ":- initialization(~q)."'-[Directive], nl, nl
		].

	explain(top_level_shortcut_as_directive(_, _, _)) -->
		[	'Top-level shortcuts should only be used at the top-level interpreter.'-[], nl,
			'Use instead a supported directive or a built-in predicate called from'-[], nl,
			'an initialization/1 directive to fix this warning.'-[], nl, nl
		].

	explain(compiling_proprietary_prolog_directive(_, _, _, _, _)) -->
		[	'Use of non-standard Prolog directives may make your code non-portable when using'-[], nl,
			'other backend compilers. Are there portable alternatives that you can use instead?'-[], nl, nl
		].

	explain(compiling_query_as_initialization_goal(_, _, _, _, _)) -->
		[	'Arbitrary queries used as directives are converted to initialization/1 directives.'-[], nl,
			'Note that, as a consequence, the queries are only called after the file being loaded.'-[], nl, nl
		].

	% Prolog dialect specific term- and goal-expansion messages

	explain(prolog_dialect_goal_expansion(_, _, _, _, _, _)) -->
		[	'Relying on Prolog dialect specific term- and goal-expansions improves integration'-[], nl,
			'with the backend Prolog system but usually at the expense of portability.'-[], nl, nl
		].
	explain(prolog_dialect_goal_expansion(_, _, _, _)) -->
		[	'Relying on Prolog dialect specific term- and goal-expansions improves integration'-[], nl,
			'with the backend Prolog system but usually at the expense of portability.'-[], nl, nl
		].
	explain(prolog_dialect_term_expansion(_, _, _, _, _, _)) -->
		[	'Relying on Prolog dialect specific term- and goal-expansions improves integration'-[], nl,
			'with the backend Prolog system but usually at the expense of portability.'-[], nl, nl
		].
	explain(prolog_dialect_term_expansion(_, _, _, _)) -->
		[	'Relying on Prolog dialect specific term- and goal-expansions improves integration'-[], nl,
			'with the backend Prolog system but usually at the expense of portability.'-[], nl, nl
		].

	explain(term_expansion_error(_, _, _, _, _, _, _)) -->
		[	'Errors during term-expansion can be caused by a bug in the term-expansion code'-[], nl,
			'or by a misunderstanding of the syntax of the terms expected to be expanded.'-[], nl, nl
		].
	explain(term_expansion_error(_, _, _, _, _)) -->
		[	'Errors during term-expansion can be caused by a bug in the term-expansion code'-[], nl,
			'or by a misunderstanding of the format of the terms expected to be expanded.'-[], nl, nl
		].
	explain(goal_expansion_error(_, _, _, _, _, _, _)) -->
		[	'Errors during goal-expansion can be caused by a bug in the goal-expansion code'-[], nl,
			'or by a misunderstanding of the format of the goals expected to be expanded.'-[], nl, nl
		].
	explain(goal_expansion_error(_, _, _, _, _)) -->
		[	'Errors during goal-expansion can be caused by a bug in the goal-expansion code'-[], nl,
			'or by a misunderstanding of the format of the goals expected to be expanded.'-[], nl, nl
		].

	% unknown entity messages

	explain(reference_to_unknown_object(_, _, _, _, _)) -->
		[	'Misspelt object name? Wrong file loading order? Circular references?'-[], nl,
			'Object defined later in the same source file? If only a loading order'-[], nl,
			'issue, the only consequence should be only a small performance penalty'-[], nl,
			'as some static binding optimizations may not be possible. Otherwise,'-[], nl,
			'references to unknown objects can result in runtime errors.'-[], nl, nl
		].

	explain(reference_to_unknown_protocol(_, _, _, _, _)) -->
		[	'Misspelt protocol name? Wrong file loading order? Protocol defined'-[], nl,
			'later in the same source file? If only a loading order issue, the'-[], nl,
			'only consequence should be only a small performance penalty as some'-[], nl,
			'some static binding optimizations may not be possible. Otherwise,'-[], nl,
			'references to unknown protocols can result in runtime errors.'-[], nl, nl
		].

	explain(reference_to_unknown_category(_, _, _, _, _)) -->
		[	'Misspelt category name? Wrong file loading order? Category defined'-[], nl,
			'later in the same source file? If only a loading order issue, the'-[], nl,
			'only consequence should be only a small performance penalty as some'-[], nl,
			'static binding optimizations may not be possible. Otherwise,'-[], nl,
			'references to unknown categories can result in runtime errors.'-[], nl, nl
		].

	explain(reference_to_unknown_module(_, _, _, _, _)) -->
		['Misspelt module name? Wrong file loading order? Circular references?'-[], nl, nl].

	explain(module_used_as_object(_, _, _, _, _)) -->
		['This is also portability issue as not all backends support a module system.'-[], nl, nl].

	explain(missing_predicate_directive(_, _, Type, _, (dynamic)/1, Name/Arity)) -->
		[	'The ~w updates the ~q predicate but does not declare it dynamic.'-[Type, Name/Arity], nl,
			'Add a local ":- dynamic(~q)." directive to suppress this warning.'-[Name/Arity], nl, nl
		].
	explain(missing_predicate_directive(_, _, Type, _, (dynamic)/1, Name//Arity)) -->
		[	'The ~w updates the ~q non-terminal but does not declare it dynamic.'-[Type, Name//Arity], nl,
			'Add a local ":- dynamic(~q)." directive to suppress this warning.'-[Name//Arity], nl, nl
		].
	explain(missing_predicate_directive(_, _, Type, _, (discontiguous)/1, Name/Arity)) -->
		[	'The ~w ~q predicate clauses are discontiguous.'-[Type, Name/Arity], nl,
			'If there are no typos in a clause head causing this warning, add a local'-[], nl,
			'":- discontiguous(~q)." directive to suppress this warning.'-[Name/Arity], nl, nl
		].
	explain(missing_predicate_directive(_, _, Type, _, (discontiguous)/1, Name//Arity)) -->
		[	'The ~w ~q non-terminal rules are discontiguous.'-[Type, Name/Arity], nl,
			'If there are no typos in a rule head causing this warning, add a local'-[], nl,
			'":- discontiguous(~q)." directive to suppress this warning.'-[Name/Arity], nl, nl
		].
	explain(missing_predicate_directive(_, _, Type, _, (multifile)/1, Predicate)) -->
		[	'Clauses and grammar rules defined in multiple entities and/or source'-[], nl,
			'files must be declared as multifile. Add to the ~w a local'-[Type], nl,
			'":- multifile(~q)." directive to suppress this warning.'-[Predicate], nl, nl
		].
	explain(missing_predicate_directive(_, _, Type, _, (meta_predicate)/1, Predicate)) -->
		[	'The ~w ~q predicate has one or more meta-arguments.'-[Type, Predicate], nl,
			'Add a local meta_predicate/1 directive declaring those meta-arguments'-[], nl,
			'to suppress this warning and ensure that meta-calls occur in the correct'-[], nl,
			'context.'-[], nl, nl
		].
	explain(missing_predicate_directive(_, _, Type, _, (meta_non_terminal)/1, NonTerminal)) -->
		[	'The ~w ~q non-terminal has one or more meta-arguments.'-[Type, NonTerminal], nl,
			'Add a local meta_non_terminal/1 directive declaring those meta-arguments'-[], nl,
			'to suppress this warning and ensure that meta-calls occur in the correct'-[], nl,
			'context.'-[], nl, nl
		].
	explain(missing_predicate_directive(_, _, _, _, Directive, Predicate)) -->
		['Add a ":- ~q(~q)." directive to suppress this warning.'-[Directive, Predicate], nl, nl].
	explain(missing_predicate_directive(_, _, _, _, Directive)) -->
		['Add a  "~q." directive to suppress this warning.'-[Directive], nl, nl
		].

	explain(missing_scope_directive(_, _, _, _, Directive, _/_)) -->
		[	'But there is a ~w directive for the predicate. If there is a'-[Directive], nl,
			'scope directive, check for a typo in the predicate name or number of'-[], nl,
			'arguments.'-[], nl, nl
		].
	explain(missing_scope_directive(_, _, _, _, Directive, _//_)) -->
		[	'But there is a ~w directive for the non-terminal. If there is a'-[Directive], nl,
			'scope directive, check for a typo in the non-terminal name or number of'-[], nl,
			'arguments.'-[], nl, nl
		].

	explain(missing_function(_, _, _, _, _)) -->
		['Check for a typo in the function name or number of arguments.'-[], nl, nl].

	% disjunction guidelines messages

	explain(disjunction_as_body(_, _, _, _, _, _)) -->
		[	'Coding guidelines advise using separate clauses instead of a single'-[], nl,
			'clause with a disjunction as body for improved code readability.'-[], nl, nl
		].

	% suspicious cuts

	explain(suspicious_cut_in_if_then_else(_, _, _, _, _)) -->
		[	'A cut in the if part of an if-then-else control construct is local to the if'-[], nl,
			'part. It can thus be replaced by a call to the true/0 control construct. But'-[], nl,
			'that means that the then part will always be called.'-[], nl, nl
		].

	explain(suspicious_cut_in_if_then_else(_, _, _, _, _, _)) -->
		[	'A cut in the if part of an if-then-else control construct is local to the if'-[], nl,
			'part. If the cut is meant to commit to the clause, add parenthesis around the'-[], nl,
			'if-then-else control construct before the cut to fix its scope.'-[], nl, nl
		].

	explain(suspicious_cut_in_soft_cut(_, _, _, _, _)) -->
		[	'A cut in the if part of a soft-cut control construct is local to the if'-[], nl,
			'part. It can thus be replaced by a call to the true/0 control construct.'-[], nl,
			'But that means that the then part will always be called and that there'-[], nl,
			'are no choice-points in the if part to warrant using a soft-cut.'-[], nl, nl
		].

	explain(suspicious_cut_in_soft_cut(_, _, _, _, _, _)) -->
		[	'A cut in the if part of a soft-cut control construct is local to the if'-[], nl,
			'part. If the cut is meant to commit to the clause, add parenthesis around'-[], nl,
			'the soft-cut control construct before the cut to fix its scope.'-[], nl, nl
		].

	explain(suspicious_cut_in_disjunction(_, _, _, _, _)) -->
		[	'A cut as the first argument of a disjunction control construct prevents'-[], nl,
			'backtracking into the second argument, which will never be called.'-[], nl, nl
		].

	explain(suspicious_cut_in_disjunction(_, _, _, _, _, _)) -->
		[	'A cut at the start of the first argument of a disjunction control construct'-[], nl,
			'prevents backtracking into the second argument. If the cut is meant to commit'-[], nl,
			'to the clause, add parenthesis around the disjunction control construct before'-[], nl,
			'the cut to fix its scope.'-[], nl, nl
		].

	% suspicious tests in if-then-else and soft-cut control constructs

	explain(suspicious_if_then_else_test(_, _, _, _, _, _)) -->
		[	'An if-then-else test that is a unification goal between a variable and'-[], nl,
			'a ground term is a potential bug: if the variable is unbound at runtime,'-[], nl,
			'the test will always succeed, committing to the then part. Consider using'-[], nl,
			'instead the (==)/2 or (=:=)/2 built-in predicates.'-[], nl, nl
		].

	explain(suspicious_soft_cut_test(_, _, _, _, _, _)) -->
		[	'A soft-cut test that is a unification goal between a variable and a ground'-[], nl,
			'term is a potential bug: if the variable is unbound at runtime, the test'-[], nl,
			'will always succeed, committing to the then part. Consider using instead'-[], nl,
			'the (==)/2 or (=:=)/2 built-in predicates.'-[], nl, nl
		].

	% other conditional warnings

	explain(missing_else_part(_, _, _, _, _)) -->
		[	'If-then-else (and soft-cut) control constructs without the else part are'-[], nl,
			'false when the condition is false; adding the missing else part makes this'-[], nl,
			'semantics clear.'-[], nl, nl
		].

	% catch/3 goals that catch all exceptions

	explain(catchall_catch(_, _, _, _, _)) -->
		[	'Catching all exceptions may cause critical errors to be ignored (e.g.'-[], nl,
			'resource errors). If possible, catch only the expected exception. When'-[], nl,
			'multiple exceptions are expected, filter them using the recovery goal.'-[], nl, nl
		].

	% naming guidelines messages

	explain(camel_case_entity_name(_, _, _, _)) -->
		[	'Coding guidelines advise the use of underscores'-[], nl,
			'to separate words in entity names.'-[], nl, nl
		].

	explain(entity_name_with_digits_in_the_middle(_, _, _, _)) -->
		[	'Coding guidelines advise against using names with'-[], nl,
			'digits in the middle as these may hurt readability.'-[], nl, nl
		].

	explain(camel_case_predicate_name(_, _, _, _, _)) -->
		[	'Coding guidelines advise the use of underscores'-[], nl,
			'to separate words in predicate names.'-[], nl, nl
		].

	explain(camel_case_non_terminal_name(_, _, _, _, _)) -->
		[	'Coding guidelines advise the use of underscores'-[], nl,
			'to separate words in non-terminal names.'-[], nl, nl
		].

	explain(predicate_name_with_digits_in_the_middle(_, _, _, _, _)) -->
		[	'Coding guidelines advise against using names with'-[], nl,
			'digits in the middle as these may hurt readability.'-[], nl, nl
		].

	explain(non_terminal_name_with_digits_in_the_middle(_, _, _, _, _)) -->
		[	'Coding guidelines advise against using names with'-[], nl,
			'digits in the middle as these may hurt readability.'-[], nl, nl
		].

	explain(non_camel_case_variable_name(_, _, _, _, _)) -->
		[	'Coding guidelines advise the use of CamelCase'-[], nl,
			'in variable names with multiple words.'-[], nl, nl
		].

	explain(non_camel_case_variable_name(_, _, _)) -->
		[	'Coding guidelines advise the use of CamelCase'-[], nl,
			'in variable names with multiple words.'-[], nl, nl
		].

	explain(variable_name_with_digits_in_the_middle(_, _, _, _, _)) -->
		[	'Coding guidelines advise against using names with'-[], nl,
			'digits in the middle as these may hurt readability.'-[], nl, nl
		].

	explain(variable_name_with_digits_in_the_middle(_, _, _)) -->
		[	'Coding guidelines advise against using names with'-[], nl,
			'digits in the middle as these may hurt readability.'-[], nl, nl
		].

	explain(variable_names_differ_only_on_case(_, _, _, _, _, _)) -->
		[	'Variable names differing only on case hurt code readability. Consider'-[], nl,
			'renaming one or both variables to better clarify their meaning.'-[], nl, nl
		].

	explain(variable_names_differ_only_on_case(_, _, _, _)) -->
		['Variables differing only on case hurt code readability.'-[], nl, nl].

	explain(entity_parameter_not_in_parameter_variable_syntax(_, _, _, _, _)) -->
		[	'Parameter variable syntax is an underscore followed by a name in title'-[], nl,
			'case followed by an underscore. This syntax allows the parameter to be'-[], nl,
			'referenced by name in the parametric entity directives and clauses.'-[], nl, nl
		].

	% singleton variable messages

	explain(singleton_variables(_, _, _, _, [_], _)) -->
		[	'Misspelt variable name? Don''t care variable?'-[], nl,
			'In the later case, use instead an anonymous variable.'-[], nl, nl
		].
	explain(singleton_variables(_, _, _, _, [_, _| _], _)) -->
		[	'Misspelt variable names? Don''t care variables?'-[], nl,
			'In the later case, use instead anonymous variables.'-[], nl, nl
		].

	explain(singleton_variables(_, _, [_], _)) -->
		[	'Misspelt variable name? Don''t care variable?'-[], nl,
			'In the later case, use instead an anonymous variable.'-[], nl, nl
		].
	explain(singleton_variables(_, _, [_, _| _], _)) -->
		[	'Misspelt variable names? Don''t care variables?'-[], nl,
			'In the later case, use instead anonymous variables.'-[], nl, nl
		].

	% suspicious call messages

	explain(suspicious_call(_, _, _, _, (_ -> _), reason(missing_else_part))) -->
		[	'Using the ->/2 if-then control construct without an else part is'-[], nl,
			'a common source of errors. Check if the else part is missing due'-[], nl,
			'to a coding error or use the (If -> Then; fail) pattern instead.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, '*->'(_, _), reason(missing_else_part))) -->
		[	'Using the *->/2 soft cut control construct without an else part is'-[], nl,
			'a common source of errors. Check if the else part is missing due'-[], nl,
			'to a coding error or use the (If *-> Then; fail) pattern instead.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, Var, [call(Var)])) -->
		[	'Naked meta-variables in control constructs that are cut-transparent'-[], nl,
			'may have different semantics across Prolog systems. Use instead the'-[], nl,
			'suggested alternative to avoid portability issues.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _ =.. _, _)) -->
		[	'The standard Prolog =../2 built-in predicate is costly and should be'-[], nl,
			'avoided whenever possible. Consider using the suggested alternative.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, atom_concat(_, _, _), _)) -->
		[	'Avoid implicit creation of atoms that will not be used as atom management'-[], nl,
			'is relatively costly. Consider using the suggested alternative.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, findall(_, Goal, _), [(Goal, fail; true)])) -->
		[	'Avoid using a costly findall/3 goal (due to its copy semantics and list'-[], nl,
			'creation) to enumerate all solutions of a goal without collecting them.'-[],  nl,
			'Consider using the suggested alternative.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, Object::_, _)) -->
		{Object == user},
		[	'Prolog standard built-in predicates can be called directly without'-[], nl,
			'requiring sending a message to the "user" pseudo-object.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _::Pred, [Pred])) -->
		[	'Using message-sending to call a local predicate is usually only required'-[], nl,
			'when we want to generate an event for the message. Otherwise, simply call'-[], nl,
			'the predicate directly.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _::Pred, [::Pred])) -->
		[	'Using an object explicit message-sending instead of a message to'-[], nl,
			'"self" is usually only required when we want to generate an event'-[], nl,
			'for the message. Otherwise, simply send a message to "self".'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, ::Pred, [Pred, ^^Pred])) -->
		[	'Sending a message to self to call the same predicate being defined is'-[], nl,
			'a redundant and costly alternative to simply call the local definition.'-[], nl,
			'Or is your intention to make a "super" call?'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _, [_])) -->
		['Use the suggested alternative to suppress this warning.'-[], nl, nl].
	explain(suspicious_call(_, _, _, _, _, [_, _| _])) -->
		['Use one of the suggested alternatives to suppress this warning.'-[], nl, nl].

	explain(suspicious_call(_, _, _, _, repeat, reason(repeat(_)))) -->
		[	'A repeat loop not ended with a cut may result in an endless loop in'-[], nl,
			'case of unexpected backtracking. Use a cut immediately after the test'-[], nl,
			'goal that exits the repeat loop unless you really want to define a'-[], nl,
			'perpetual loop.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, !, reason(multifile(_)))) -->
		[	'A cut in a multifile predicate clause may prevent other clauses,'-[], nl,
			'notably those defined elsewhere, from being used when calling the'-[], nl,
			'predicate or when backtracking into a call to the predicate.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _ is _, reason(shared_variable(_)))) -->
		[	'A variable occurring in both arguments of an is/2 goal will likely result'-[], nl,
			'in a runtime failure. Logical variables can only be further instantiated,'-[], nl,
			'not unified with a different term when already bound. If an arithmetic'-[], nl,
			'comparison is intended, consider using instead the (=:=)/2 operator.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, forall(_, _), reason(no_shared_variables(forall)))) -->
		[	'In most cases, the forall/2 predicate is used for implementing'-[], nl,
			'generate-and-test loops where the first argument generates solutions'-[], nl,
			'that are verified by the second argument. This warning can likely be'-[], nl,
			'ignored if the first argument simply implements a counter.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _, reason(no_shared_variables(_)))) -->
		[	'In most cases, the template argument shares variables with the'-[], nl,
			'goal argument so that we collect template bindings for the goal'-[], nl,
			'solutions. This warning can likely be ignored if you are using'-[], nl,
			'this meta-predicate to simply help count solutions.'-[], nl, nl
		].

	explain(suspicious_call(_, _, _, _, _, reason(cyclic_terms))) -->
		[	'Is the creation of a cyclic term intended or just a bug?'-[], nl,
			'Is there a typo in a variable name in one of terms ?'-[], nl, nl
		].

	explain(suspicious_call(_, _, _, _, _, reason(no_variable_bindings_after_unification))) -->
		[	'Unification goals that do not bind any variables are redundant.'-[], nl,
			'Typo in one of the unification goal arguments?'-[], nl, nl
		].

	explain(suspicious_call(_, _, _, _, _, reason(existential_variables([_], _)))) -->
		[	'A existentially-qualified variable must exist in the qualified goal.'-[], nl,
			'Typo in the variable name?'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _, reason(existential_variables([_, _| _], _)))) -->
		[	'Existentially-qualified variables must exist in the qualified goal.'-[], nl,
			'Typos in the variable names?'-[], nl, nl
		].

	explain(suspicious_call(_, _, _, _, _, reason(singleton_variables(_, _, _)))) -->
		[	'Singleton variables in the goal argument of bagof/3 and setof/3 calls'-[], nl,
			'that are not existentially-qualified can result in multiple solutions.'-[], nl,
			'If you want a single solution, use the ^/2 operator to existentially'-[], nl,
			'qualify the variables. If you want multiple solutions, use a lambda'-[], nl,
			'expression with the relevant variables listed as lambda free variables.'-[], nl, nl
		].

	explain(suspicious_call(_, _, _, _, _, reason(comparing_numbers_using_unification))) -->
		[	'Comparing numbers using unification can fail in cases where using instead'-[], nl,
			'the standard (=:=)/2 or (=\\=)/2 arithmetic comparison built-in predicates'-[], nl,
			'would succeed.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _, reason(float_comparison))) -->
		[	'Comparing floats is problematic as it can fail due to rounding errors and'-[], nl,
			'loss of precision when converting from decimal to binary representation.'-[], nl,
			'Check instead if the float values are within a given epsilon. The "float"'-[], nl,
			'library object provides several predicates for comparing floats.'-[], nl, nl
		].

	% encoding/1 directive messages

	explain(ignored_encoding_directive(_, _)) -->
		[	'The encoding/1 directive must be the first term, in the first line, in a'-[], nl,
			'file. Moreover, only the first found encoding/1 directive is processed.'-[], nl, nl
		].

	explain(misplaced_encoding_directive(_, _)) -->
		['The encoding/1 directive must be the first term, in the first line, in a file.'-[], nl, nl].

	% unsupported directive messages

	explain(unsupported_directive(_, _, _, _, _)) -->
		[	'This directive requires features not provided by the current backend.'-[], nl,
			'Trying to compile code requiring a different backend? Trying to port'-[], nl,
			'Prolog code? If the latter, consult the Handbook for porting advice.'-[], nl, nl
		].

	explain(unsupported_directive(_, _, _)) -->
		[	'This directive requires features not provided by the current backend.'-[], nl,
			'Trying to compile code requiring a different backend? Trying to port'-[], nl,
			'Prolog code? If the latter, consult the Handbook for porting advice.'-[], nl, nl
		].

	% steadfastness messages

	explain(possible_non_steadfast_predicate(_, _, _, _, _)) -->
		[	'Variable aliasing in a clause head means that two or more head'-[], nl,
			'arguments share variables. If the predicate is called with usually'-[], nl,
			'output arguments bound, a premature cut may result in the wrong'-[], nl,
			'clause being used. If that''s the case, change the clause to perform'-[], nl,
			'output unifications after the cut.'-[], nl, nl
		].
	explain(possible_non_steadfast_non_terminal(_, _, _, _, _)) -->
		[	'Variable aliasing in a grammar rule head means that two or more head'-[], nl,
			'arguments share variables. If the grammar rule is called with usually'-[], nl,
			'output arguments bound, a premature cut may result in the wrong grammar'-[], nl,
			'rule being used. If that''s the case, change the grammar rule to perform'-[], nl,
			'output unifications after the cut.'-[], nl, nl
		].

	% make tool messages

	explain(duplicated_library_aliases(_)) -->
		[	'Duplicated library aliases are a potential source of issues as an application'-[], nl,
			'loading a library may load a wrong version. Even when the duplicated aliases'-[], nl,
			'point to the same version of a library in different locations, the duplication'-[], nl,
			'may result in mishandling library deletions and updates. You should find and'-[], nl,
			'delete the duplicated aliases.'-[], nl, nl
		].

	explain(library_paths_dont_end_with_slash(_)) -->
		[	'Library paths are expected to always end with a slash for uniform handling.'-[], nl,
			'You should fix all library aliases to ensure that all paths end with a slash.'-[], nl, nl
		].

	explain(circular_references(_)) -->
		[	'Circular references prevent some code optimizations to be applied by the'-[], nl,
			'compiler (e.g., static binding). Try to avoid them whenever possible, e.g.'-[], nl,
			'by refactoring your code.'-[], nl, nl
		].

	% lgtunit tool messages

	explain(no_code_coverage_for_protocols(_, _, _, _, _)) -->
		[	'Protocols cannot contain predicate definitions, only predicate declarations.'-[], nl,
			'Thus, declaring a protocol as covered is pointless. Fix this warning by'-[], nl,
			'simply removing the corresponding cover/1 clause from the tests object.'-[], nl, nl
		].

	explain(unknown_entity_declared_covered(_, _, _, _, _, _)) -->
		[	'Likely there is a typo in the corresponding cover/1 clause in the tests'-[], nl,
			'object. You will need to find and fix this typo to fix the warning.'-[], nl, nl
		].

	explain(assertion_uses_unification(_, _, _, _, _, _, _)) -->
		[	'Using unification in an assertion will fail to catch cases where a test'-[], nl,
			'query fails to bind (or further instantiate) an output argument as the'-[], nl,
			'unification will trivially succeed. Consider using instead (==)/2,'-[], nl,
			'variant/2, or subsumes_term/2 (or the corresponding test outcomes) for'-[], nl,
			'more strict and reliable test success checking. For float approximate'-[], nl,
			'comparison, use the (=~=)/2 predicate provided by lgtunit.', nl, nl
		].

	explain(tests_run_differ_from_tests_total(_, _)) -->
		[	'A number of tests run different from the total number of defined tests'-[], nl,
			'usually implies bugs in the implementation of basic Prolog control'-[], nl,
			'constructs by the used backend system.'-[], nl, nl
		].

	% packs tool messages

	explain(reset_failed(_)) -->
		[	'The common cause of failure when resetting the registries and packs directory'-[], nl,
			'structure are file and directory permissions errors. You may need to use a'-[], nl,
			'terminal application to diagnose those errors and delete the directory. After,'-[], nl,
			're-run the packs::reset message to ensure that the default directory structure'-[], nl,
			'is created.'-[], nl, nl
		].

	explain(cannot_update_pinned_registry(_)) -->
		[	'Pinning a registry is good practice for ensuring that applications dependent'-[], nl,
			'on the registry packs will not be broken by updating the registry and, by'-[], nl,
			'doing that, updating pack manifest files for packs used by the applications.'-[], nl,
			'To force updating, unpin the registry using the registries::unpin(Registry)'-[], nl,
			'message or use the registries::update(Registry, Options) message with the'-[], nl,
			'force(true) option.'-[], nl, nl
		].

	explain(cannot_delete_pinned_registry(_)) -->
		[	'Pinning a registry is good practice for ensuring that applications dependent'-[], nl,
			'on the registry packs will not become dependent on packs that can no longer'-[], nl,
			'be reinstalled/updated/reverted due to the deletion of their manifest files.'-[], nl,
			'To force deletion, unpin the registry using the registries::unpin(Registry)'-[], nl,
			'message or use the registries::delete(Registry, Options) message with the'-[], nl,
			'force(true) option.'-[], nl, nl
		].

	explain(cannot_delete_registry_with_installed_packs(_)) -->
		[	'Deleting a registry with installed packs will orphan the packs, preventing'-[], nl,
			'reinstalling/updating/reverting them, potentially breaking installing any'-[], nl,
			'applications depending on those packs. To force deletion, use the message'-[], nl,
			'registries::delete(Registry, Options) with the force(true) option.'-[], nl, nl
		].

	explain(pack_already_installed_from_different_registry(_, _, _)) -->
		[	'Although different registries can define packs with the same name, only one'-[], nl,
			'of the packs with a conflicting name can be installed at the same time'-[], nl,
			'Consider using application specific pack virtual environments to avoid'-[], nl,
			'these conflicts.'-[], nl, nl
		].

	explain(unsupported_archive_format(_)) -->
		[	'The supported archive formats is limited to ensure portability. Consult the'-[], nl,
			'packs tool documentation for a list of all the supported formats.'-[], nl, nl
		].

	explain(cannot_uninstall_pinned_pack(_)) -->
		[	'Pinning packs is a common practice for ensuring that dependent applications'-[], nl,
			'use explicit and verified pack versions. This practice protects applications'-[], nl,
			'from breaking when pack dependencies are inadvertently uninstalled. To force'-[], nl,
			'uninstallation, either unpin the pack using the packs::unpin(Pack) message or'-[], nl,
			'use the packs::uninstall(Pack, Options) message with the force(true) option.'-[], nl, nl
		].

	explain(cannot_update_pinned_pack(_)) -->
		[	'Pinning packs is a common practice for ensuring that dependent applications'-[], nl,
			'use explicit and verified pack versions. This practice protects applications'-[], nl,
			'from breaking when unverified pack updates are installed. To force the pack'-[], nl,
			'update, either unpin the pack using the packs::unpin(Pack) message or use the'-[], nl,
			'the packs::update(Pack, Options) message with the force(true) option.'-[], nl, nl
		].

	explain(pack_archive_copy_failed(_, _)) -->
		[	'Pack archive copy failures usually result directory or file permissions.'-[], nl,
			'Check those permissions. In rare cases, there may be a pack manifest error'-[], nl,
			'with a wrong path for the archive. you can use the packs::describe(Pack)'-[], nl,
			'message to list the pack home and archive paths.'-[], nl, nl
		].

	explain(pack_archive_download_failed(_, _)) -->
		[	'Pack archive download failures usually result from network connectivity'-[], nl,
			'issues. Check if you can reach the pack provider server. In rare cases,'-[], nl,
			'there may be a pack manifest error with a wrong URL for the archive.'-[], nl,
			'You can use the packs::describe(Pack) message to list the pack home and'-[], nl,
			'archive URLs.'-[], nl, nl
		].

	explain(pack_archive_checksum_failed(_, _)) -->
		[	'Pack archive checksum failures usually result from incomplete or failed'-[], nl,
			'downloads. In rare cases, there may be a pack manifest error with a'-[], nl,
			'wrong checksum for the archive. Failures may be caused by authentication'-[], nl,
			'errors. Inspecting the contents of the archive may allow you to diagnose'-[], nl,
			'the problem. You can use the packs::clean(Pack) message to delete the'-[], nl,
			'archive file and try again to install the pack.'-[], nl, nl
		].

	explain(pack_signature_download_failed(_, _)) -->
		[	'Pack archive signature download failures usually result from network.'-[], nl,
			'connectivity issues. Check if you can reach the pack provider server.'-[], nl,
			'In rare cases, the signature file may be missing upstream. You can use the'-[], nl,
			'packs::describe(Pack) message to list the pack home and archive URLs.'-[], nl, nl
		].

	explain(pack_archive_checksig_failed(_, _)) -->
		[	'Pack archive signature failures may be evidence of malicious tampering with'-[], nl,
			'the archive. In rare cases, there may be a pack manifest error with a wrong'-[], nl,
			'signature for the archive. It is advisable to contact the pack provider for'-[], nl,
			'diagnosing the problem.'-[], nl, nl
		].

	explain(pack_archive_decrypt_failed(_, _)) -->
		[	'Pack archive decrypt failures may be caused by passphrase typos or missing'-[], nl,
			'missing decryption keys, assuming no tampering of the archive. Using the'-[], nl,
			'verbose(true) option may provide details on the failure. It is advisable to'-[], nl,
			'contact the pack provider for diagnosing the problem.'-[], nl, nl
		].

	explain(pack_archive_uncompress_failed(_, _)) -->
		[	'Pack archive uncompressing failures may be caused by a mismatch between the'-[], nl,
			'archive format and its file name extension. It is advisable to contact the'-[], nl,
			'pack provider for diagnosing the problem.'-[], nl, nl
		].

	explain(os_dependency_not_available(_)) -->
		[	'The pack will most likely be broken if installed due to the current'-[], nl,
			'operating-system version not being the version required.'-[], nl, nl
		].

	explain(logtalk_dependency_not_available(_)) -->
		[	'The pack will most likely be broken if installed due to the current'-[], nl,
			'Logtalk version not being the version required. Either update to the'-[], nl,
			'required Logtalk version or use a virtual environment with Logtalk'-[], nl,
			'instaled as a pack at the required version.'-[], nl, nl
		].

	explain(backend_dependency_not_available(_)) -->
		[	'The pack is not compatible wuth the current backend. You will need'-[], nl,
			'to run Logtalk with the required backend to use this pack.'-[], nl, nl
		].

	explain(pack_dependency_not_available(_, _, _, _, _)) -->
		[	'The pack will most likely be broken if installed due to the required'-[], nl,
			'dependency not being available.'-[], nl, nl
		].

	explain(pack_dependency_not_available(_, _)) -->
		[	'The pack will most likely be broken if installed due to the required'-[], nl,
			'dependency not being available.'-[], nl, nl
		].

	explain(pack_dependency_not_available(_)) -->
		[	'The pack will most likely be broken if installed due to the required'-[], nl,
			'dependency not being available.'-[], nl, nl
		].

	explain(no_pack_version_compatible_with_os_version(_, _, _)) -->
		[	'You will need to either install a different pack version or use a'-[], nl,
			'different operating-system version. The pack dependencies can be'-[], nl,
			'listed using the packs::describe/1 predicate.'-[], nl, nl
		].

	explain(no_pack_version_compatible_with_logtalk_version(_)) -->
		[	'You will need to either install a different pack version or install a'-[], nl,
			'different Logtalk version. The pack dependencies can be listed using'-[], nl,
			'the packs::describe/1 predicate.'-[], nl, nl
		].

	explain(no_pack_version_compatible_with_backend_version(_, _)) -->
		[	'You will need to either install a different pack version or install a'-[], nl,
			'different backend version. The pack dependencies can be listed using'-[], nl,
			'the packs::describe/1 predicate.'-[], nl, nl
		].

	explain(updating_pack_would_break_installed_pack(_, _, _)) -->
		[	'Consider using application specific pack virtual environments to avoid'-[], nl,
			'these conflicts.'-[], nl, nl
		].

	explain(updating_pack_breaks_installed_pack(_, _, _)) -->
		[	'Consider using application specific pack virtual environments to avoid'-[], nl,
			'these conflicts.'-[], nl, nl
		].

:- end_object.
