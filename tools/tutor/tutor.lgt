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


:- object(tutor).

	:- info([
		version is 0.24,
		author is 'Paulo Moura',
		date is 2019/09/09,
		comment is 'This object adds explanations and suggestions to selected compiler warning and error messages.',
		remarks is [
			'Usage' - 'Simply load this object at startup using the goal ``logtalk_load(tutor(loader))``.'
		]
	]).

	% intercept all compiler warning and error messages

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, Kind, core, Tokens) :-
		logtalk::message_prefix_stream(Kind, core, Prefix, Stream),
		phrase(explain(Message), ExplanationTokens),
		% avoid empty line between the compiler message and its explanation
		list::append(Tokens0, [nl], Tokens),
		% add begin/2 and end/1 tokens to enable message coloring
		% if supported by the backend Prolog compiler
		list::append([begin(Kind,Ctx)| Tokens0], ExplanationTokens, ExtendedTokens0),
		list::append(ExtendedTokens0, [end(Ctx)], ExtendedTokens),
		logtalk::print_message_tokens(Stream, Prefix, ExtendedTokens).

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
		['Aliases are specified using the as/2 infix operator: Original as Alias.'-[], nl, nl].

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

	error(domain_error(compiler_flag, _)) -->
		[	'Compiler flags syntax is a compound term where the name is'-[], nl,
			'the flag name and the single argument is the flag value.'-[], nl, nl
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
		['The argument of the message delegation control construct must be a ::/2 message sending goal.'-[], nl, nl].

	error(domain_error(meta_directive_template, _)) -->
		['Likely one on the meta-argument specifiers is not valid.'-[], nl, nl].
	error(domain_error(meta_argument_specifier, _)) -->
		[	'One (or more) of the meta-argument specifiers in the meta-predicate'-[], nl,
			'(or meta-non-terminal) template is ambiguous. A typical case is the'-[], nl,
			'module-aware specifier, ":", which can mean a goal, a closure, or a'-[], nl,
			'non-callable but yet module-aware argument.'-[], nl, nl
		].

	error(domain_error([1,_], _)) -->
		['The parameter is outside the valid range. Typo?'-[], nl, nl].
	error(domain_error({_}, _)) -->
		['Inconsistent number of arguments. The numbers are expected to be equal. Typo?'-[], nl, nl].

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
		['This predicate is already listed in a uses/2 directive.'-[], nl, nl].
	error(permission_error(modify, predicate_scope, _)) -->
		[	'This predicate is already declared in a previous scope directive.'-[], nl,
			'There can be only one scope directive per predicate.'-[], nl, nl
		].
	error(permission_error(modify, meta_predicate_template, _)) -->
		[	'A meta-predicate template is already declared in a previous directive.'-[], nl,
			'There can be only one meta-predicate directive per predicate.'-[], nl, nl
		].
	error(permission_error(modify, meta_non_terminal_template, _)) -->
		[	'A meta-non-terminal template is already declared in a previous directive.'-[], nl,
			'There can be only one meta-non-terminal directive per non-terminal.'-[], nl, nl
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

	error(permission_error(define, dynamic_predicate, _)) -->
		[	'Categories cannot define clauses for dynamic predicates'-[], nl,
			'as they can be imported by any number of objects.'-[], nl, nl
		].
	error(permission_error(define, clause, _)) -->
		[	'Protocols cannot contain predicate clauses or grammar rules,'-[], nl,
			'only predicate and grammar rule directives.'-[], nl, nl
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
		['A protocols cannot implement itself. Typo in the object or category identifier?'-[], nl, nl].
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
		[	'There is not declared predicate with that functor.'-[], nl,
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
	explain(deprecated_directive(_, _, _, _, _)) -->
		[	'Code that uses deprecated directives will likely break when those'-[], nl,
			'directives are removed in future Logtalk or Prolog versions.'-[], nl, nl
		].
	explain(deprecated_predicate(_, _, _, _, _)) -->
		[	'Code that uses deprecated predicates will likely break when those'-[], nl,
			'predicates are removed in future Logtalk or Prolog versions.'-[], nl, nl
		].

	% other warning messages

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

	explain(goal_is_always_true(_, _, _, _, _)) -->
		['Misspelt variable in goal? Wrong operator or built-in predicate?'-[], nl, nl].

	explain(goal_is_always_false(_, _, _, _, _)) -->
		['Misspelt variable in goal? Wrong operator or built-in predicate?'-[], nl, nl].

	explain(no_matching_clause_for_goal(_, _, _, _, _)) -->
		[	'Calls to locally defined predicates without a clause with a matching head'-[], nl,
			'fail. Typo in a predicate argument? Predicate definition incomplete?'-[], nl, nl
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

	% lambda expression messages

	explain(parameter_variable_used_elsewhere(_, _, _, _, _, _)) -->
		[	'An occurence of a lambda parameter variable before the lambda expression'-[], nl,
			'is a common source of errors. An occurence of a lambda parameter variable'-[], nl,
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
			'Note that, as a consequence, the queries are only called after the file being loaded. '-[], nl, nl
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
		['Misspelt object name? Wrong file loading order? Circular references?'-[], nl, nl].

	explain(reference_to_unknown_protocol(_, _, _, _, _)) -->
		['Misspelt protocol name? Wrong file loading order?'-[], nl, nl].

	explain(reference_to_unknown_category(_, _, _, _, _)) -->
		['Misspelt category name? Wrong file loading order?'-[], nl, nl].

	explain(reference_to_unknown_module(_, _, _, _, _)) -->
		['Misspelt module name? Wrong file loading order? Circular references?'-[], nl, nl].

	explain(missing_predicate_directive(_, _, Type, _, (dynamic), Predicate)) -->
		[	'The ~w updates the ~q predicate but does not declare it dynamic.'-[Type, Predicate], nl,
			'Add a local ":- dynamic(~q)." directive to suppress this warning.'-[Predicate], nl, nl
		].

	explain(missing_scope_directive(_, _, _, _, Directive, _)) -->
		[	'But there is a ~w directive for the predicate. If there is a scope'-[Directive], nl,
			'directive, check for a typo in the predicate name or number of arguments.'-[], nl, nl
		].

	% naming guidelines messages

	explain(camel_case_entity_name(_, _, _, _)) -->
		[	'Coding guidelines advise the use of underscores'-[], nl,
			'to separate words in entity names.'-[], nl, nl
		].

	explain(camel_case_predicate_name(_, _, _, _, _)) -->
		[	'Coding guidelines advise the use of underscores'-[], nl,
			'to separate words in predicate names.'-[], nl, nl
		].

	explain(camel_case_non_terminal_name(_, _, _, _, _)) -->
		[	'Coding guidelines advise the use of underscores'-[], nl,
			'to separate words in non-terminal names.'-[], nl, nl
		].

	explain(non_camel_case_variable_name(_, _, _, _, _)) -->
		[	'Coding guidelines advise the use of CamelCase'-[], nl,
			'in variable names with multiple words.'-[], nl, nl
		].

	explain(non_camel_case_variable_name(_, _, _)) -->
		[	'Coding guidelines advise the use of CamelCase'-[], nl,
			'in variable names with multiple words.'-[], nl, nl
		].

	explain(variable_names_differ_only_on_case(_, _, _, _, _, _)) -->
		[	'Variable names differing only on case hurt code readability. Consider'-[], nl,
			'renaming one or both variables to better clarify their meaning.'-[], nl, nl
		].

	explain(variable_names_differ_only_on_case(_, _, _, _)) -->
		['Variables differing only on case hurt code readability.'-[], nl, nl].

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
	explain(suspicious_call(_, _, _, _, _ =.. _, _)) -->
		[	'The standard Prolog =../2 built-in predicate is costly and should be'-[], nl,
			'avoided whenever possible. Consider using the suggested alternative.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, atom_concat(_, _, _), _)) -->
		[	'Avoid implicit creation of atoms that will not be used as atom management'-[], nl,
			'is relatively costly. Consider using the suggested alternative.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, Object::_, _)) -->
		{Object == user},
		[	'Prolog standard built-in predicates can be called directly without'-[], nl,
			'requiring sending a message to the "user" pseudo-object.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _::Pred, [Pred])) -->
		[	'Only use message sending to call a local predicate when is necessary to'-[], nl,
			'generate an event for the message. Otherwise, simply call the predicate'-[], nl,
			'directly.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _::Pred, [::Pred])) -->
		[	'Only use an explicit message sending instead of a message to "self" when'-[], nl,
			'you need to generate an event for the message. Otherwise, simply send'-[], nl,
			'a message to "self".'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, ::Pred, [Pred, ^^Pred])) -->
		[	'Sending a message to self to call the same predicate being defined is'-[], nl,
			'a redundant and costly alternative to simply call the local definition.'-[], nl,
			'Or is your intention to make a "super" call?'-[], nl, nl
		].

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
		[	'A variable occurring in both arguments of a is/2 predicate call will most'-[], nl,
			'likely result in a runtime failure. Logical variables can only be further'-[], nl,
			'instantiated and not unified with a different term when already bound.'-[], nl, nl
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

	% encoding/1 directive messages

	explain(ignored_encoding_directive(_, _)) -->
		[	'The encoding/1 directive must be the first term, in the first line, in a'-[], nl,
			'file. Moreover, only the first found encoding/1 directive is processed.'-[], nl, nl
		].

	explain(misplaced_encoding_directive(_, _)) -->
		['The encoding/1 directive must be the first term, in the first line, in a file.'-[], nl, nl].

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

:- end_object.
