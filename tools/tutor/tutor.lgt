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
		version is 0.2,
		author is 'Paulo Moura',
		date is 2019/07/25,
		comment is 'This object adds explanations and suggestions to selected compiler warning and error messages.'
	]).

	% intercept all compiler warning and error messages

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, Kind, core, Tokens) :-
		logtalk::message_prefix_stream(Kind, core, Prefix, Stream),
		phrase(explain(Message), ExplanationTokens),
		list::append(Tokens0, [nl], Tokens),
		list::append([begin(Kind,Ctx)| Tokens0], ExplanationTokens, ExtendedTokens0),
		list::append(ExtendedTokens0, [end(Ctx)], ExtendedTokens),
		logtalk::print_message_tokens(Stream, Prefix, ExtendedTokens).

	% errors


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

	explain(declared_static_predicate_called_but_not_defined(_, _, _, _, _)) -->
		[	'Calls to declared, static, but undefined predicates fail. Predicate'-[], nl,
			'definition missing? Typo in the predicate name or number of arguments?'-[], nl,
			'Should the predicate be declared dynamic?'-[], nl, nl
		].
	explain(unknown_predicate_called_but_not_defined(_, _, _, _, _)) -->
		[	'Calls to unknown and undefined predicates generate a runtime error.'-[], nl,
			'Misspelt the predicate name? Wrong number of predicate arguments?'-[], nl, nl
		].

	explain(redefined_logtalk_built_in_predicate(_, _, _, _, _)) -->
		[	'Avoid redefining Logtalk built-in predicates as it hurts code readability.'-[], nl,
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

	explain(duplicated_directive(_, _, _, _, _)) -->
		['Easy to fix warning: simply delete or correct the duplicated directive.'-[], nl, nl].

	% portability messages

	explain(non_standard_predicate_call(_, _, _, _, _)) -->
		[	'Calls to non-standard built-in predicates may make your code non-portable when'-[], nl,
			'using other backend compilers. Are there portable alternatives that you can use?'-[], nl, nl
		].
	explain(non_standard_arithmetic_function_call(_, _, _, _, _)) -->
		[	'Calls to non-standard built-in functions may make your code non-portable when'-[], nl,
			'using other backend compilers. Are there portable alternatives that you can use?'-[], nl, nl
		].

	explain(non_standard_prolog_flag(_, _, _, _, _)) -->
		[	'Use of non-standard Prolog flags may make your code non-portable when using'-[], nl,
			'other backend compilers. Are there portable alternatives that you can use?'-[], nl, nl
		].
	explain(non_standard_prolog_flag(_, _, _)) -->
		[	'Use of non-standard Prolog flags may make your code non-portable when using'-[], nl,
			'other backend compilers. Are there portable alternatives that you can use?'-[], nl, nl
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

	explain(compiling_proprietary_prolog_directive(_, _, _, _, _)) -->
		[	'Use of non-standard Prolog directives may make your code non-portable when using'-[], nl,
			'other backend compilers. Are there portable alternatives that you can use instead?'-[], nl, nl
		].

	explain(compiling_query_as_initialization_goal(_, _, _, _, _)) -->
		[	'Arbitrary queries used as directives are converted to initialization/1 directives.'-[], nl,
			'Note that, as a consequence, the queries are only called after the file being loaded. '-[], nl, nl
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
			'Add a ":- dynamic(~q)." directive to the ~w to suppress this warning.'-[Predicate, Type], nl, nl
		].

	explain(missing_scope_directive(_, _, _, _, Directive, _)) -->
		[	'But there is a ~w directive for the predicate.'-[Directive], nl,
			'If there is a scope directive, check for a typo in the predicate name.'-[], nl, nl
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

	explain(suspicious_call(_, _, _, _, (_ -> _), _)) -->
		[	'Using the ->/2 if-then control construct without an else part is a common'-[], nl,
			'source of errors. Check if the else part is missing due to a coding error'-[], nl,
			'or use the suggested alternative.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, '*->'(_, _), _)) -->
		[	'Using the *->/2 soft cut control construct without an else part is a common'-[], nl,
			'source of errors. Check if the else part is missing due to a coding error'-[], nl,
			'or use the suggested alternative.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _ =.. _, _)) -->
		[	'The standard Prolog =../2 built-in predicate is costly and should be avoided'-[], nl,
			'whenever possible. Simply use the suggested predicate.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, Object::_, _)) -->
		{Object == user},
		[	'Prolog standard built-in predicates can be called directly without requiring'-[], nl,
			'sending a message to the "user" pseudo-object. Simply call the predicate.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _::Pred, [Pred])) -->
		[	'Only use message sending to call a local predicate when you need to generate'-[], nl,
			'an event for the message. Otherwise, simply call the predicate directly.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _::Pred, [::Pred])) -->
		[	'Only use an explicit message sending instead of a message to "self" when you need'-[], nl,
			'to generate an event for the message. Otherwise, simply send a message to "self".'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, ::Pred, [Pred, ^^Pred])) -->
		[	'Sending a message to self to call the same predicate being defined is a'-[], nl,
			'redundant and costly alternative to simply call the local definition. Or'-[], nl,
			'is your intention to make a "super" call?'-[], nl, nl
		].

	explain(suspicious_call(_, _, _, _, repeat, reason(repeat(_)))) -->
		[	'A repeat loop not ended with a cut may result in an endless loop in case'-[], nl,
		 	'of unexpected backtracking. Always use a cut immediately after the test'-[], nl,
			'goal that exits the repeat loop.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, !, reason(multifile(_)))) -->
		[	'A cut in a multifile predicate clause may prevent other clauses, notably'-[], nl,
			'those defined elsewhere, from being used when calling the predicate or'-[], nl,
			'when backtracking into a call to the predicate.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _ is _, reason(shared_variable(_)))) -->
		[	'A variable occurring in both arguments of a is/2 predicate call will most'-[], nl,
			'likely result in a runtime failure. Logical variables can only be further'-[], nl,
			'instantiated and not unified with a different term when already bound.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, forall(_, _), reason(no_shared_variables(forall)))) -->
		[	'In the most common cases, the forall/2 predicate is used for implementing'-[], nl,
			'a generate-and-test loop where the first argument generates solutions that'-[], nl,
			'are verified by the second argument. You can likely ignore this warning if'-[], nl,
			'the first argument simply implements a counter.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _, reason(no_shared_variables(_)))) -->
		[	'In the most common cases, the template argument shares variables with the'-[], nl,
			'goal argument so that we collect template bindings for the goal solutions.'-[], nl,
			'You can likely ignore this warning if you are using this meta-predicate to'-[], nl,
			'simply help count solutions.'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _, reason(existential_variables([_], _)))) -->
		[	'A existentially-qualified variable must exist in the qualified goal.'-[], nl,
			'Typo in the variable name?'-[], nl, nl
		].
	explain(suspicious_call(_, _, _, _, _, reason(existential_variables([_, _| _], _)))) -->
		[	'Existentially-qualified variables must exist in the qualified goal.'-[], nl,
			'Typos in the variable names?'-[], nl, nl
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
		[	'Variable aliasing in a clause head means that two or more head arguments'-[], nl,
			'share variables. If the predicate is called with usually output arguments'-[], nl,
			'bound, a premature cut may result in the wrong clause being used. If that''s'-[], nl,
			'the case, change the clause to perform output unifications after the cut.'-[], nl, nl
		].
	explain(possible_non_steadfast_non_terminal(_, _, _, _, _)) -->
		[	'Variable aliasing in a grammar rule head means that two or more head arguments'-[], nl,
			'share variables. If the grammar rule is called with usually output arguments'-[], nl,
			'bound, a premature cut may result in the wrong grammar rule being used. If that''s'-[], nl,
			'the case, change the grammar rule to perform output unifications after the cut.'-[], nl, nl
		].

	% catchall clause

	explain(_) -->
		[nl].

:- end_object.
