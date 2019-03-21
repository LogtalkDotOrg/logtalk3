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


:- object(type).

	:- info([
		version is 1.22,
		author is 'Paulo Moura',
		date is 2019/03/20,
		comment is 'Type checking predicates. User extensible. New types can be defined by adding clauses for the type/1 and check/2 multifile predicates.',
		remarks is [
			'Logtalk specific types' - '{entity, object, protocol, category, entity_identifier, object_identifier, protocol_identifier, category_identifier, event, predicate}',
			'Prolog module related types (when the backend compiler supports modules)' - '{module, module_identifier, qualified_callable}',
			'Base types from Prolog' - '{term, var, nonvar, atomic, atom, number, integer, float, compound, callable, ground}',
			'Atom derived types' - '{atom(CharSet), non_empty_atom, non_empty_atom(CharSet), boolean, character, character(CharSet), char (same as character), char(CharSet) (same as character(CharSet)), operator_specifier}',
			'Number derived types' - '{positive_number, negative_number, non_positive_number, non_negative_number}',
			'Float derived types' - '{positive_float, negative_float, non_positive_float, non_negative_float, probability}',
			'Integer derived types' - '{positive_integer, negative_integer, non_positive_integer, non_negative_integer, byte, character_code, character_code(CharSet), code (same as character_code), code(CharSet) (same as character_code(CharSet)), operator_priority}',
			'List types (compound derived types)' - '{list, non_empty_list, partial_list, list_or_partial_list, list(Type), list(Type, Min, Max), non_empty_list(Type), difference_list, difference_list(Type), codes (same as list(character_code)), chars (same as list(character))}',
			'Other compound derived types' - '{predicate_indicator, non_terminal_indicator, predicate_or_non_terminal_indicator, clause, clause_or_partial_clause, grammar_rule, pair, pair(KeyType,ValueType), cyclic, acyclic}',
			'Stream types' - '{stream, stream_or_alias, stream(Property), stream_or_alias(Property)}',
			'Other types' - '{between(Type,Lower,Upper), property(Type, LambdaExpression), one_of(Type, Set), var_or(Type), ground(Type), types(Types), type}',
			'predicate type notes' - 'This type is used to check for an object public predicate specified as Object::Functor/Arity.',
			'boolean type notes' - 'The two value of this type are the atoms true and false.',
			'Stream types notes' - 'In the case of the stream(Property) and stream_or_alias(Property) types, Property must be a valid stream property.',
			'order type notes' - 'The three possible values of this type are the single character atoms <, =, and >.',
			'character_code type notes' - 'This type takes into account Unicode support by the backend compiler. When Unicode is supported, it distinguishes between BMP and full support. When Unicode is not supported, it assumes a byte representation for characters.',
			'between(Type, Lower, Upper) type notes' - 'The type argument allows distinguishing between numbers and other types. It also allows choosing between mixed integer/float comparisons and strict float or integer comparisons. The term is type-checked before testing for interval membership.',
			'property(Type, Lambda) type notes' - 'Verifies that Term satisfies a property described using a lambda expression of the form [Parameter]>>Goal. The lambda expression is applied in the context of "user". The term is type-checked before calling the goal.',
			'one_of(Type, Set) type notes' - 'For checking if a given term is an element of a set. The set is represented using a list. The term is type-checked before testing for set membership.',
			'var_or(Type) notes' - 'Allows checking if a term is either a variable or a valid value of the given type.',
			'ground(Type) notes' - 'Allows checking if a term is ground and a valid value of the given type.',
			'types(Types) notes' - 'Allows checking if a term is a valid value for one of the types in a list of types.',
			'type notes' - 'Allows checking if a term is a valid type.',
			'qualified_callable notes' - 'Allows checking if a term is a possibly module-qualified callable term. When the term is qualified, it also checks that the qualification modules are type correct. When the term is not qualified, its semantics are the same as the callable type.',
			'Design choices' - 'The main predicates are valid/2 and check/3. These are defined using the predicate check/2. Defining clauses for check/2 instead of valid/2 gives the user full control of exception terms without requiring an additional predicate.',
			'Error context' - 'The built-in execution-context method context/1 can be used to provide the calling context for errors when using the predicate check/3.',
			'Registering new types' - 'New types can be registered by defining clauses for the type/1 and check/2 multifile predicates. Clauses for both predicates must have a bound first argument to avoid introducing spurious choice-points when type-checking terms.',
			'Meta-types' - 'Meta-types are types that have one or more sub-types. E.g. var_or(Type). The sub-types of a meta-type can be enumerated by defining a clause for the meta_type/3 multifile predicate.',
			'Character sets' - 'When testing character or character codes, or terms that contain them (e.g. atom), it is possible to choose a character set (ascii_printable, ascii_full, byte, unicode_bmp, or unicode_full) using the parameterizable types.',
			'Caveats' - 'The type argument to the predicates is never itself type-checked for performance reasons.'
		],
		see_also is [arbitrary, os_types]
	]).

	:- set_logtalk_flag(complements, restrict).

	:- public(type/1).
	:- multifile(type/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog and XSB
	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect==xsb; Dialect==qp))).
		:- dynamic(type/1).
	:- endif.
	:- mode(type(?callable), zero_or_more).
	:- info(type/1, [
		comment is 'Table of defined types. A new type can be registered by defining a clause for this predicate and adding a clause for the check/2 multifile predicate.',
		argnames is ['Type']
	]).

	:- public(meta_type/3).
	:- multifile(meta_type/3).
	% workaround the lack of support for static multifile predicates in Qu-Prolog and XSB
	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect==xsb; Dialect==qp))).
		:- dynamic(meta_type/3).
	:- endif.
	:- mode(meta_type(?callable, -list, -list), zero_or_more).
	:- info(meta_type/3, [
		comment is 'Table of defined meta-types. A registered type that is a meta-type can be described by defining a clause for this predicate to enumerate its sub-types and optional values in case of a single sub-type.',
		argnames is ['MetaType', 'SubTypes', 'Values']
	]).

	:- public(valid/2).
	:- mode(valid(@callable, @term), zero_or_one).
	:- info(valid/2, [
		comment is 'True if the given term is of the specified type. Fails otherwise.',
		argnames is ['Type', 'Term']
	]).

	:- public(check/3).
	:- mode(check(@callable, @term, @term), one_or_error).
	:- info(check/3, [
		comment is 'True if the given term is of the specified type. Throws an error otherwise using the format error(Error, Context). For the possible values of Error see the check/2 predicate.',
		argnames is ['Type', 'Term', 'Context']
	]).

	:- public(check/2).
	:- multifile(check/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog and XSB
	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect==xsb; Dialect==qp))).
		:- dynamic(check/2).
	:- endif.
	:- mode(check(@callable, @term), one_or_error).
	:- info(check/2, [
		comment is 'True if the given term is of the specified type. Throws an error otherwise. A new type can be added by defining a clause for this predicate and registering it by adding a clause for the type/1 multifile predicate.',
		argnames is ['Type', 'Term'],
		exceptions is [
			'Term is not bound as required' - 'instantiation_error',
			'Term is bound but not of the specified type' - 'type_error(Type, Term)',
			'Term is the of the correct type but not in the specified domain' - 'domain_error(Domain, Term)',
			'Term is the of the correct type and domain but the resource it represents does not exist' - 'existence_error(Type, Term)'
		]
	]).

	% Logtalk entity types
	type(entity).
	type(object).
	type(protocol).
	type(category).
	:- if(current_logtalk_flag(modules, supported)).
		type(module).
	:- endif.
	% Logtalk entity identifiers
	type(entity_identifier).
	type(object_identifier).
	type(protocol_identifier).
	type(category_identifier).
	:- if(current_logtalk_flag(modules, supported)).
		type(module_identifier).
	:- endif.
	% Logtalk object public predicate
	type(predicate).
	% Logtalk events
	type(event).
	% base types from the Prolog standard
	type(term).
	type(var).
	type(nonvar).
	type(atomic).
	type(atom).
	type(number).
	type(integer).
	type(float).
	type(compound).
	type(callable).
	type(ground).
	% other type predicates
	:- if(current_logtalk_flag(modules, supported)).
		type(qualified_callable).
	:- endif.
	% number derived types
	type(positive_number).
	type(negative_number).
	type(non_positive_number).
	type(non_negative_number).
	% float derived types
	type(positive_float).
	type(negative_float).
	type(non_positive_float).
	type(non_negative_float).
	type(probability).
	% integer derived types
	type(positive_integer).
	type(negative_integer).
	type(non_positive_integer).
	type(non_negative_integer).
	type(byte).
	type(character_code).
	type(character_code(_Charset)).
	type(code).
	type(operator_priority).
	% atom derived types
	type(atom(_Charset)).
	type(non_empty_atom).
	type(non_empty_atom(_Charset)).
	type(boolean).
	type(character).
	type(character(_Charset)).
	type(char).
	type(char(_Charset)).
	type(order).
	type(operator_specifier).
	% compound derived types
	type(predicate_indicator).
	type(non_terminal_indicator).
	type(predicate_or_non_terminal_indicator).
	type(clause).
	type(clause_or_partial_clause).
	type(grammar_rule).
	type(list).
	type(non_empty_list).
	type(partial_list).
	type(list_or_partial_list).
	type(list(_Type)).
	type(list(_Type, _Min, _Max)).
	type(non_empty_list(_Type)).
	type(difference_list).
	type(difference_list(_Type)).
	type(codes).
	type(codes(_Charset)).
	type(chars).
	type(chars(_Charset)).
	type(pair).
	type(pair(_KeyType, _ValueType)).
	type(cyclic).
	type(acyclic).
	type(between(_Type, _Lower, _Upper)).
	type(property(_Type, _LambdaExpression)).
	% stream types
	type(stream).
	type(stream(_Type)).
	% other types
	type(one_of(_Type, _Set)).
	type(var_or(_Type)).
	type(ground(_Type)).
	type(types(_Types)).
	type(type).

	meta_type(list(Type), [Type], []).
	meta_type(list(Type, Min, Max), [Type], [Min, Max]).
	meta_type(non_empty_list(Type), [Type], []).
	meta_type(difference_list(Type), [Type], []).
	meta_type(pair(KeyType, ValueType), [KeyType, ValueType], []).
	meta_type(between(Type, Lower, Upper), [Type], [Lower, Upper]).
	meta_type(property(Type, _), [Type], []).
	meta_type(one_of(Type, Values), [Type], Values).
	meta_type(var_or(Type), [Type], []).
	meta_type(ground(Type), [Type], []).
	meta_type(types(Types), Types, []).

	valid(Type, Term) :-
		catch(check(Type, Term), _, fail).

	check(Type, Term, Context) :-
		catch(check(Type, Term), Error, throw(error(Error, Context))).

	% entities

	:- if(current_logtalk_flag(modules, supported)).

	check(entity, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	current_object(Term) ->
			true
		;	current_protocol(Term) ->
			true
		;	current_category(Term) ->
			true
		;	{current_module(Term)} ->
			true
		;	callable(Term) ->
			throw(existence_error(entity, Term))
		;	throw(type_error(entity_identifier, Term))
		).

	:- else.

	check(entity, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	current_object(Term) ->
			true
		;	current_protocol(Term) ->
			true
		;	current_category(Term) ->
			true
		;	callable(Term) ->
			throw(existence_error(entity, Term))
		;	throw(type_error(entity_identifier, Term))
		).

	:- endif.

	check(object, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	current_object(Term) ->
			true
		;	callable(Term) ->
			throw(existence_error(object, Term))
		;	throw(type_error(object_identifier, Term))
		).

	check(protocol, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	current_protocol(Term) ->
			true
		;	atom(Term) ->
			throw(existence_error(protocol, Term))
		;	throw(type_error(protocol_identifier, Term))
		).

	check(category, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	current_category(Term) ->
			true
		;	callable(Term) ->
			throw(existence_error(category, Term))
		;	throw(type_error(category_identifier, Term))
		).

	:- if(current_logtalk_flag(modules, supported)).

	check(module, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	{current_module(Term)} ->
			true
		;	atom(Term) ->
			throw(existence_error(module, Term))
		;	throw(type_error(module_identifier, Term))
		).

	:- endif.

	% entity identifiers

	check(entity_identifier, Term) :-
		(	callable(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(entity_identifier, Term))
		).

	check(object_identifier, Term) :-
		(	callable(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(object_identifier, Term))
		).

	check(protocol_identifier, Term) :-
		(	atom(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(protocol_identifier, Term))
		).

	check(category_identifier, Term) :-
		(	callable(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(category_identifier, Term))
		).

	:- if(current_logtalk_flag(modules, supported)).

	check(module_identifier, Term) :-
		(	atom(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(module_identifier, Term))
		).

	:- endif.

	% object public predicate

	check(predicate, Term) :-
		(	Term = Object::Predicate ->
			(	check(object_identifier, Object),
				check(predicate_indicator, Predicate),
				Object::current_predicate(Predicate) ->
				true
			;	throw(existence_error(predicate, Object::Predicate))
			)
		;	throw(type_error(predicate, Term))
		).

	% events

	check(event, Term) :-
		(	Term == before ->
			true
		;	Term == after ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(event, Term))
		).

	% Prolog base types

	check(term, _).

	check(var, Term) :-
		(	var(Term) ->
			true
		;	throw(type_error(var, Term))
		).

	check(nonvar, Term) :-
		(	nonvar(Term) ->
			true
		;	throw(instantiation_error)
		).

	check(atomic, Term) :-
		(	atomic(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(atomic, Term))
		).

	check(atom, Term) :-
		(	atom(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(atom, Term))
		).

	check(number, Term) :-
		(	number(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(number, Term))
		).

	check(integer, Term) :-
		(	integer(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(integer, Term))
		).

	check(float, Term) :-
		(	float(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(float, Term))
		).

	check(compound, Term) :-
		(	compound(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(compound, Term))
		).

	check(callable, Term) :-
		(	callable(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(callable, Term))
		).

	check(ground, Term) :-
		(	ground(Term) ->
			true
		;	throw(instantiation_error)
		).

	% other type predicates

	:- if(current_logtalk_flag(modules, supported)).

	check(qualified_callable, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term = ':'(Module, Goal) ->
			check(module_identifier, Module),
			check(qualified_callable, Goal)
		;	callable(Term) ->
			true
		;	throw(type_error(callable, Term))
		).

	:- endif.

	% atom derived types

	check(atom(CharSet), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	atom(Term),
			atom_codes(Term, Codes),
			catch(check(list(character_code(CharSet)), Codes), _, fail) ->
			true
		;	throw(type_error(atom(CharSet), Term))
		).

	check(non_empty_atom, Term) :-
		(	Term == '' ->
			throw(type_error(non_empty_atom, Term))
		;	atom(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(non_empty_atom, Term))
		).

	check(non_empty_atom(CharSet), Term) :-
		(	Term == '' ->
			throw(type_error(non_empty_atom(CharSet), Term))
		;	atom(Term),
			atom_codes(Term, Codes),
			catch(check(list(character_code(CharSet)), Codes), _, fail) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(non_empty_atom(CharSet), Term))
		).

	check(boolean, Term) :-
		(	Term == true ->
			true
		;	Term == false ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	atom(Term) ->
			throw(domain_error(boolean, Term))
		;	throw(type_error(atom, Term))
		).

	check(character, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	atom_length(Term, 1) ->
			true
		;	throw(domain_error(character, Term))
		).

	check(character(CharSet), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	\+ atom_length(Term, 1) ->
			throw(domain_error(character(CharSet), Term))
		;	char_code(Term, Code),
			valid_character_code(CharSet, Code) ->
			true
		;	throw(domain_error(character(CharSet), Term))
		).

	check(char, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	atom_length(Term, 1) ->
			true
		;	throw(domain_error(char, Term))
		).

	check(char(CharSet), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	\+ atom_length(Term, 1) ->
			throw(domain_error(char(CharSet), Term))
		;	char_code(Term, Code),
			valid_character_code(CharSet, Code) ->
			true
		;	throw(domain_error(char(CharSet), Term))
		).

	check(order, Term) :-
		(	Term == (<) ->
			true
		;	Term == (=) ->
			true
		;	Term == (>) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	atom(Term) ->
			throw(domain_error(order, Term))
		;	throw(type_error(atom, Term))
		).

	check(operator_specifier, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	member(Term, [fx, fy, xfx, xfy, yfx, xf, yf]) ->
			true
		;	atom(Term) ->
			throw(domain_error(operator_specifier, Term))
		;	throw(type_error(atom, Term))
		).

	% number derived types

	check(positive_number, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ number(Term) ->
			throw(type_error(number, Term))
		;	Term =< 0 ->
			throw(domain_error(positive_number, Term))
		;	true
		).

	check(negative_number, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ number(Term) ->
			throw(type_error(number, Term))
		;	Term >= 0 ->
			throw(domain_error(negative_number, Term))
		;	true
		).

	check(non_negative_number, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ number(Term) ->
			throw(type_error(number, Term))
		;	Term < 0 ->
			throw(domain_error(non_negative_number, Term))
		;	true
		).

	check(non_positive_number, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ number(Term) ->
			throw(type_error(number, Term))
		;	Term > 0 ->
			throw(domain_error(non_positive_number, Term))
		;	true
		).

	% float derived types

	check(positive_float, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ float(Term) ->
			throw(type_error(float, Term))
		;	Term =< 0 ->
			throw(domain_error(positive_float, Term))
		;	true
		).

	check(negative_float, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ float(Term) ->
			throw(type_error(float, Term))
		;	Term >= 0 ->
			throw(domain_error(negative_float, Term))
		;	true
		).

	check(non_negative_float, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ float(Term) ->
			throw(type_error(float, Term))
		;	Term < 0 ->
			throw(domain_error(non_negative_float, Term))
		;	true
		).

	check(non_positive_float, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ float(Term) ->
			throw(type_error(float, Term))
		;	Term > 0 ->
			throw(domain_error(non_positive_float, Term))
		;	true
		).

	check(probability, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ float(Term) ->
			throw(type_error(float, Term))
		;	0.0 =< Term, Term =< 1.0 ->
			true
		;	throw(domain_error(probability, Term))
		).

	% integer derived types

	check(positive_integer, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	Term =< 0 ->
			throw(domain_error(positive_integer, Term))
		;	true
		).

	check(negative_integer, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	Term >= 0 ->
			throw(domain_error(negative_integer, Term))
		;	true
		).

	check(non_negative_integer, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	Term < 0 ->
			throw(domain_error(non_negative_integer, Term))
		;	true
		).

	check(non_positive_integer, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	Term > 0 ->
			throw(domain_error(non_positive_integer, Term))
		;	true
		).

	check(byte, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	0 =< Term, Term =< 255 ->
			true
		;	throw(domain_error(byte, Term))
		).

	check(character_code, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	code_upper_limit(Upper),
			0 =< Term, Term =< Upper ->
			true
		;	throw(domain_error(character_code, Term))
		).

	check(character_code(CharSet), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	valid_character_code(CharSet, Term) ->
			true
		;	throw(domain_error(character_code(CharSet), Term))
		).

	check(code, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	code_upper_limit(Upper),
			0 =< Term, Term =< Upper ->
			true
		;	throw(domain_error(code, Term))
		).

	check(code(CharSet), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	valid_character_code(CharSet, Term) ->
			true
		;	throw(domain_error(code(CharSet), Term))
		).

	check(operator_priority, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	0 =< Term, Term =< 1200 ->
			true
		;	throw(domain_error(operator_priority, Term))
		).

	% compound derived types

	check(predicate_indicator, Term) :-
		(	Term = Functor/Arity ->
			check(atom, Functor),
			check(non_negative_integer, Arity)
		;	throw(type_error(predicate_indicator, Term))
		).

	check(non_terminal_indicator, Term) :-
		(	Term = Functor//Arity ->
			check(atom, Functor),
			check(non_negative_integer, Arity)
		;	throw(type_error(non_terminal_indicator, Term))
		).

	check(predicate_or_non_terminal_indicator, Term) :-
		(	Term = Functor/Arity ->
			check(atom, Functor),
			check(non_negative_integer, Arity)
		;	Term = Functor//Arity ->
			check(atom, Functor),
			check(non_negative_integer, Arity)
		;	throw(type_error(predicate_or_non_terminal_indicator, Term))
		).

	check(clause, Term) :-
		(	Term = (Head :- Body) ->
			check(callable, Head),
			check(callable, Body)
		;	callable(Term) ->
			true
		;	throw(type_error(clause, Term))
		).

	check(clause_or_partial_clause, Term) :-
		(	Term = (Head :- Body) ->
			check(callable, Head),
			(	var(Body) ->
				true
			;	check(callable, Body)
			)
		;	callable(Term) ->
			true
		;	throw(type_error(clause_or_partial_clause, Term))
		).

	check(grammar_rule, Term) :-
		(	Term = (Head, List --> Body) ->
			check(callable, Head),
			check(list, List),
			check(callable, Body)
		;	Term = (Head --> Body) ->
			check(callable, Head),
			check(callable, Body)
		;	throw(type_error(grammar_rule, Term))
		).

	check(list, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_list(Term) ->
			true
		;	throw(type_error(list, Term))
		).

	check(non_empty_list, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term == [] ->
			throw(type_error(non_empty_list, Term))
		;	is_list(Term) ->
			true
		;	throw(type_error(non_empty_list, Term))
		).

	check(partial_list, Term) :-
		(	var(Term) ->
			true
		;	is_partial_list(Term) ->
			true
		;	throw(type_error(partial_list, Term))
		).

	check(list_or_partial_list, Term) :-
		(	var(Term) ->
			true
		;	is_list_or_partial_list(Term) ->
			true
		;	throw(type_error(list_or_partial_list, Term))
		).

	check(list(Type), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_list(Term) ->
			is_list_of_type(Term, Type)
		;	throw(type_error(list(Type), Term))
		).

	check(list(Type, Min, Max), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ is_list(Term) ->
			throw(type_error(list(Type,Min,Max), Term))
		;	is_list_of_type(Term, Type) ->
			(	list::min(Term, MinOfTerm),
				MinOfTerm @< Min ->
				throw(type_error(list(Type,Min,Max), Term))
			;	list::max(Term, MaxOfTerm),
				MaxOfTerm @> Max ->
				throw(type_error(list(Type,Min,Max), Term))
			;	true
			)
		;	throw(type_error(list(Type,Min,Max), Term))
		).

	check(non_empty_list(Type), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term == [] ->
			throw(type_error(non_empty_list(Type), Term))
		;	is_list(Term) ->
			is_list_of_type(Term, Type)
		;	throw(type_error(non_empty_list(Type), Term))
		).

	check(difference_list, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_difference_list(Term) ->
			true
		;	throw(type_error(difference_list, Term))
		).

	check(difference_list(Type), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_difference_list(Term) ->
			is_difference_list_of_type(Term, Type)
		;	throw(type_error(difference_list(Type), Term))
		).

	check(codes, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_list(Term) ->
			is_list_of_type(Term, character_code)
		;	throw(type_error(codes, Term))
		).

	check(codes(CharSet), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_list(Term) ->
			is_list_of_type(Term, code(CharSet))
		;	throw(type_error(codes(CharSet), Term))
		).

	check(chars, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_list(Term) ->
			is_list_of_type(Term, character)
		;	throw(type_error(chars, Term))
		).

	check(chars(CharSet), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_list(Term) ->
			is_list_of_type(Term, char(CharSet))
		;	throw(type_error(chars(CharSet), Term))
		).

	check(pair, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term = _-_ ->
			true
		;	throw(type_error(pair, Term))
		).

	check(pair(KeyType, ValueType), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term = Key-Value ->
			check(KeyType, Key),
			check(ValueType, Value)
		;	throw(type_error(pair(KeyType, ValueType), Term))
		).

	check(acyclic, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	acyclic_term(Term) ->
			true
		;	throw(type_error(acyclic, Term))
		).

	check(cyclic, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	acyclic_term(Term) ->
			throw(type_error(cyclic, Term))
		;	true
		).

	check(between(Type, Lower, Upper), Term) :-
		check(Type, Term),
		(	number(Term) ->
			(	Lower =< Term, Term =< Upper ->
				true
			;	throw(domain_error(between(Type, Lower, Upper), Term))
			)
		;	% not a number; use standard term order
			Lower @=< Term, Term @=< Upper ->
			true
		;	throw(domain_error(between(Type, Lower, Upper), Term))
		).

	check(property(Type, [Parameter]>>Goal), Term) :-
		check(Type, Term),
		(	\+ \+ {Parameter = Term, call(Goal)} ->
			true
		;	throw(domain_error(property(Type, [Parameter]>>Goal), Term))
		).

	% stream types
	% (assume a compliant implementation of stream_property/2)

	check(stream, Term) :-
		catch(once(stream_property(Term, _)), error(Error,_), throw(Error)).

	check(stream(Property), Term) :-
		(	catch(stream_property(Term, Property), error(Error,_), throw(Error)) ->
			true
		;	throw(type_error(stream(Property), Term))
		).

	check(stream_or_alias, Term) :-
		(	atom(Term), stream_property(_, alias(Term)) ->
			true
		;	catch(once(stream_property(Term, _)), error(Error,_), throw(Error))
		).

	check(stream_or_alias(Property), Term) :-
		(	atom(Term), stream_property(Stream, alias(Term)),
			stream_property(Stream, Property) ->
			true
		;	catch(stream_property(Term, Property), error(Error,_), throw(Error)) ->
			true
		;	throw(type_error(stream_or_alias(Property), Term))
		).

	% other types

	check(one_of(Type, Set), Term) :-
		check(Type, Term),
		% use negation to avoid further instantiating the term
		(	\+ member(Term, Set) ->
			throw(domain_error(one_of(Type, Set), Term))
		;	true
		).

	check(var_or(Type), Term) :-
		(	var(Term) ->
			true
		;	check(Type, Term)
		).

	check(ground(Type), Term) :-
		(	ground(Term) ->
			check(Type, Term)
		;	throw(instantiation_error)
		).

	check(types(Types), Term) :-
		(	member(Type, Types),
			valid(Type, Term) ->
			true
		;	throw(domain_error(types(Types), Term))
		).

	check(type, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	meta_type(Term, [Type], Values) ->
			check(type, Type),
			check(list(Type), Values)
		;	meta_type(Term, Types, _) ->
			forall(member(Type, Types), check(type, Type))
		;	type(Term) ->
			true
		;	throw(domain_error(type, Term))
		).

	% auxiliary predicates; we could use the Logtalk standard library
	% for some of them but we prefer to avoid any object dependencies

	code_upper_limit(Upper) :-
		current_logtalk_flag(unicode, Unicode),
		code_upper_limit(Unicode, Upper).

	% 0x10FFFF
	code_upper_limit(full, 1114111).
	% 0xFFFF
	code_upper_limit(bmp, 65535).
	% 0xFF
	code_upper_limit(unsupported, 255).

	valid_character_code(ascii_full, Code) :-
		0 =< Code, Code =< 127.
	valid_character_code(ascii_printable, Code) :-
		32 =< Code, Code =< 126.
	valid_character_code(byte, Code) :-
		0 =< Code, Code =< 255.
	valid_character_code(unicode_bmp, Code) :-
		0 =< Code, Code =< 65535.
	valid_character_code(unicode_full, Code) :-
		0 =< Code, Code =< 1114111.

	is_list(Var) :-
		var(Var),
		!,
		fail.
	is_list([]).
	is_list([_| Tail]) :-
		is_list(Tail).

	is_partial_list(Var) :-
		var(Var),
		!.
	is_partial_list([_| Tail]) :-
		is_partial_list(Tail).

	is_list_or_partial_list(Var) :-
		var(Var),
		!.
	is_list_or_partial_list([]).
	is_list_or_partial_list([_| Tail]) :-
		is_list_or_partial_list(Tail).

	is_list_of_type([], _).
	is_list_of_type([Term| Terms], Type) :-
		check(Type, Term),
		is_list_of_type(Terms, Type).

	is_difference_list(Var1 - Var2) :-
		var(Var1),
		var(Var2),
		!,
		Var1 == Var2.
	is_difference_list(List-End) :-
		nonvar(List),
		List = [_| Tail],
		is_difference_list(Tail-End).

	is_difference_list_of_type(Var1 - Var2, _) :-
		var(Var1),
		var(Var2),
		!,
		Var1 == Var2.
	is_difference_list_of_type(List-End, Type) :-
		nonvar(List),
		List = [Head| Tail],
		check(Type, Head),
		is_difference_list_of_type(Tail-End, Type).

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.
