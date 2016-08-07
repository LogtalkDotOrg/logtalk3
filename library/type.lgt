%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/08/07,
		comment is 'Type checking predicates. New types can be supported by defining clauses for the type/1 and check/2 multifile predicates.',
		remarks is [
			'Logtalk specific types' - '{entity, object, protocol, category, module, entity_identifier, object_identifier, protocol_identifier, category_identifier, module_identifier, event}',
			'Base types from Prolog' - '{term, var, nonvar, atomic, atom, number, integer, float, compound, callable, ground}',
			'Atom derived types' - '{boolean, character}',
			'Number derived types' - '{positive_integer, negative_integer, non_positive_integer, non_negative_integer, byte, character_code}',
			'Compound derived types' - '{predicate_indicator, non_terminal_indicator, predicate_or_non_terminal_indicator, clause, clause_or_partial_clause, list, list_or_partial_list, list(Type), pair, pair(KeyType,ValueType), cyclic, acyclic, between(Type,Lower,Upper)}',
			'Other types' - '{member(Type), var_or(Type)}',
			'between(Type, Lower, Upper)' - 'The type argument allows us to distinguish between numbers and other terms. It also allows unambiguous mixed integer/float (number) comparisons and strict float and integer comparisons.',
			'boolean' - 'The two value of this type are the atoms true and false.',
			'character_code' - 'This type takes into account Unicode support by the backend compiler. When Unicode is supported, it distinguishes between BMP and full support. When Unicode is not supported, it assumes a byte representation for characters.',
			'one_of(Type, Set)' - 'For checking if a given term is a member of a set of elements of the a given type. The set is represented using a list.'
		]
	]).

	:- public(type/1).
	:- multifile(type/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog and XSB
	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect==xsb; Dialect==qp))).
		:- dynamic(type/1).
	:- endif.
	:- mode(type(?nonvar), zero_or_more).
	:- info(type/1, [
		comment is 'Table of defined types. A new type can be registered by defining a clause for this predicate.',
		argnames is ['Type']
	]).

	:- public(valid/2).
	:- mode(valid(+atom, @term), zero_or_one).
	:- info(valid/2, [
		comment is 'True if the given term is of the specified term. Fails otherwise.',
		argnames is ['Type', 'Term']
	]).

	:- public(check/3).
	:- mode(check(+atom, @term, @term), one).
	:- info(check/3, [
		comment is 'True if the given term is of the specified term. Throws an error otherwise using the format error(Error, Context).',
		argnames is ['Type', 'Term', 'Context']
	]).

	:- public(check/2).
	:- multifile(check/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog and XSB
	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect==xsb; Dialect==qp))).
		:- dynamic(check/2).
	:- endif.
	:- mode(check(+atom, @term), one).
	:- info(check/2, [
		comment is 'True if the given term is of the specified term. Throws an error otherwise. Checking of a new type can be added by defining a clause for this predicate.',
		argnames is ['Type', 'Term']
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
	% number derived types
	type(positive_integer).
	type(negative_integer).
	type(non_positive_integer).
	type(non_negative_integer).
	type(byte).
	type(character_code).
	% atom derived types
	type(boolean).
	type(character).
	% compound derived types
	type(predicate_indicator).
	type(non_terminal_indicator).
	type(predicate_or_non_terminal_indicator).
	type(clause).
	type(clause_or_partial_clause).
	type(list).
	type(list_or_partial_list).
	type(list(_Type)).
	type(pair).
	type(pair(_KeyType, _ValueType)).
	type(cyclic).
	type(acyclic).
	type(between(_Type, _Lower, _Upper)).
	% other types
	type(one_of(_Type, _Set)).
	type(var_or(_Type)).

	valid(Type, Term) :-
		catch(check(Type, Term), _, fail).

	check(Type, Term, Context) :-
		catch(check(Type, Term), Error, throw(error(Error, Context))).

	% entities

	check(entity, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	current_object(Term) ->
			true
		;	current_protocol(Term) ->
			true
		;	current_category(Term) ->
			true
		;	current_module(Term) ->
			true
		;	callable(Term) ->
			throw(existence_error(entity, Term))
		;	throw(type_error(entity_identifier, Term))
		).

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
		;	current_module(Term) ->
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
		;	throw(type_error(nonvar, Term))
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

	% atom derived types

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

	% number derived types

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
		;	between(0, 255, Term) ->
			true
		;	throw(domain_error(byte, Term))
		).

	check(character_code, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ integer(Term) ->
			throw(type_error(integer, Term))
		;	code_upper_limit(Upper),
			between(0, Upper, Term) ->
			true
		;	throw(domain_error(character_code, Term))
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

	check(list, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	is_list(Term) ->
			true
		;	throw(type_error(list, Term))
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
			Lower =< Term, Term =< Upper
		;	% not a number; use standard term order
			Lower @=< Term, Term @=< Upper ->
			true
		;	throw(type_error(between(Type, Lower, Upper), Term))
		).

	check(one_of(Type, Set), Term) :-
		check(Type, Term),
		(	\+ member(Term, Set) ->
			throw(domain_error(one_of(Type, Set), Term))
		;	true
		).

	check(var_or(Type), Term) :-
		(	var(Term) ->
			true
		;	check(Type, Term)
		).

	% auxiliary predicates

	code_upper_limit(Upper) :-
		current_logtalk_flag(unicode, Unicode),
		code_upper_limit(Unicode, Upper).

	code_upper_limit(full, 1114111).
	code_upper_limit(bmp, 65535).
	code_upper_limit(unsupported, 255).

	is_list(Var) :-
		var(Var),
		!,
		fail.
	is_list([]).
	is_list([_| Tail]) :-
		is_list(Tail).	

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

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.
