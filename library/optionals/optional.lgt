%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 2017 Sergio Castro <sergioc78@gmail.com> and  
%  Paulo Moura <pmoura@logtalk.org>
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


:- object(optional).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2019/01/23,
		comment is 'Constructors for optional term references. A reference is either empty or holds a term. References should be regarded as opaque terms and always used with the "optional(_)" object by passing the reference as a parameter.',
		remarks is [
			'Type-checking support' - 'This object also defines a type "optional" for use with the "type" library object.'
		],
		see_also is [optional(_), type]
	]).

	:- public(empty/1).
	:- mode(empty(--nonvar), one).
	:- info(empty/1, [
		comment is 'Constructs an empty reference.',
		argnames is ['Reference']
	]).

	:- public(of/2).
	:- mode(of(@term, --nonvar), one).
	:- info(of/2, [
		comment is 'Constructs a reference holding a term.',
		argnames is ['Term', 'Reference']
	]).

	:- public(from_goal/3).
	:- meta_predicate(from_goal(0, *, *)).
	:- mode(from_goal(+callable, --term, --nonvar), one).
	:- info(from_goal/3, [
		comment is 'Constructs a reference by calling Goal that binds and holds Term on success. Returns an empty reference if the goal fails or throws an error.',
		argnames is ['Goal', 'Term', 'Reference']
	]).

	empty(empty).

	of(Term, optional(Term)).

	from_goal(Goal, Term, Reference) :-
		(	catch(Goal, Error, true) ->
			(	var(Error) ->
				Reference = optional(Term)
			;	Reference = empty
			)
		;	Reference = empty
		).

	:- multifile(type::type/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::type/1).
	:- endif.

	% clauses for the type::type/1 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::type(optional).

	:- multifile(type::check/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::check/2).
	:- endif.

	% clauses for the type::check/2 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::check(optional, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term == empty ->
			true
		;	Term = optional(_) ->
			true
		;	throw(type_error(optional, Term))
		).

:- end_object.


:- object(optional(_Reference)).

	:- info([
		version is 1.5,
		author is 'Paulo Moura',
		date is 2019/01/24,
		comment is 'Optional reference predicates. Requires passing a reference constructed using the "optional" object as a parameter.',
		parnames is ['Reference'],
		see_also is [optional]
	]).

	:- public(is_empty/0).
	:- mode(is_empty, zero_or_one).
	:- info(is_empty/0, [
		comment is 'True if the reference is empty. Avoid whenever possible by using instead the if_empty/1 predicate.'
	]).

	:- public(is_present/0).
	:- mode(is_present, zero_or_one).
	:- info(is_present/0, [
		comment is 'True if the reference holds a term. Avoid whenever possible by using instead the if_present/1 predicate.'
	]).

	:- public(if_empty/1).
	:- meta_predicate(if_empty(0)).
	:- mode(if_empty(+callable), zero_or_more).
	:- info(if_empty/1, [
		comment is 'Calls a goal if the reference is empty. Succeeds otherwise.',
		argnames is ['Goal']
	]).

	:- public(if_present/1).
	:- meta_predicate(if_present(1)).
	:- mode(if_present(+callable), zero_or_more).
	:- info(if_present/1, [
		comment is 'Applies a closure if the reference holds a term using the term as additional argument. Succeeds otherwise.',
		argnames is ['Closure']
	]).

	:- public(filter/2).
	:- meta_predicate(filter(1, *)).
	:- mode(filter(+callable, --nonvar), one).
	:- info(filter/2, [
		comment is 'Returns the reference when it is non-empty and the term it holds satisfies a closure. Otherwise returns an empty reference.',
		argnames is ['Closure', 'Reference']
	]).

	:- public(map/2).
	:- meta_predicate(map(2, *)).
	:- mode(map(+callable, --nonvar), one).
	:- info(map/2, [
		comment is 'When the reference is non-empty and mapping a closure with the term it holds and the new term as additional arguments is successful, returns a reference with the new term. Otherwise returns an empty reference.',
		argnames is ['Closure', 'NewReference']
	]).

	:- public(flat_map/2).
	:- meta_predicate(flat_map(2, *)).
	:- mode(flat_map(+callable, --nonvar), one).
	:- info(flat_map/2, [
		comment is 'When the reference is non-empty and mapping a closure with the optional tern and the new reference as additional arguments is successful, returns the new reference. Otherwise returns an empty reference.',
		argnames is ['Closure', 'NewReference']
	]).

	:- public(get/1).
	:- mode(get(--term), one_or_error).
	:- info(get/1, [
		comment is 'Returns the term hold by the reference if not empty. Throws an error otherwise.',
		argnames is ['Term'],
		exceptions is ['Optional is empty' - existence_error(optional_term,'Reference')]
	]).

	:- public(or_else/2).
	:- mode(or_else(--term, @term), one).
	:- info(or_else/2, [
		comment is 'Returns the term hold by the reference if not empty or the given default term if the reference is empty.',
		argnames is ['Term', 'Default']
	]).

	:- public(or_else_get/2).
	:- meta_predicate(or_else_get(*, 1)).
	:- mode(or_else_get(--term, +callable), one_or_error).
	:- info(or_else_get/2, [
		comment is 'Returns the term hold by the reference if not empty. Applies a closure to compute the term otherwise. Throws an error when the reference is empty and the term cannot be computed.',
		argnames is ['Term', 'Closure'],
		exceptions is ['Reference is empty and the term cannot be computed' - existence_error(optional_term,'Reference')]
	]).

	:- public(or_else_call/2).
	:- meta_predicate(or_else_call(*, 0)).
	:- mode(or_else_call(--term, +callable), zero_or_one).
	:- info(or_else_call/2, [
		comment is 'Returns the term hold by the reference if not empty or calls a goal deterministically if the reference is empty. Can be used e.g. to generate an exception for empty optionals.',
		argnames is ['Term', 'Goal']
	]).

	:- public(or_else_fail/1).
	:- mode(or_else_fail(--term), zero_or_one).
	:- info(or_else_fail/1, [
		comment is 'Returns the term hold by the reference if not empty. Fails otherwise. Usually called to skip over empty references.',
		argnames is ['Term']
	]).

	is_empty :-
		parameter(1, empty).

	is_present :-
		parameter(1, optional(_)).

	if_empty(Goal) :-
		parameter(1, Reference),
		(	Reference == empty ->
			call(Goal)
		;	true
		).

	if_present(Closure) :-
		parameter(1, Reference),
		(	Reference == empty ->
			true
		;	Reference = optional(Term),
			call(Closure, Term)
		).

	filter(Closure, NewReference) :-
		parameter(1, Reference),
		(	Reference = optional(Term),
			call(Closure, Term) ->
			NewReference = Reference
		;	NewReference = empty
		).

	map(Closure, NewReference) :-
		parameter(1, Reference),
		(	Reference = optional(Term),
			call(Closure, Term, NewTerm) ->
			NewReference = optional(NewTerm)
		;	NewReference = empty
		).

	flat_map(Closure, NewReference) :-
		parameter(1, Reference),
		(	Reference = optional(Term),
			call(Closure, Term, NewReference) ->
			true
		;	NewReference = empty
		).

	get(Term) :-
		parameter(1, Reference),
		(	Reference == empty ->
			existence_error(optional_term, Reference)
		;	Reference = optional(Term)
		).

	or_else(Term, Default) :-
		parameter(1, Reference),
		(	Reference == empty ->
			Term = Default
		;	Reference = optional(Term)
		).

	or_else_get(Term, Closure) :-
		parameter(1, Reference),
		(	Reference == empty ->
			(	catch(call(Closure, Term), _, existence_error(optional_term,Reference)) ->
				true
			;	existence_error(optional_term, Reference)
			)
		;	Reference = optional(Term)
		).

	or_else_call(Term, Goal) :-
		parameter(1, Reference),
		(	Reference == empty ->
			once(Goal)
		;	Reference = optional(Term)
		).

	or_else_fail(Term) :-
		parameter(1, optional(Term)).

:- end_object.
