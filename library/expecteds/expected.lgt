%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 2017-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(expected).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2019/01/23,
		comment is 'Constructors for expected term references. A reference contains either an expected term or an unexpected term. References should be regarded as opaque terms and always used with the "expected(_)" object by passing the reference as a parameter.',
		remarks is [
			'Type-checking support' - 'This object also defines a type "expected" for use with the "type" library object.'
		],
		see_also is [expected(_), type]
	]).

	:- public(of_unexpected/2).
	:- mode(of_unexpected(@term, --nonvar), one).
	:- info(of_unexpected/2, [
		comment is 'Constructs a reference from an unexpected term.',
		argnames is ['Unexpected', 'Reference']
	]).

	:- public(of_expected/2).
	:- mode(of_expected(@term, --nonvar), one).
	:- info(of_expected/2, [
		comment is 'Constructs a reference from an expected term.',
		argnames is ['Expected', 'Reference']
	]).

	:- public(from_goal/4).
	:- meta_predicate(from_goal(0, *, *, *)).
	:- mode(from_goal(+callable, --term, @term, --nonvar), one).
	:- info(from_goal/4, [
		comment is 'Constructs a reference by calling Goal that binds and holds Expected on success. Otherwise returns a reference with the unexpected goal error or failure represented by the Failure argument.',
		argnames is ['Goal', 'Expected', 'Failure', 'Reference']
	]).

	:- public(from_goal/3).
	:- meta_predicate(from_goal(0, *, *)).
	:- mode(from_goal(+callable, --term, --nonvar), one).
	:- info(from_goal/3, [
		comment is 'Constructs a reference by calling Goal that binds and uses Expected on success. Otherwise returns a reference with the unexpected goal error or failure represented by the atom "fail".',
		argnames is ['Goal', 'Expected', 'Reference']
	]).

	of_unexpected(Error, unexpected(Error)).

	of_expected(Expected, expected(Expected)).

	from_goal(Goal, Expected, Failure, Reference) :-
		(	catch(Goal, Error, true) ->
			(	var(Error) ->
				Reference = expected(Expected)
			;	Reference = unexpected(Error)
			)
		;	Reference = unexpected(Failure)
		).

	from_goal(Goal, Expected, Reference) :-
		from_goal(Goal, Expected, fail, Reference).

	:- multifile(type::type/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::type/1).
	:- endif.

	% clauses for the type::type/1 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::type(expected).

	:- multifile(type::check/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::check/2).
	:- endif.

	% clauses for the type::check/2 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::check(expected, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term = unexpected(_) ->
			true
		;	Term = expected(_) ->
			true
		;	throw(type_error(expected, Term))
		).

:- end_object.


:- object(expected(_Reference)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2019/01/24,
		comment is 'Expected term reference predicates. Requires passing a reference constructed using the "expected" object as a parameter.',
		parnames is ['Reference'],
		see_also is [expected]
	]).

	:- public(is_unexpected/0).
	:- mode(is_unexpected, zero_or_one).
	:- info(is_unexpected/0, [
		comment is 'True if the reference holds an unexpected term. Avoid whenever possible by using instead the if_unexpected/1 predicate.'
	]).

	:- public(is_expected/0).
	:- mode(is_expected, zero_or_one).
	:- info(is_expected/0, [
		comment is 'True if the reference holds an expected term. Avoid whenever possible by using instead the if_expected/1 predicate.'
	]).

	:- public(if_unexpected/1).
	:- meta_predicate(if_unexpected(1)).
	:- mode(if_unexpected(+callable), zero_or_more).
	:- info(if_unexpected/1, [
		comment is 'Applies a closure when the reference holds an unexpected term using the term as argument. Succeeds otherwise. Can be used to throw the exception hold by the reference by calling it the atom "throw".',
		argnames is ['Closure']
	]).

	:- public(if_expected/1).
	:- meta_predicate(if_expected(1)).
	:- mode(if_expected(+callable), zero_or_more).
	:- info(if_expected/1, [
		comment is 'Applies a closure when the reference holds an expected term using the term as argument. Succeeds otherwise.',
		argnames is ['Closure']
	]).

	:- public(unexpected/1).
	:- mode(unexpected(--term), one_or_error).
	:- info(unexpected/1, [
		comment is 'Returns the unexpected term hold by the reference. Throws an error otherwise.',
		argnames is ['Unexpected'],
		exceptions is ['Reference holds an expected term' - existence_error(unexpected_term,'Reference')]
	]).

	:- public(expected/1).
	:- mode(expected(--term), one_or_error).
	:- info(expected/1, [
		comment is 'Returns the expected term hold by the reference. Throws an error otherwise.',
		argnames is ['Expected'],
		exceptions is ['Reference holds an unexpected term' - existence_error(expected_term,'Reference')]
	]).

	:- public(map/2).
	:- meta_predicate(map(2, *)).
	:- mode(map(+callable, --nonvar), one).
	:- info(map/2, [
		comment is 'When the reference does not hold an unexpected term and mapping a closure with the expected term and the new expected term as additional arguments is successful, returns a reference with the new expected term. Otherwise returns the same reference.',
		argnames is ['Closure', 'NewReference']
	]).

	:- public(flat_map/2).
	:- meta_predicate(flat_map(2, *)).
	:- mode(flat_map(+callable, --nonvar), one).
	:- info(flat_map/2, [
		comment is 'When the reference does not hold an unexpected term and mapping a closure with the expected term and the new reference as additional arguments is successful, returns the new reference. Otherwise returns the same reference.',
		argnames is ['Closure', 'NewReference']
	]).

	:- public(or_else/2).
	:- mode(or_else(--term, @term), one).
	:- info(or_else/2, [
		comment is 'Returns the expected term hold by the reference if it does not hold an unexpected term or the given default term if the reference holds an unexpected term.',
		argnames is ['Expected', 'Default']
	]).

	:- public(or_else_get/2).
	:- meta_predicate(or_else_get(*, 1)).
	:- mode(or_else_get(--term, +callable), one_or_error).
	:- info(or_else_get/2, [
		comment is 'Returns the expected term hold by the reference if it does not hold an unexpected term. Otherwise applies a closure to compute the expected term. Throws an error when the reference holds an unexpected term and an expected term cannot be computed.',
		argnames is ['Expected', 'Closure'],
		exceptions is ['Reference holds an unexpected term and an expected term cannot be computed' - existence_error(expected_term,'Reference')]
	]).

	:- public(or_else_call/2).
	:- meta_predicate(or_else_call(*, 0)).
	:- mode(or_else_call(--term, +callable), zero_or_one).
	:- info(or_else_call/2, [
		comment is 'Returns the expected term if the reference does not hold an unexpected term. Calls a goal deterministically otherwise.',
		argnames is ['Expected', 'Goal']
	]).

	:- public(or_else_throw/1).
	:- mode(or_else_throw(--term), one_or_error).
	:- info(or_else_throw/1, [
		comment is 'Returns the expected term hold by the reference if present. Throws the unexpected term as an error otherwise.',
		argnames is ['Expected']
	]).

	:- public(or_else_fail/1).
	:- mode(or_else_fail(--term), zero_or_one).
	:- info(or_else_fail/1, [
		comment is 'Returns the expected term hold by the reference if it does not hold an unexpected term. Fails otherwise. Usually called to skip over references holding unexpected terms.',
		argnames is ['Expected']
	]).

	is_unexpected :-
		parameter(1, unexpected(_)).

	is_expected :-
		parameter(1, expected(_)).

	if_unexpected(Closure) :-
		parameter(1, Reference),
		(	Reference = unexpected(Unexpected) ->
			call(Closure, Unexpected)
		;	true
		).

	if_expected(Closure) :-
		parameter(1, Reference),
		(	Reference = expected(Expected) ->
			call(Closure, Expected)
		;	true
		).

	unexpected(Unexpected) :-
		parameter(1, Reference),
		(	Reference = unexpected(Unexpected) ->
			true
		;	existence_error(unexpected_term,Reference)
		).

	expected(Expected) :-
		parameter(1, Reference),
		(	Reference = expected(Expected) ->
			true
		;	existence_error(expected_term,Reference)
		).

	map(Closure, NewReference) :-
		parameter(1, Reference),
		(	Reference = expected(Term),
			catch(call(Closure, Term, NewTerm), _, fail) ->
			NewReference = expected(NewTerm)
		;	NewReference = Reference
		).

	flat_map(Closure, NewReference) :-
		parameter(1, Reference),
		(	Reference = expected(Term),
			catch(call(Closure, Term, NewReference), _, fail) ->
			true
		;	NewReference = Reference
		).

	or_else(Term, Default) :-
		parameter(1, Reference),
		(	Reference = expected(Term) ->
			true
		;	Term = Default
		).

	or_else_get(Term, Closure) :-
		parameter(1, Reference),
		(	Reference = expected(Term) ->
			true
		;	catch(call(Closure, Term), _, existence_error(expected_term,Reference)) ->
			true
		;	existence_error(expected_term, Reference)
		).

	or_else_call(Expected, Goal) :-
		parameter(1, Reference),
		(	Reference = expected(Expected) ->
			true
		;	once(Goal)	
		).

	or_else_throw(Expected) :-
		parameter(1, Reference),
		(	Reference = expected(Expected) ->
			true
		;	Reference = unexpected(Error),
			throw(Error)
		).

	or_else_fail(Expected) :-
		parameter(1, expected(Expected)).

:- end_object.
