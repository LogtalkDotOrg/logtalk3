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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/12/16,
		comment is 'Constructors for expected term references. An expected reference contains either a term or an unexpected term. Expected references should be regarded as opaque terms and always used with the "expected(_)" object by passing the reference as a parameter.',
		remarks is [
			'Type-checking support' - 'This object also defines a type "expected" for use with the "type" library object.'
		],
		see_also is [expected(_), type]
	]).

	:- public(of_unexpected/2).
	:- mode(of_unexpected(@term, --nonvar), one).
	:- info(of_unexpected/2, [
		comment is 'Constructs an expected reference from an unexpected term.',
		argnames is ['Unexpected', 'Reference']
	]).

	:- public(of_expected/2).
	:- mode(of_expected(@term, --nonvar), one).
	:- info(of_expected/2, [
		comment is 'Constructs an expected reference from a term.',
		argnames is ['Expected', 'Reference']
	]).

	of_unexpected(Error, unexpected(Error)).

	of_expected(Term, expected(Term)).

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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/12/27,
		comment is 'Expected term reference predicates. Requires passing an expected reference constructed using the "expected" object as a parameter.',
		parnames is ['Reference'],
		see_also is [expected]
	]).

	:- public(is_unexpected/0).
	:- mode(is_unexpected, zero_or_one).
	:- info(is_unexpected/0, [
		comment is 'True if the expected reference holds an unexpected term. Avoid whenever possible by using instead the if_unexpected/1 predicate.'
	]).

	:- public(is_expected/0).
	:- mode(is_expected, zero_or_one).
	:- info(is_expected/0, [
		comment is 'True if the expected reference holds an expected term. Avoid whenever possible by using instead the if_expected/1 predicate.'
	]).

	:- public(if_unexpected/1).
	:- meta_predicate(if_unexpected(1)).
	:- mode(if_unexpected(+callable), zero_or_more).
	:- info(if_unexpected/1, [
		comment is 'Applies a closure, with the expected reference term as additional argument, if it holds an unexpected term. Succeeds otherwise.',
		argnames is ['Closure']
	]).

	:- public(if_expected/1).
	:- meta_predicate(if_expected(1)).
	:- mode(if_expected(+callable), zero_or_more).
	:- info(if_expected/1, [
		comment is 'Applies a closure, with the expected reference term as additional argument, if it does not hold an unexpected term. Succeeds otherwise.',
		argnames is ['Closure']
	]).

	:- public(unexpected/1).
	:- mode(unexpected(--term), one).
	:- info(unexpected/1, [
		comment is 'Returns the unexpected reference term if not an expected term. Throws an error otherwise.',
		argnames is ['Unexpected'],
		exceptions is ['Expected reference holds an expected term' - existence_error(unexpected_term,'Reference')]
	]).

	:- public(expected/1).
	:- mode(expected(--term), one).
	:- info(expected/1, [
		comment is 'Returns the expected reference term if not an unexpected term. Throws an error otherwise.',
		argnames is ['Expected'],
		exceptions is ['Expected reference holds an unexpected term' - existence_error(expected_term,'Reference')]
	]).

	:- public(map/2).
	:- meta_predicate(map(2, *)).
	:- mode(map(+callable, --nonvar), one).
	:- info(map/2, [
		comment is 'When the the expected reference does not hold an unexpected term and mapping a closure with the expected reference term and the new term as additional arguments is successful, returns an expected reference with the new value. Otherwise returns the expected reference.',
		argnames is ['Closure', 'NewReference']
	]).

	:- public(flat_map/2).
	:- meta_predicate(flat_map(2, *)).
	:- mode(flat_map(+callable, --nonvar), one).
	:- info(flat_map/2, [
		comment is 'When the the expected reference does not hold an unexpected term and mapping a closure with the expected reference value and the new expected reference as additional arguments is successful, returns the new expected reference. Otherwise returns the expected reference.',
		argnames is ['Closure', 'NewReference']
	]).

	:- public(or_else/2).
	:- mode(or_else(--term, @term), one).
	:- info(or_else/2, [
		comment is 'Returns the expected reference term if it does not hold an unexpected term or the given default term if the expected reference holds an unexpected term.',
		argnames is ['Term', 'Default']
	]).

	:- public(or_else_get/2).
	:- meta_predicate(or_else_get(*, 1)).
	:- mode(or_else_get(--term, +callable), one).
	:- info(or_else_get/2, [
		comment is 'Returns the expected reference term if it does not hold an unexpected term or applies a closure to compute the term if the expected holds an unexpected term. Throws an error when the expected reference holds an unexpected term and a term cannot be computed using the given closure.',
		argnames is ['Term', 'Closure'],
		exceptions is ['Expected reference holds an unexpected term and a term cannot be computed' - existence_error(expected_term,'Reference')]
	]).

	:- public(or_else_call/2).
	:- meta_predicate(or_else_call(*, 0)).
	:- mode(or_else_call(--term, +callable), zero_or_one).
	:- info(or_else_call/2, [
		comment is 'Returns the expected reference term if it does not hold an unexpected term or calls a goal deterministically if the expected reference holds an unexpected term. Can be used e.g. to throw the exception hold by the expected reference.',
		argnames is ['Expected', 'Goal']
	]).

	:- public(or_else_fail/1).
	:- mode(or_else_fail(--term), zero_or_one).
	:- info(or_else_fail/1, [
		comment is 'Returns the expected reference term if it does not hold an unexpected term. Fails otherwise. Usually called to skip over empty expected reference holding unexpected terms.',
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
		;	context(Context),
			throw(error(existence_error(unexpected_term,Reference), Context))
		).

	expected(Expected) :-
		parameter(1, Reference),
		(	Reference = expected(Expected) ->
			true
		;	context(Context),
			throw(error(existence_error(expected_term,Reference), Context))
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
		context(Context),
		(	Reference = expected(Term) ->
			true
		;	catch(call(Closure, Term), _, throw(error(existence_error(expected_term,Reference), Context))) ->
			true
		;	throw(error(existence_error(expected_term,Reference), Context))
		).

	or_else_call(Expected, Goal) :-
		parameter(1, Reference),
		(	Reference = expected(Expected) ->
			true
		;	once(Goal)	
		).

	or_else_fail(Expected) :-
		parameter(1, expected(Expected)).

:- end_object.
