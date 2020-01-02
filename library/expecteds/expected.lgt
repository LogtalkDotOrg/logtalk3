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
		version is 1.6,
		author is 'Paulo Moura',
		date is 2020/01/02,
		comment is 'Constructors for expected terms. An expected term contains either a value or an error. Expected terms should be regarded as opaque terms and always used with the ``expected/1`` object by passing the expected term as a parameter.',
		remarks is [
			'Type-checking support' - 'This object also defines a type ``expected`` for use with the ``type`` library object.'
		],
		see_also is [expected(_), type]
	]).

	:- public(of_unexpected/2).
	:- mode(of_unexpected(@term, --nonvar), one).
	:- info(of_unexpected/2, [
		comment is 'Constructs an expected term from an error that represent that the expected value is missing.',
		argnames is ['Error', 'Expected']
	]).

	:- public(of_expected/2).
	:- mode(of_expected(@term, --nonvar), one).
	:- info(of_expected/2, [
		comment is 'Constructs an expected term from an expected value.',
		argnames is ['Value', 'Expected']
	]).

	:- public(from_goal/4).
	:- meta_predicate(from_goal(0, *, *, *)).
	:- mode(from_goal(+callable, --term, @term, --nonvar), one).
	:- info(from_goal/4, [
		comment is 'Constructs an expected term holding a value bound by calling the given goal. Otherwise returns a reference with the unexpected goal error or failure represented by the ``Error`` argument.',
		argnames is ['Goal', 'Value', 'Error', 'Expected']
	]).

	:- public(from_goal/3).
	:- meta_predicate(from_goal(0, *, *)).
	:- mode(from_goal(+callable, --term, --nonvar), one).
	:- info(from_goal/3, [
		comment is 'Constructs an expected term holding a value bound by calling the given goal. Otherwise returns an expected term with the unexpected goal error or, in case of goal failure, the atom ``fail``.',
		argnames is ['Goal', 'Value', 'Expected']
	]).

	:- public(from_goal/2).
	:- meta_predicate(from_goal(1, *)).
	:- mode(from_goal(+callable, --nonvar), one).
	:- info(from_goal/2, [
		comment is 'Constructs an expected term holding a value bound by calling the given closure. Otherwise returns an expected term holding the unexpected closure error or, in case of closure failure, the atom ``fail``.',
		argnames is ['Closure', 'Expected']
	]).

	of_unexpected(Error, unexpected(Error)).

	of_expected(Value, expected(Value)).

	from_goal(Goal, Value, Error, Expected) :-
		(	catch(Goal, Ball, true) ->
			(	var(Ball) ->
				Expected = expected(Value)
			;	Expected = unexpected(Error)
			)
		;	Expected = unexpected(Error)
		).

	from_goal(Goal, Value, Expected) :-
		from_goal(Goal, Value, fail, Expected).

	from_goal(Closure, Expected) :-
		(	catch(call(Closure, Value), Error, true) ->
			(	var(Error) ->
				Expected = expected(Value)
			;	Expected = unexpected(Error)
			)
		;	Expected = unexpected(fail)
		).

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


:- object(expected(_Expected)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2019/11/26,
		comment is 'Expected term predicates. Requires passing an expected term (constructed using the ``expected`` object predicates) as a parameter.',
		parnames is ['Expected'],
		see_also is [expected]
	]).

	:- public(is_unexpected/0).
	:- mode(is_unexpected, zero_or_one).
	:- info(is_unexpected/0, [
		comment is 'True if the expected term holds an error. See also the ``if_unexpected/1`` predicate.'
	]).

	:- public(is_expected/0).
	:- mode(is_expected, zero_or_one).
	:- info(is_expected/0, [
		comment is 'True if the expected term holds a value. See also the ``if_expected/1`` predicate.'
	]).

	:- public(if_unexpected/1).
	:- meta_predicate(if_unexpected(1)).
	:- mode(if_unexpected(+callable), zero_or_more).
	:- info(if_unexpected/1, [
		comment is 'Applies a closure when the expected term holds an error using the error as argument. Succeeds otherwise. Can be used to throw the exception hold by the expected term by calling it the atom ``throw``.',
		argnames is ['Closure']
	]).

	:- public(if_expected/1).
	:- meta_predicate(if_expected(1)).
	:- mode(if_expected(+callable), zero_or_more).
	:- info(if_expected/1, [
		comment is 'Applies a closure when the expected term holds a value using the value as argument. Succeeds otherwise.',
		argnames is ['Closure']
	]).

	:- public(unexpected/1).
	:- mode(unexpected(--term), one_or_error).
	:- info(unexpected/1, [
		comment is 'Returns the error hold by the expected term. Throws an error otherwise.',
		argnames is ['Error'],
		exceptions is ['Reference holds a value' - existence_error(unexpected_term,'Expected')]
	]).

	:- public(expected/1).
	:- mode(expected(--term), one_or_error).
	:- info(expected/1, [
		comment is 'Returns the value hold by the expected term. Throws an error otherwise.',
		argnames is ['Value'],
		exceptions is ['Reference holds an error' - existence_error(expected_term,'Expected')]
	]).

	:- public(map/2).
	:- meta_predicate(map(2, *)).
	:- mode(map(+callable, --nonvar), one).
	:- info(map/2, [
		comment is 'When the expected term does not hold an error and mapping a closure with the expected value and the new value as additional arguments is successful, returns a reference with the new value. Otherwise returns the same expected term.',
		argnames is ['Closure', 'NewExpected']
	]).

	:- public(flat_map/2).
	:- meta_predicate(flat_map(2, *)).
	:- mode(flat_map(+callable, --nonvar), one).
	:- info(flat_map/2, [
		comment is 'When the expected term does not hold an error and mapping a closure with the expected value and the new expected term as additional arguments is successful, returns the new expected term. Otherwise returns the same expected term.',
		argnames is ['Closure', 'NewExpected']
	]).

	:- public(or_else/2).
	:- mode(or_else(--term, @term), one).
	:- info(or_else/2, [
		comment is 'Returns the value hold by the expected term if it does not hold an error or the given default term if the expected term holds an error.',
		argnames is ['Value', 'Default']
	]).

	:- public(or_else_get/2).
	:- meta_predicate(or_else_get(*, 1)).
	:- mode(or_else_get(--term, +callable), one_or_error).
	:- info(or_else_get/2, [
		comment is 'Returns the value hold by the expected term if it does not hold an error. Otherwise applies a closure to compute the expected value. Throws an error when the expected term holds an error and a value cannot be computed.',
		argnames is ['Value', 'Closure'],
		exceptions is ['Reference holds an unexpected term and an expected term cannot be computed' - existence_error(expected_term,'Expected')]
	]).

	:- public(or_else_call/2).
	:- meta_predicate(or_else_call(*, 0)).
	:- mode(or_else_call(--term, +callable), zero_or_one).
	:- info(or_else_call/2, [
		comment is 'Returns the value hold by the expected term if it does not hold an error. Calls a goal deterministically otherwise.',
		argnames is ['Value', 'Goal']
	]).

	:- public(or_else_throw/1).
	:- mode(or_else_throw(--term), one_or_error).
	:- info(or_else_throw/1, [
		comment is 'Returns the value hold by the expected term if present. Throws the error hold by the expected term as an exception otherwise.',
		argnames is ['Value']
	]).

	:- public(or_else_fail/1).
	:- mode(or_else_fail(--term), zero_or_one).
	:- info(or_else_fail/1, [
		comment is 'Returns the value hold by the expected term if it does not hold an error. Fails otherwise. Usually called to skip over expected terms holding errors.',
		argnames is ['Value']
	]).

	is_unexpected :-
		parameter(1, unexpected(_)).

	is_expected :-
		parameter(1, expected(_)).

	if_unexpected(Closure) :-
		parameter(1, Expected),
		(	Expected = unexpected(Error) ->
			call(Closure, Error)
		;	true
		).

	if_expected(Closure) :-
		parameter(1, Expected),
		(	Expected = expected(Value) ->
			call(Closure, Value)
		;	true
		).

	unexpected(Error) :-
		parameter(1, Expected),
		(	Expected = unexpected(Error) ->
			true
		;	existence_error(unexpected_term, Expected)
		).

	expected(Value) :-
		parameter(1, Expected),
		(	Expected = expected(Value) ->
			true
		;	existence_error(expected_term, Expected)
		).

	map(Closure, NewExpected) :-
		parameter(1, Expected),
		(	Expected = expected(Value),
			catch(call(Closure, Value, NewValue), _, fail) ->
			NewExpected = expected(NewValue)
		;	NewExpected = Expected
		).

	flat_map(Closure, NewExpected) :-
		parameter(1, Expected),
		(	Expected = expected(Value),
			catch(call(Closure, Value, NewExpected), _, fail) ->
			true
		;	NewExpected = Expected
		).

	or_else(Value, Default) :-
		parameter(1, Expected),
		(	Expected = expected(Value) ->
			true
		;	Value = Default
		).

	or_else_get(Value, Closure) :-
		parameter(1, Expected),
		(	Expected = expected(Value) ->
			true
		;	catch(call(Closure, Value), _, existence_error(expected_term,Expected)) ->
			true
		;	existence_error(expected_term, Expected)
		).

	or_else_call(Value, Goal) :-
		parameter(1, Expected),
		(	Expected = expected(Value) ->
			true
		;	once(Goal)
		).

	or_else_throw(Value) :-
		parameter(1, Expected),
		(	Expected = expected(Value) ->
			true
		;	Expected = unexpected(Error),
			throw(Error)
		).

	or_else_fail(Value) :-
		parameter(1, expected(Value)).

:- end_object.
