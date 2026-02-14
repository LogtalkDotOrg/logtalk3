%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(datalog_protocol).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-02-13,
		comment is 'Datalog and incremental rule engine protocol (stratified negation subset).',
		remarks is [
			'Rules' - 'Rules are represented as ``rule(Id, Head, Body)`` where ``Body`` is a list of literals using ``Term`` for positive, ``neg(Term)`` for negative, and ``agg(Op, Template, Goals, Result)`` for aggregates where ``Op`` is one of ``count``, ``sum``, ``min``, or ``max``.',
			'Facts' - 'EDB facts are represented by callable and ground terms.',
			'Limitations' - 'Current version requires aggregate dependencies to be in lower strata.'
		]
	]).

	:- public(clear/0).
	:- mode(clear, one).
	:- info(clear/0, [
		comment is 'Clears all loaded rules, base facts, derived facts, and explanation supports.'
	]).

	:- public(load_program/1).
	:- mode(load_program(+list), one).
	:- info(load_program/1, [
		comment is 'Loads a full program represented as a list of ``rule(Id,Head,Body)`` and ``fact(Fact)`` terms, replacing current engine state.',
		argnames is ['Program']
	]).

	:- public(add_rule/3).
	:- mode(add_rule(+nonvar, +callable, +list(callable)), one).
	:- info(add_rule/3, [
		comment is 'Adds or replaces a rule. Rule safety is checked.',
		argnames is ['Id', 'Head', 'Body']
	]).

	:- public(remove_rule/1).
	:- mode(remove_rule(+nonvar), one).
	:- info(remove_rule/1, [
		comment is 'Removes all rules matching a rule identifier.',
		argnames is ['Id']
	]).

	:- public(begin/0).
	:- mode(begin, one).
	:- info(begin/0, [
		comment is 'Starts a transaction by saving the current engine state snapshot.'
	]).

	:- public(commit/0).
	:- mode(commit, one).
	:- info(commit/0, [
		comment is 'Commits a transaction by discarding the saved state snapshot.'
	]).

	:- public(rollback/0).
	:- mode(rollback, one).
	:- info(rollback/0, [
		comment is 'Rolls back a transaction by restoring the saved state snapshot.'
	]).

	:- public(assert_fact/1).
	:- mode(assert_fact(+callable), one).
	:- info(assert_fact/1, [
		comment is 'Asserts a ground EDB fact if not already present.',
		argnames is ['Fact']
	]).

	:- public(retract_fact/1).
	:- mode(retract_fact(+callable), one).
	:- info(retract_fact/1, [
		comment is 'Retracts an EDB fact if present.',
		argnames is ['Fact']
	]).

	:- public(materialize/0).
	:- mode(materialize, one).
	:- info(materialize/0, [
		comment is 'Computes rule closure from current EDB facts and loaded rules using a fixpoint algorithm.'
	]).

	:- public(update/3).
	:- mode(update(+list(callable), +list(callable), -compound), one).
	:- info(update/3, [
		comment is 'Applies incremental EDB updates and propagates derivation additions/removals; returns the resulting truth delta.',
		argnames is ['Inserts', 'Deletes', 'Delta']
	]).

	:- public(query/1).
	:- mode(query(?callable), zero_or_more).
	:- info(query/1, [
		comment is 'Enumerates currently true facts (EDB + IDB).',
		argnames is ['Goal']
	]).

	:- public(query/2).
	:- mode(query(?callable, ?callable), zero_or_more).
	:- info(query/2, [
		comment is 'Same as ``query/1`` while returning the unified goal as the second argument.',
		argnames is ['Goal', 'Bindings']
	]).

	:- public(explain/2).
	:- mode(explain(+callable, -nonvar), zero_or_more).
	:- info(explain/2, [
		comment is 'Returns one explanation for a currently true fact.',
		argnames is ['Fact', 'Explanation']
	]).

	:- public(rules/1).
	:- mode(rules(-list), one).
	:- info(rules/1, [
		comment is 'Returns the loaded rules.',
		argnames is ['Rules']
	]).

	:- public(facts/1).
	:- mode(facts(-list(callable)), one).
	:- info(facts/1, [
		comment is 'Returns all currently true facts as a sorted list.',
		argnames is ['Facts']
	]).

	:- public(predicate_stratum/3).
	:- mode(predicate_stratum(?atom, ?integer, ?integer), zero_or_more).
	:- info(predicate_stratum/3, [
		comment is 'Enumerates predicate strata as functor, arity, and stratum number.',
		argnames is ['Functor', 'Arity', 'Stratum']
	]).

	:- public(strata/1).
	:- mode(strata(-list), one).
	:- info(strata/1, [
		comment is 'Returns all strata grouped by stratum number and contained predicates.',
		argnames is ['Strata']
	]).

:- end_protocol.
