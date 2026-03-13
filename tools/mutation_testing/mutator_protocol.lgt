%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- protocol(mutator_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-07,
		comment is 'Mutator protocol.'
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets any mutator internal state used while expanding terms.'
	]).

	:- public(mutation/2).
	:- mode(mutation(@callable, @callable), zero_or_more).
	:- info(mutation/2, [
		comment is 'Generates by backtracking zero or more mutations for a given term.',
		argnames is ['Term', 'Mutation']
	]).

	:- public(coverage_clause_mutator/0).
	:- mode(coverage_clause_mutator, zero_or_one).
	:- info(coverage_clause_mutator/0, [
		comment is 'True when mutation occurrences map directly to predicate clause numbers and can use baseline clause coverage for skipping uncovered mutants.'
	]).

:- end_protocol.
