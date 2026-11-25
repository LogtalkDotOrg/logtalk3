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


:- protocol(wmc_problem_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-25,
		comment is 'Protocol for ordered, ground weighted model counting statement providers.'
	]).

	:- public(wmc_statement/2).
	:- mode(wmc_statement(-compound, -term), zero_or_more).
	:- info(wmc_statement/2, [
		comment is 'Enumerates, by backtracking, canonical weighted model counting statements in source order and their origins.',
		argnames is ['Statement', 'Origin'],
		remarks is [
			'Clauses' - 'A disjunction of literals (``Literal ; Literal ; ...``) where a literal is ``+Term``, ``-Term``, or a bare ground ``Term`` (equivalent to ``+Term``). The constant ``false`` is the empty clause. A model of the problem satisfies all clauses.',
			'Literal weights' - '``weight(Literal, Weight)`` sets the weight of a literal, a non-negative integer or float. The weight of a literal is 1 unless specified. The weight of a model is the product of the weights of its literals, and the weighted model count is the sum of the weights of all models.',
			'Probabilities' - '``probability(Term, P)`` is a shorthand for ``weight(+Term, P)`` together with ``weight(-Term, 1-P)`` with ``P`` between 0 and 1.',
			'Weight uniqueness' - 'The weight of a literal can be given at most once.',
			'Comments and raw source' - '``comment(Text)`` and ``raw(Text)``, as in the ``sat_problem_protocol`` protocol.',
			'Reserved functors' - 'The functors ``weight/2``, ``probability/2``, ``comment/1``, and ``raw/1`` and the constant ``false`` cannot be used as bare propositional terms; wrap them in ``+``/``-`` (e.g., ``+weight(a, b)``) if necessary. A disjunction used as an argument must be parenthesized to be portable across backends.',
			'Origin' - 'Either ``source(File, Start, End)`` or ``unknown``.'
		]
	]).

:- end_protocol.
