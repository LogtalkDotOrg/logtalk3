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


:- protocol(max_sat_problem_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-23,
		comment is 'Protocol for ordered, ground weighted partial MaxSAT statement providers.'
	]).

	:- public(wcnf_statement/2).
	:- mode(wcnf_statement(-compound, -term), zero_or_more).
	:- info(wcnf_statement/2, [
		comment is 'Enumerates, by backtracking, canonical weighted partial MaxSAT statements in source order and their origins.',
		argnames is ['Statement', 'Origin'],
		remarks is [
			'Hard clauses' - 'A disjunction of literals (``Literal ; Literal ; ...``) where a literal is ``+Term``, ``-Term``, or a bare ground ``Term`` (equivalent to ``+Term``). The constant ``false`` is the empty hard clause.',
			'Soft clauses' - '``soft(Weight, Disjunction)`` where ``Weight`` is a positive integer, or ``soft(Disjunction)`` for the weight 1. The solver minimizes the sum of the weights of the falsified soft clauses. Note that a disjunction used as an argument must be parenthesized, as in ``soft(2, (p ; -q))``, to be portable across backend Prolog compilers.',
			'Comments and raw source' - '``comment(Text)`` and ``raw(Text)``, as in the ``sat_problem_protocol`` protocol.',
			'Reserved functors' - 'The functors ``soft/1``, ``soft/2``, ``comment/1``, and ``raw/1`` and the constant ``false`` cannot be used as bare propositional terms; wrap them in ``+``/``-`` (e.g., ``+soft(a)``) if necessary.',
			'Origin' - 'Either ``source(File, Start, End)`` or ``unknown``.'
		]
	]).

:- end_protocol.
