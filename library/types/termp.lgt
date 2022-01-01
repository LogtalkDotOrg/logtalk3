%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(termp).

	:- info([
		version is 1:33:0,
		author is 'Paulo Moura',
		date is 2021-04-13,
		comment is 'Term utility predicates protocol.',
		see_also is [term]
	]).

	:- public(depth/2).
	:- mode(depth(@term, ?integer), zero_or_one).
	:- info(depth/2, [
		comment is 'True if the depth of ``Term`` is ``Depth``. The depth of atomic terms is zero; the depth of a compound term is one plus the maximum depth of its sub-terms.',
		argnames is ['Term', 'Depth']
	]).

	:- public(ground/1).
	:- mode(ground(@term), zero_or_one).
	:- info(ground/1, [
		comment is 'True if the argument is ground. Deprecated. Use the ``ground/1`` standard predicate instead.',
		argnames is ['Term']
	]).

	:- public(new/1).
	:- mode(new(-nonvar), zero_or_one).
	:- info(new/1, [
		comment is 'Creates a new term instance (if meaningful).',
		argnames is ['Term']
	]).

	:- public(occurs/2).
	:- mode(occurs(@var, @term), zero_or_one).
	:- info(occurs/2, [
		comment is 'True if the variable occurs in the term.',
		argnames is ['Variable', 'Term']
	]).

	:- public(subsumes/2).
	:- mode(subsumes(?term, @term), zero_or_one).
	:- info(subsumes/2, [
		comment is 'The first term subsumes the second term. Deprecated. Use the ``subsumes_term/2`` standard predicate instead.',
		argnames is ['General', 'Specific']
	]).

	:- public(subterm/2).
	:- mode(subterm(?term, +term), zero_or_more).
	:- info(subterm/2, [
		comment is 'The first term is a subterm of the second term.',
		argnames is ['Subterm', 'Term']
	]).

	:- public(valid/1).
	:- mode(valid(@nonvar), zero_or_one).
	:- info(valid/1, [
		comment is 'Term is valid.',
		argnames is ['Term']
	]).

	:- public(check/1).
	:- mode(check(@nonvar), one).
	:- info(check/1, [
		comment is 'Checks if a term is valid. Throws an exception if the term is not valid.',
		argnames is ['Term']
	]).

	:- public(variant/2).
	:- mode(variant(@term, @term), zero_or_one).
	:- info(variant/2, [
		comment is 'Each term is a variant of the other (i.e. they are structurally equivalent).',
		argnames is ['Term1', 'Term2']
	]).

	:- public(variables/2).
	:- mode(variables(@term, -list), one).
	:- info(variables/2, [
		comment is 'Returns a list of all term variables (ordered as found when doing a depth-first, left-to-right traversal of ``Term``).',
		argnames is ['Term', 'List']
	]).

	:- public(singletons/2).
	:- mode(singletons(@term, -list), one).
	:- info(singletons/2, [
		comment is 'Returns a list of all term singleton variables (ordered as found when doing a depth-first, left-to-right traversal of ``Term``).',
		argnames is ['Term', 'Singletons']
	]).

	:- public(numbervars/3).
	:- mode(numbervars(?term, +integer, ?integer), zero_or_one).
	:- info(numbervars/3, [
		comment is 'Grounds a term by replacing all variables with ``\'$VAR\'(N)`` terms with ``N`` starting at ``From``. The ``Next`` argument is unified with the next value for ``N`` after binding all variables.',
		argnames is ['Term', 'From', 'Next']
	]).

	:- public(numbervars/1).
	:- mode(numbervars(?term), zero_or_one).
	:- info(numbervars/1, [
		comment is 'Grounds a term by replacing all variables with ``\'$VAR\'(N)`` terms with ``N`` starting at ``0``.',
		argnames is ['Term']
	]).

	:- public(varnumbers/3).
	:- mode(varnumbers(@term, +integer, ?term), zero_or_one).
	:- info(varnumbers/3, [
		comment is 'Replaces all ``\'$VAR\'(N)`` sub-terms in a term with fresh variables for all values of ``N`` grater or equal to ``From``. Variables in ``Term`` are shared with ``Copy``.',
		argnames is ['Term', 'From', 'Copy']
	]).

	:- public(varnumbers/2).
	:- mode(varnumbers(@term, ?term), zero_or_one).
	:- info(varnumbers/2, [
		comment is 'Replaces all ``\'$VAR\'(N)`` sub-terms in a term with fresh variables for all values of ``N`` grater or equal to ``0``. Variables in ``Term`` are shared with ``Copy``.',
		argnames is ['Term', 'Copy']
	]).

:- end_protocol.
