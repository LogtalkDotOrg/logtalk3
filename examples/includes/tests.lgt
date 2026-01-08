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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 202-10-28,
		comment is 'Unit tests for the "includes" example.'
	]).

	cover(counters).
	cover(countries).

	test(includes_01, true(Vowels == [a,e,i,o,u])) :-
		{findall(Vowel, vowel(Vowel), Vowels)}.

	test(includes_02, true(Capitals == [berlim, lisbon, madrid, paris, varsovia])) :-
		countries::capitals(Capitals).

	test(includes_03, true(Solutions == [[france, poland], [germany, spain], [portugal]])) :-
		setof(Countries, countries::same_population(Countries), Solutions).

	test(includes_04, true(Capitals == [berlim, lisbon, madrid, paris, varsovia])) :-
		create_object(capitals, [], [public(capital/1), include(includes('countries.pl'))], [(capital(Capital) :- country(_,Capital,_))]),
		{setof(Capital, capitals::capital(Capital), Capitals)},
		abolish_object(capitals).

	test(includes_05, true(Counters == [a-0, b-0, c-0])) :-
		findall(Counter-Value, counters::counter(Counter, Value), Counters).

	test(includes_06, true(Vowels == [a,e,i,o,u])) :-
		findall(Vowel, {vowel(Vowel)}, Vowels).

:- end_object.
