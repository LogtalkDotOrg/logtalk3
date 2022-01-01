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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-06-06,
		comment is 'Unit tests for the "includes" example.'
	]).

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

:- end_object.
