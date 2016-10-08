%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/05/21,
		comment is 'Unit tests for the "includes" example.'
	]).

	test(includes_1) :-
		{findall(Vowel, vowel(Vowel), Vowels)},
		Vowels == [a,e,i,o,u].

	test(includes_2) :-
		countries::capitals(Capitals),
		Capitals == [berlim, lisbon, madrid, paris, varsovia].

	test(includes_3) :-
		setof(Countries, countries::same_population(Countries), Solutions),
		Solutions == [[france, polony], [germany, spain], [portugal]].

	test(includes_4) :-
		create_object(capitals, [], [public(capital/1), include(includes('countries.pl'))], [(capital(Capital) :- country(_,Capital,_))]),
		{setof(Capital, capitals::capital(Capital), Capitals)},
		Capitals == [berlim, lisbon, madrid, paris, varsovia],
		abolish_object(capitals).

:- end_object.
