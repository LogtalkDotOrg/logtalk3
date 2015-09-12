%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		date is 2013/03/16,
		comment is 'Unit tests for the "symbiosis" example.'
	]).

	test(symbiosis_1) :-
		symbiosis::p.

	test(symbiosis_2) :-
		symbiosis::q(L),
		L == [97, 98, 99].

	test(symbiosis_3) :-
		symbiosis::r(L),
		L == [1, 2, 3].

	test(symbiosis_4) :-
		symbiosis::s(L),
		L == [2,3,4].

	test(symbiosis_5) :-
		symbiosis::t(L),
		L == [2,3,4].

:- end_object.
